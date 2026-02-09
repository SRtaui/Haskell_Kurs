{-# LANGUAGE RecordWildCards #-}

module AudioLib
  ( Audio(..)
  , Note(..)
  , parseComposition
  , synthesize
  , readWav
  , writeWav
  , changeAmplitude
  , changeSpeed
  , removeByAmplitude
  , normalize
  , applyEcho
  , applyDistortion
  , mixAudio
  ) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Int (Int16)
import Data.Word (Word8, Word16, Word32)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (when)

-- ============================================================
-- ТИПЫ
-- ============================================================

data Audio = Audio
  { sampleRate :: Int
  , channels :: [[Double]]
  } deriving (Show, Eq)

data Note = Note
  { freq :: Double
  , duration :: Double
  } deriving (Show)

-- ============================================================
-- ПАРСЕР НОТНЫХ ФАЙЛОВ
-- ============================================================

parseComposition :: String -> Either ParseError [Note]
parseComposition input = parse compositionParser "" input

compositionParser :: Parser [Note]
compositionParser = many noteParser

noteParser :: Parser Note
noteParser = do
  pitchStr <- many1 letter
  octave <- digit
  spaces
  durStr <- many1 (digit <|> char '.')
  newline <|> (eof >> return '\n')
  let freq = noteToFreq (pitchStr ++ [octave])
  return $ Note freq (read durStr)

noteToFreq :: String -> Double
noteToFreq noteStr = 
  let noteMap = [("C",0),("D",2),("E",4),("F",5),("G",7),("A",9),("B",11)]
      octave = read [last noteStr] :: Int
      noteName = init noteStr
      semitone = case lookup noteName noteMap of
                   Just n -> n
                   Nothing -> 0
  in 440.0 * (2.0 ** (fromIntegral (semitone + (octave - 4) * 12 - 9) / 12.0))

-- ============================================================
-- СИНТЕЗАТОР
-- ============================================================

synthesize :: [Note] -> Audio
synthesize notes = Audio 44100 [concatMap generateNote notes]

generateNote :: Note -> [Double]
generateNote (Note f dur) =
  let rate = 44100
      totalSamples = round (dur * fromIntegral rate)
      steps = [0.0, 1.0 / fromIntegral rate ..]
  in take totalSamples $ map (\t -> sin (2 * pi * f * t)) steps

-- ============================================================
-- ЧТЕНИЕ WAV
-- ============================================================

readWav :: FilePath -> IO (Either String Audio)
readWav path = do
  content <- BL.readFile path
  case runGetOrFail getWav content of
    Left (_, _, err) -> return $ Left $ "Ошибка парсинга WAV: " ++ err
    Right (_, _, audio) -> return $ Right audio

getWav :: Get Audio
getWav = do
  riff <- getByteString 4
  if BS.unpack riff /= [82, 73, 70, 70]  -- "RIFF"
    then fail "Не найден RIFF заголовок"
    else do
      _ <- getWord32le  -- chunk size
      wave <- getByteString 4
      if BS.unpack wave /= [87, 65, 86, 69]  -- "WAVE"
        then fail "Не найден WAVE заголовок"
        else scanChunks Nothing

scanChunks :: Maybe (Int, Int, Int) -> Get Audio
scanChunks fmtInfo = do
  empty <- isEmpty
  if empty
    then fail "Конец файла, а чанк 'data' так и не найден"
    else do
      chunkId <- getByteString 4
      chunkSize <- getWord32le
      case BS.unpack chunkId of
        [102, 109, 116, 32] -> do  -- "fmt "
          _ <- getWord16le  -- audio format
          numChans <- getWord16le
          sampleRate <- getWord32le
          _ <- getWord32le  -- byte rate
          _ <- getWord16le  -- block align
          bitsPerSample <- getWord16le
          let extraBytes = fromIntegral chunkSize - 16
          when (extraBytes > 0) $ skip extraBytes
          scanChunks (Just (fromIntegral numChans, fromIntegral sampleRate, fromIntegral bitsPerSample))
        [100, 97, 116, 97] -> do  -- "data"
          case fmtInfo of
            Nothing -> fail "Чанк 'data' найден раньше, чем 'fmt'"
            Just (chans, rate, bits) -> do
              let bytesPerSample = bits `div` 8
              let numSamples = fromIntegral chunkSize `div` (chans * bytesPerSample)
              samples <- if bits == 16
                then replicateM (numSamples * chans) (fromIntegral <$> getInt16le)
                else fail "Пока поддерживаются только 16-битные WAV"
              let normalized = map (\x -> x / 32768.0) samples
              let audioData = deinterleave chans normalized
              return $ Audio rate audioData
        _ -> do
          skip (fromIntegral chunkSize)
          scanChunks fmtInfo

deinterleave :: Int -> [Double] -> [[Double]]
deinterleave n xs
  | n <= 0 = []
  | otherwise = [takeEvery n (drop i xs) | i <- [0..n-1]]
  where
    takeEvery _ [] = []
    takeEvery step (y:ys) = y : takeEvery step (drop (step-1) ys)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n action = sequence (replicate n action)

-- ============================================================
-- ЗАПИСЬ WAV
-- ============================================================

writeWav :: FilePath -> Audio -> IO ()
writeWav path audio = BL.writeFile path (runPut (putWav audio))

putWav :: Audio -> Put
putWav (Audio rate chans) = do
  let numChannels = length chans
      bitsPerSample = 16
      interleaved = interleave chans
      samples = map toPCM16 interleaved
      dataSize = length samples * 2
      fileSize = 36 + dataSize
  
  -- RIFF header
  putByteString (BS.pack [82, 73, 70, 70])  -- "RIFF"
  putWord32le (fromIntegral fileSize)
  putByteString (BS.pack [87, 65, 86, 69])  -- "WAVE"
  
  -- fmt chunk
  putByteString (BS.pack [102, 109, 116, 32])  -- "fmt "
  putWord32le 16
  putWord16le 1  -- PCM
  putWord16le (fromIntegral numChannels)
  putWord32le (fromIntegral rate)
  putWord32le (fromIntegral $ rate * numChannels * bitsPerSample `div` 8)
  putWord16le (fromIntegral $ numChannels * bitsPerSample `div` 8)
  putWord16le (fromIntegral bitsPerSample)
  
  -- data chunk
  putByteString (BS.pack [100, 97, 116, 97])  -- "data"
  putWord32le (fromIntegral dataSize)
  mapM_ putInt16le samples

interleave :: [[Double]] -> [Double]
interleave [] = []
interleave chans
  | all null chans = []
  | otherwise = map head (filter (not . null) chans) ++ interleave (map safeTail chans)
  where
    safeTail [] = []
    safeTail (_:xs) = xs

toPCM16 :: Double -> Int16
toPCM16 x = 
  let clamped = max (-1.0) (min 1.0 x)
  in round (clamped * 32767.0)

-- ============================================================
-- ЭФФЕКТЫ
-- ============================================================

zipWithLong :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLong f [] ys = ys
zipWithLong f xs [] = xs
zipWithLong f (x:xs) (y:ys) = f x y : zipWithLong f xs ys

changeAmplitude :: Double -> Audio -> Audio
changeAmplitude factor (Audio rate chans) =
  Audio rate (map (map (* factor)) chans)

changeSpeed :: Double -> Audio -> Audio
changeSpeed factor (Audio rate chans) =
  Audio (round (fromIntegral rate * factor)) chans

removeByAmplitude :: String -> Double -> Audio -> Audio
removeByAmplitude mode threshold (Audio rate chans) =
  Audio rate (map (map check) chans)
  where
    check x = case mode of
      "lower" -> if abs x < threshold then 0 else x
      "higher" -> if abs x > threshold then 0 else x
      _ -> x

normalize :: Double -> Audio -> Audio
normalize targetLevel (Audio rate chans) =
  let allSamples = concat chans
      maxPeak = if null allSamples then 0.0 else maximum (map abs allSamples)
      factor = if maxPeak == 0 then 1.0 else targetLevel / maxPeak
  in changeAmplitude factor (Audio rate chans)

applyEcho :: Double -> Double -> Audio -> Audio
applyEcho delay decay (Audio rate chans) =
  Audio rate (map (echoChannel rate delay decay) chans)

echoChannel :: Int -> Double -> Double -> [Double] -> [Double]
echoChannel rate delay decay samples =
  let delaySamples = round (delay * fromIntegral rate)
      original = map (* 0.6) samples
      echo = replicate delaySamples 0.0 ++ map (* (decay * 0.4)) samples
  in zipWithLong (+) original echo

applyDistortion :: Double -> Audio -> Audio
applyDistortion drive (Audio rate chans) =
  Audio rate (map (map grunge) chans)
  where
    grunge x = (2.0 / pi) * atan (x * drive)

mixAudio :: Audio -> Audio -> Audio
mixAudio (Audio r1 c1) (Audio r2 c2) =
  let maxChans = max (length c1) (length c2)
      c1' = c1 ++ replicate (maxChans - length c1) []
      c2' = c2 ++ replicate (maxChans - length c2) []
      mixedChans = zipWith mixChannel c1' c2'
  in Audio r1 mixedChans
  where
    mixChannel ch1 ch2 = zipWithLong (+) ch1 ch2
