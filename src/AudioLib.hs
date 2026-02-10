{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AudioLib where

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Get
import Data.Binary.Put
import Data.Int (Int16, Int32)
import Data.Word (Word8, Word16, Word32)
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (when, replicateM)
import Data.List (transpose)

-- ============================================================
-- CORE LOGIC (From Part 1)
-- ============================================================
-- (Оставляем как было, сокращенно для экономии места, но в файле должно быть полностью)
type Sample = Double
type Signal = [Sample]
type Hz = Double
type Seconds = Double

mySampleRate :: Double
mySampleRate = 44100.0
samplesPerPeriod :: Hz -> Int
samplesPerPeriod hz = round (mySampleRate / hz)
-- (в книге: samplesPerPeriod hz = round (sampleRate / hz)) [file:63]

samplesPerSecond :: Seconds -> Int
samplesPerSecond duration = round (duration * mySampleRate)

-- Waveforms & Core functions (Osc, ADSR...) - ТЕ ЖЕ САМЫЕ, ЧТО БЫЛИ РАНЬШЕ
type Wave = Double -> Sample
sine :: Wave
sine t = Prelude.sin (2 * pi * t)

sqw :: Wave
sqw t | t <= 0.5  = -1
      | otherwise = 1

saw :: Wave
saw t = 2 * t - 1

tri :: Wave
tri t | t <= 0.5  = 4 * t - 1
      | otherwise = -4 * t + 3
-- это соответствует описанию/листингу 10.2 (saw: 2*t-1, tri: 4*t-1 и -4*t+3) [file:63]


silence :: Seconds -> Signal
silence t = replicate (samplesPerSecond t) 0


tone :: Wave -> Hz -> Seconds -> Signal
tone wave freq t = map wave periodValues
  where
    numSamples = samplesPerPeriod freq
    periodValues =
      [ fromIntegral (i `mod` numSamples) / fromIntegral numSamples
      | i <- [0 .. samplesPerSecond t - 1]
      ]
-- это ровно логика Listing 10.3 (tone wave freq t = map wave periodValues, periodValues через mod) [file:63]


-- ADSR
data ADSR = ADSR { attack :: Seconds, decay :: Seconds, sustain :: Double, release :: Seconds } deriving Show
adsr :: ADSR -> Signal -> Signal
adsr ADSR{..} sig = zipWith3 (\a d s -> a * d * s) (att ++ dec ++ sus) rel sig
 where
  attSamples = fromIntegral (samplesPerSecond attack)
  decSamples = fromIntegral (samplesPerSecond decay)
  relSamples = fromIntegral (samplesPerSecond release)
  att = map (/ attSamples) [0.0 .. attSamples]
  dec = reverse $ map (\x -> 1 - sustain + (x / decSamples) * sustain) [0.0 .. decSamples - 1]
  sus = repeat sustain
  rel = reverse $ take (length sig) $ map (/ relSamples) [0.0 .. relSamples] ++ repeat 1.0

-- Oscillator
data Event = Tone { evFreq :: Hz, evStart :: Seconds, evDur :: Seconds }
           | Silence { evStart :: Seconds, evDur :: Seconds } deriving Show
newtype Oscillator = Osc { playEvent :: Event -> Signal }

osc :: Wave -> ADSR -> Oscillator
osc w env = Osc f
  where
    f (Silence s t) = silence (s + t) -- Корректный сдвиг тишиной
    f (Tone hz s t) = silence s ++ adsr env (tone w hz t)

defaultOsc :: Oscillator
defaultOsc = osc sine (ADSR 0.01 0.1 0.8 0.1) -- Немного подкрутил сустейн

mixSignals :: [Signal] -> Signal
mixSignals = foldr (zipWithLong (+)) []

zipWithLong :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLong f [] ys = ys
zipWithLong f xs [] = xs
zipWithLong f (x:xs) (y:ys) = f x y : zipWithLong f xs ys

-- ============================================================
-- AUDIO TYPES
-- ============================================================

data Audio = Audio
  { sampleRate :: Int
  , channels :: [[Double]]
  } deriving (Show, Eq)

data NoteDTO = NoteDTO 
  { freq :: Double
  , duration :: Double 
  , isRest :: Bool -- Флаг паузы
  } deriving (Show)

-- ============================================================
-- PARSER (Improved: Rests support)
-- ============================================================

parseComposition :: String -> Either ParseError [NoteDTO]
parseComposition = parse (many noteParser) ""

noteParser :: Parser NoteDTO
noteParser = do
  spaces
  name <- many1 letter
  octave <- optionMaybe digit
  spaces
  durStr <- many1 (digit <|> char '.')
  optional (string "\n")
  
  let dur = read durStr :: Double
  
  case name of
    "R" -> return $ NoteDTO 0.0 dur True -- Rest
    "P" -> return $ NoteDTO 0.0 dur True -- Pause
    _ -> do
        let oct = maybe 4 (\d -> read [d]) octave -- Default octave 4
        let f = noteToFreq name oct
        return $ NoteDTO f dur False

noteToFreq :: String -> Int -> Double
noteToFreq name octave = 
  let noteMap = [("C",0),("D",2),("E",4),("F",5),("G",7),("A",9),("B",11)]
      semitone = case lookup name noteMap of Just n -> n; Nothing -> 0
  in 440.0 * (2.0 ** (fromIntegral (semitone + (octave - 4) * 12 - 9) / 12.0))

-- ============================================================
-- SYNTHESIS
-- ============================================================

synthesize :: [NoteDTO] -> Audio
synthesize notes = 
    let 
        -- Превращаем ноты в события для осциллятора
        -- Здесь мы делаем простую последовательность: start накапливается
        events = snd $ foldl convert (0.0, []) notes
        
        convert (currentTime, acc) (NoteDTO f d isRest) =
            let ev = if isRest 
                     then Silence currentTime d
                     else Tone f currentTime d
            in (currentTime + d, acc ++ [ev])
            
        -- Рендерим все события и миксуем их
        signals = map (playEvent defaultOsc) events
        finalSig = mixSignals signals
    in
        Audio (round mySampleRate) [finalSig]

-- ============================================================
-- WAV I/O (Improved: 8/16/32 bit support)
-- ============================================================

readWav :: FilePath -> IO (Either String Audio)
readWav path = do
  content <- BL.readFile path
  return $ case runGetOrFail getWav content of
    Left (_, _, err) -> Left $ "WAV Parse Error: " ++ err
    Right (_, _, audio) -> Right audio

getWav :: Get Audio
getWav = do
  riff <- getByteString 4
  if BS.unpack riff /= [82, 73, 70, 70] then fail "No RIFF" else do
    _ <- getWord32le
    wave <- getByteString 4
    if BS.unpack wave /= [87, 65, 86, 69] then fail "No WAVE" else scanChunks Nothing

scanChunks :: Maybe (Int, Int, Int) -> Get Audio
scanChunks fmtInfo = do
  empty <- isEmpty
  if empty then fail "No data chunk found" else do
    chunkId <- getByteString 4
    chunkSize <- getWord32le
    case BS.unpack chunkId of
      [102, 109, 116, 32] -> do -- "fmt "
        _ <- getWord16le; numChans <- getWord16le; sRate <- getWord32le
        _ <- getWord32le; _ <- getWord16le; bits <- getWord16le
        skip (fromIntegral chunkSize - 16)
        scanChunks (Just (fromIntegral numChans, fromIntegral sRate, fromIntegral bits))
      [100, 97, 116, 97] -> do -- "data"
        case fmtInfo of
          Nothing -> fail "Data before fmt"
          Just (chans, sRate, bits) -> do
            let numSamples = fromIntegral chunkSize `div` (chans * (bits `div` 8))
            
            rawSamples <- case bits of
                8 ->  replicateM (numSamples * chans) (normalize8 <$> getWord8)
                16 -> replicateM (numSamples * chans) (normalize16 <$> getInt16le)
                32 -> replicateM (numSamples * chans) (normalize32 <$> getWord32le) -- IEEE float as Word32 logic often complex, here assuming PCM integer 32 for simplicity or float reinterpretation
                _ -> fail $ "Unsupported bit depth: " ++ show bits
            
            return $ Audio sRate (deinterleave chans rawSamples)
      _ -> skip (fromIntegral chunkSize) >> scanChunks fmtInfo

-- Нормализаторы для разных битностей
normalize8 :: Word8 -> Double
normalize8 w = (fromIntegral w - 128.0) / 128.0 -- 8 bit is unsigned 0..255

normalize16 :: Int16 -> Double
normalize16 i = fromIntegral i / 32768.0

normalize32 :: Word32 -> Double
normalize32 w = fromIntegral (fromIntegral w :: Int32) / 2147483648.0 -- Assuming 32-bit int PCM

deinterleave :: Int -> [Double] -> [[Double]]
deinterleave n xs | n <= 0 = [] | otherwise = [takeEvery n (drop i xs) | i <- [0..n-1]]
  where takeEvery step (y:ys) = y : takeEvery step (drop (step-1) ys); takeEvery _ [] = []

-- Пишем всегда в 16 бит для совместимости (как самое надежное)
writeWav :: FilePath -> Audio -> IO ()
writeWav path (Audio rate chans) = BL.writeFile path (runPut $ do
  let samples = interleave chans
      numChans = length chans
      dataSize = length samples * 2
      fileSize = 36 + dataSize
  putByteString (BS.pack [82, 73, 70, 70]) -- RIFF
  putWord32le (fromIntegral fileSize)
  putByteString (BS.pack [87, 65, 86, 69]) -- WAVE
  putByteString (BS.pack [102, 109, 116, 32]) -- fmt 
  putWord32le 16; putWord16le 1; putWord16le (fromIntegral numChans); putWord32le (fromIntegral rate)
  putWord32le (fromIntegral $ rate * numChans * 2); putWord16le (fromIntegral $ numChans * 2); putWord16le 16
  putByteString (BS.pack [100, 97, 116, 97]) -- data
  putWord32le (fromIntegral dataSize)
  mapM_ (putInt16le . toPCM16) samples)

interleave :: [[Double]] -> [Double]
interleave [] = []
interleave chans = concat $ transpose chans

toPCM16 :: Double -> Int16
toPCM16 x = round (max (-1.0) (min 1.0 x) * 32767.0)

-- ============================================================
-- EDITING (NEW: Cut & Concat)
-- ============================================================

trimAudio :: Double -> Double -> Audio -> Audio
trimAudio start end (Audio rate chans) = 
    let startSamp = floor (start * fromIntegral rate)
        endSamp   = floor (end * fromIntegral rate)
        len       = endSamp - startSamp
    in Audio rate (map (take len . drop startSamp) chans)

concatAudio :: Audio -> Audio -> Audio
concatAudio (Audio r1 c1) (Audio r2 c2) =
    -- Если частоты разные, нужно ресемплить (простой вариант: привести r2 к r1)
    let c2Resampled = if r1 == r2 then c2 else map (resample r2 r1) c2
        -- Выравниваем каналы
        maxChans = max (length c1) (length c2Resampled)
        pad chs = chs ++ replicate (maxChans - length chs) [] -- Пустые каналы? Лучше тишину
        c1Pad = pad c1
        c2Pad = pad c2Resampled
    in Audio r1 (zipWith (++) c1Pad c2Pad)

-- Простейший ресемплинг (drop/dup)
-- Исправленная функция ресемплинга
resample :: Int -> Int -> [Double] -> [Double]
resample fromRate toRate samples =
    let ratio = fromIntegral fromRate / fromIntegral toRate :: Double
        newLength = floor (fromIntegral (length samples) / ratio) :: Int
        indices = [floor (fromIntegral i * ratio) | i <- [0 .. newLength - 1]]
    in map (safeIndex samples) indices
  where
    safeIndex xs i | i < length xs = xs !! i | otherwise = 0.0


-- ============================================================
-- EFFECTS
-- ============================================================

changeAmplitude :: Double -> Audio -> Audio
changeAmplitude f (Audio r c) = Audio r (map (map (* f)) c)

addAmplitude :: Double -> Audio -> Audio
addAmplitude val (Audio r c) = Audio r (map (map (+ val)) c)

changeSpeed :: Double -> Audio -> Audio
changeSpeed f (Audio r c) = Audio (round (fromIntegral r * f)) c

removeByAmplitude :: String -> Double -> Audio -> Audio
removeByAmplitude mode thr (Audio r c) = Audio r (map (map check) c)
  where check x = case mode of "lower" -> if abs x < thr then 0 else x; "higher" -> if abs x > thr then 0 else x; _ -> x

normalize :: Double -> Audio -> Audio
normalize target (Audio r c) = 
    let peak = maximum (0 : map abs (concat c))
        factor = if peak == 0 then 1 else target / peak
    in changeAmplitude factor (Audio r c)

applyEcho :: Double -> Double -> Audio -> Audio
applyEcho delay decay (Audio r c) = Audio r (map (\s -> zipWithLong (+) s (echoGen r delay decay s)) c)
  where echoGen rate d dec s = replicate (round (d * fromIntegral rate)) 0 ++ map (* dec) s

applyDistortion :: Double -> Audio -> Audio
applyDistortion drive (Audio r c) = Audio r (map (map (\x -> (2/pi) * atan (x*drive))) c)

mixAudio :: Audio -> Audio -> Audio
mixAudio a1 a2 = 
    let (Audio r1 c1) = a1
        (Audio r2 c2) = a2 -- Тут по-хорошему тоже нужен ресемплинг, если r1 != r2
        -- Упростим: считаем r1 главным
        c2' = if r1 == r2 then c2 else map (resample r2 r1) c2
        maxChans = max (length c1) (length c2')
        mixCh ch1 ch2 = zipWithLong (+) ch1 ch2
    in Audio r1 (zipWith mixCh (pad c1 maxChans) (pad c2' maxChans))
  where pad chs n = chs ++ replicate (n - length chs) []
