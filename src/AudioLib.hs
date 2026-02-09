{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module AudioLib
  ( -- * Core Types (from Part 1 / Book)
    Signal, Sample, Hz, Seconds
  , Music(..), Event(..), ADSR(..), Oscillator(..)
  , Pitchable(..)
  , sine, saw, sqw, tri -- waveforms
  , osc, adsr, tone, silence, renderMusic
  , defaultOsc
  
    -- * Extended Types (Part 2)
  , Audio(..)
  , NoteDTO(..) -- Для парсера
  
    -- * Extended Functions
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
import Control.Monad (when, replicateM)

-- ============================================================
-- ЧАСТЬ 1: CORE (Логика синтеза из книги)
-- ============================================================

type Sample  = Double
type Signal  = [Sample]
type Hz      = Double
type Seconds = Double

mySampleRate :: Double
mySampleRate = 44100.0

samplesPerSecond :: Seconds -> Int
samplesPerSecond duration = round (duration * mySampleRate)

samplesPerPeriod :: Hz -> Int
samplesPerPeriod hz = round (mySampleRate / hz)

-- Waveforms
type Wave = Double -> Sample

sine :: Wave
sine t = sin (2 * pi * t)

sqw :: Wave
sqw t = if t <= 0.5 then -1 else 1

saw :: Wave
saw t = if t <= 0.5 then 2 * t else 2 * t - 2

tri :: Wave
tri t = if t <= 0.25 then 4 * t
        else if t <= 0.75 then 2 - 4 * t
        else 4 * t - 4

-- Basic Tone Generation
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

-- ADSR Envelope
data ADSR = ADSR
  { attack  :: Seconds
  , decay   :: Seconds
  , sustain :: Double
  , release :: Seconds
  } deriving (Show)

adsr :: ADSR -> Signal -> Signal
adsr ADSR{..} sig =
  zipWith3 (\a d s -> a * d * s) 
           (att ++ dec ++ sus) 
           rel 
           sig
 where
  attackSamples  = fromIntegral (samplesPerSecond attack)
  decaySamples   = fromIntegral (samplesPerSecond decay)
  releaseSamples = fromIntegral (samplesPerSecond release)

  att = map (/ attackSamples) [0.0 .. attackSamples]
  dec = reverse $ map (\x -> 1 - sustain + (x / decaySamples) * sustain) [0.0 .. decaySamples - 1]
  sus = repeat sustain
  rel = reverse $ take (length sig) $ 
        map (/ releaseSamples) [0.0 .. releaseSamples] ++ repeat 1.0

-- Oscillators & Events
data Event 
  = Tone    { evFreq :: Hz, evStart :: Seconds, evDur :: Seconds }
  | Silence { evStart :: Seconds, evDur :: Seconds }
  deriving Show

newtype Oscillator = Osc { playEvent :: Event -> Signal }

osc :: Wave -> ADSR -> Oscillator
osc w env = Osc f
  where
    f (Silence _ t) = silence t
    f (Tone hz _ t) = adsr env (tone w hz t)

defaultOsc :: Oscillator
defaultOsc = osc sine (ADSR 0.01 0.1 0.5 0.1)

-- Music Model (Chapter 11)
class Pitchable a where
  toFrequency :: a -> Hz


instance Pitchable Double where
  toFrequency = id

data Music 
  = forall a. Pitchable a => Note a Seconds
  | Rest Seconds
  | Seq [Music]
  | Par [Music]
  | Modify (Event -> Event) Music

-- Music Rendering Logic
playMusic :: Music -> [Event]
playMusic = run 0
  where
    run :: Seconds -> Music -> [Event]
    run t (Note p d) = [Tone (toFrequency p) t d]
    run t (Rest d)   = [Silence t d]
    run t (Seq ms)   = snd $ foldl step (t, []) ms
      where
        step (now, evs) m = 
            let newEvs = run now m
                endT   = maximum (now : map end newEvs)
            in  (endT, evs ++ newEvs)
    run t (Par ms) = concatMap (run t) ms
    run t (Modify f m) = map f (run t m)

end :: Event -> Seconds
end (Tone _ s d)  = s + d
end (Silence s d) = s + d

mixSignals :: [Signal] -> Signal
mixSignals [] = []
mixSignals sigs = foldr (zipWithLong (+)) [] sigs

renderMusic :: Oscillator -> Music -> Signal
renderMusic (Osc p) m = mixSignals $ map p (playMusic m)


-- ============================================================
-- ЧАСТЬ 2: РАСШИРЕНИЯ (I/O, Effects, Parser)
-- ============================================================

-- Extended Audio Type (Multichannel support for I/O)
data Audio = Audio
  { sampleRate :: Int
  , channels :: [[Double]]
  } deriving (Show, Eq)

data NoteDTO = NoteDTO { freq :: Double, duration :: Double } deriving (Show)

-- 1. SYNTHESIS BRIDGE
-- Использует логику Core (renderMusic) для создания Audio
synthesize :: [NoteDTO] -> Audio
synthesize notes = 
    let 
        -- Конвертируем DTO в Music
        musicSeq = Seq [ Note (freq n) (duration n) | n <- notes ]
        -- Генерируем сигнал (моно)
        signal = renderMusic defaultOsc musicSeq
    in
        Audio (round mySampleRate) [signal]

-- 2. PARSER
parseComposition :: String -> Either ParseError [NoteDTO]
parseComposition input = parse compositionParser "" input

compositionParser :: Parser [NoteDTO]
compositionParser = many noteParser

noteParser :: Parser NoteDTO
noteParser = do
  pitchStr <- many1 letter
  octave <- digit
  spaces
  durStr <- many1 (digit <|> char '.')
  newline <|> (eof >> return '\n')
  let f = noteToFreq (pitchStr ++ [octave])
  return $ NoteDTO f (read durStr)

noteToFreq :: String -> Double
noteToFreq noteStr = 
  let noteMap = [("C",0),("D",2),("E",4),("F",5),("G",7),("A",9),("B",11)]
      octave = read [last noteStr] :: Int
      noteName = init noteStr
      semitone = case lookup noteName noteMap of
                   Just n -> n
                   Nothing -> 0
  in 440.0 * (2.0 ** (fromIntegral (semitone + (octave - 4) * 12 - 9) / 12.0))

-- 3. EFFECTS
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

-- 4. WAV I/O (Manual implementation, no HCodecs dependency needed here if manual is fine for Part 2)
-- (В этом варианте я использую вашу реализацию через Binary, она надежна)

readWav :: FilePath -> IO (Either String Audio)
readWav path = do
  content <- BL.readFile path
  case runGetOrFail getWav content of
    Left (_, _, err) -> return $ Left $ "Parse error: " ++ err
    Right (_, _, audio) -> return $ Right audio

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
  if empty then fail "No data chunk" else do
    chunkId <- getByteString 4
    chunkSize <- getWord32le
    case BS.unpack chunkId of
      [102, 109, 116, 32] -> do -- "fmt "
        _ <- getWord16le; numChans <- getWord16le; sampleRate <- getWord32le
        _ <- getWord32le; _ <- getWord16le; bitsPerSample <- getWord16le
        let extra = fromIntegral chunkSize - 16
        when (extra > 0) $ skip extra
        scanChunks (Just (fromIntegral numChans, fromIntegral sampleRate, fromIntegral bitsPerSample))
      [100, 97, 116, 97] -> do -- "data"
        case fmtInfo of
          Nothing -> fail "Data before fmt"
          Just (chans, rate, bits) -> do
            let bytesPerSample = bits `div` 8
            let numSamples = fromIntegral chunkSize `div` (chans * bytesPerSample)
            samples <- if bits == 16 
                       then replicateM (numSamples * chans) (fromIntegral <$> getInt16le)
                       else fail "Only 16-bit supported"
            let normalized = map (\x -> x / 32768.0) samples
            return $ Audio rate (deinterleave chans normalized)
      _ -> skip (fromIntegral chunkSize) >> scanChunks fmtInfo

deinterleave :: Int -> [Double] -> [[Double]]
deinterleave n xs | n <= 0 = [] | otherwise = [takeEvery n (drop i xs) | i <- [0..n-1]]
  where takeEvery step (y:ys) = y : takeEvery step (drop (step-1) ys); takeEvery _ [] = []

writeWav :: FilePath -> Audio -> IO ()
writeWav path audio = BL.writeFile path (runPut (putWav audio))

putWav :: Audio -> Put
putWav (Audio rate chans) = do
  let numChannels = length chans
      samples = map toPCM16 (interleave chans)
      dataSize = length samples * 2
      fileSize = 36 + dataSize
  putByteString (BS.pack [82, 73, 70, 70]) -- RIFF
  putWord32le (fromIntegral fileSize)
  putByteString (BS.pack [87, 65, 86, 69]) -- WAVE
  putByteString (BS.pack [102, 109, 116, 32]) -- fmt 
  putWord32le 16
  putWord16le 1
  putWord16le (fromIntegral numChannels)
  putWord32le (fromIntegral rate)
  putWord32le (fromIntegral $ rate * numChannels * 2)
  putWord16le (fromIntegral $ numChannels * 2)
  putWord16le 16
  putByteString (BS.pack [100, 97, 116, 97]) -- data
  putWord32le (fromIntegral dataSize)
  mapM_ putInt16le samples

interleave :: [[Double]] -> [Double]
interleave [] = []
interleave chans | all null chans = [] | otherwise = map head (filter (not.null) chans) ++ interleave (map safeTail chans)
  where safeTail (_:xs) = xs; safeTail [] = []

toPCM16 :: Double -> Int16
toPCM16 x = round (max (-1.0) (min 1.0 x) * 32767.0)
