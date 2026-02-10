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
import Data.List (transpose, foldl')

-- CORE LOGIC
type Sample = Double
type Signal = [Sample]
type Hz = Double
type Seconds = Double

mySampleRate :: Double
mySampleRate = 44100.0

samplesPerPeriod :: Hz -> Int
samplesPerPeriod hz = round (mySampleRate / hz)

samplesPerSecond :: Seconds -> Int
samplesPerSecond duration = round (duration * mySampleRate)


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
    f (Silence s t) = silence (s + t)
    f (Tone hz s t) = silence s ++ adsr env (tone w hz t)

defaultOsc :: Oscillator
defaultOsc = osc sine (ADSR 0.01 0.1 0.8 0.1)

mixSignals :: [Signal] -> Signal
mixSignals = foldr (zipWithLong (+)) []

zipWithLong :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLong f [] ys = ys
zipWithLong f xs [] = xs
zipWithLong f (x:xs) (y:ys) = f x y : zipWithLong f xs ys

-- AUDIO TYPES
data Audio = Audio
  { sampleRate :: Int
  , channels :: [[Double]]
  } deriving (Show, Eq)

data NoteDTO = NoteDTO 
  { freq :: Double
  , duration :: Double 
  , isRest :: Bool
  } deriving (Show)


-- PARSER
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
    "R" -> return $ NoteDTO 0.0 dur True
    "P" -> return $ NoteDTO 0.0 dur True
    _ -> do
        let oct = maybe 4 (\d -> read [d]) octave
        let f = noteToFreq name oct
        return $ NoteDTO f dur False

noteToFreq :: String -> Int -> Double
noteToFreq name octave = 
  let noteMap = [("C",0),("D",2),("E",4),("F",5),("G",7),("A",9),("B",11)]
      semitone = case lookup name noteMap of Just n -> n; Nothing -> 0
  in 440.0 * (2.0 ** (fromIntegral (semitone + (octave - 4) * 12 - 9) / 12.0))


-- SYNTHESIS
synthesize :: [NoteDTO] -> Audio
synthesize notes = 
    let 
        events = snd $ foldl' convert (0.0, []) notes
        convert (currentTime, acc) (NoteDTO f d isRest) =
            let ev = if isRest 
                     then Silence currentTime d
                     else Tone f currentTime d
            in (currentTime + d, acc ++ [ev])
        signals = map (playEvent defaultOsc) events
        finalSig = mixSignals signals
    in
        Audio (round mySampleRate) [finalSig]

-- WAV I/O
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
        let skipAmount = fromIntegral chunkSize - 16
        if skipAmount > 0 then skip skipAmount else return ()
        scanChunks (Just (fromIntegral numChans, fromIntegral sRate, fromIntegral bits))
      [100, 97, 116, 97] -> do -- "data"
        case fmtInfo of
          Nothing -> fail "Data before fmt"
          Just (chans, sRate, bits) -> do
            let numSamples = fromIntegral chunkSize `div` (chans * (bits `div` 8))
            rawSamples <- case bits of
                8 ->  replicateM (numSamples * chans) (normalize8 <$> getWord8)
                16 -> replicateM (numSamples * chans) (normalize16 <$> getInt16le)
                32 -> replicateM (numSamples * chans) (normalize32 <$> getWord32le)
                _ -> fail $ "Unsupported bit depth: " ++ show bits
            return $ Audio sRate (deinterleave chans rawSamples)
      _ -> skip (fromIntegral chunkSize) >> scanChunks fmtInfo

normalize8 :: Word8 -> Double
normalize8 w = (fromIntegral w - 128.0) / 128.0 

normalize16 :: Int16 -> Double
normalize16 i = fromIntegral i / 32768.0

normalize32 :: Word32 -> Double
normalize32 w = fromIntegral (fromIntegral w :: Int32) / 2147483648.0

deinterleave :: Int -> [Double] -> [[Double]]
deinterleave n xs | n <= 0 = [] | otherwise = [takeEvery n (drop i xs) | i <- [0..n-1]]
  where takeEvery step (y:ys) = y : takeEvery step (drop (step-1) ys); takeEvery _ [] = []

writeWav :: FilePath -> Audio -> IO ()
writeWav path (Audio rate chans) = BL.writeFile path (runPut $ do
  let samples = interleave chans
      numChans = length chans
      dataSize = length samples * 2
      fileSize = 36 + dataSize
  putByteString (BS.pack [82, 73, 70, 70])
  putWord32le (fromIntegral fileSize)
  putByteString (BS.pack [87, 65, 86, 69])
  putByteString (BS.pack [102, 109, 116, 32])
  putWord32le 16; putWord16le 1; putWord16le (fromIntegral numChans); putWord32le (fromIntegral rate)
  putWord32le (fromIntegral $ rate * numChans * 2); putWord16le (fromIntegral $ numChans * 2); putWord16le 16
  putByteString (BS.pack [100, 97, 116, 97])
  putWord32le (fromIntegral dataSize)
  mapM_ (putInt16le . toPCM16) samples)

interleave :: [[Double]] -> [Double]
interleave [] = []
interleave chans = concat $ transpose chans

toPCM16 :: Double -> Int16
toPCM16 x = round (max (-1.0) (min 1.0 x) * 32767.0)


-- EDITING
trimAudio :: Double -> Double -> Audio -> Audio
trimAudio start end (Audio rate chans) = 
    let startSamp = floor (start * fromIntegral rate)
        endSamp   = floor (end * fromIntegral rate)
        len       = endSamp - startSamp
    in Audio rate (map (take len . drop startSamp) chans)

concatAudio :: Audio -> Audio -> Audio
concatAudio (Audio r1 c1) (Audio r2 c2) =
    let 
        
        c2Resampled = if r1 == r2 then c2 else map (resample r2 r1) c2
        
    
        len1 = if null c1 then 0 else length (head c1)
        len2 = if null c2Resampled then 0 else length (head c2Resampled)
        maxChans = max (length c1) (length c2Resampled)
        getChan chans idx len = 
            if idx < length chans 
            then chans !! idx 
            else replicate len 0.0
        newChannels = [ getChan c1 i len1 ++ getChan c2Resampled i len2 
                      | i <- [0 .. maxChans - 1] ]
    in Audio r1 newChannels

resample :: Int -> Int -> [Double] -> [Double]
resample fromRate toRate input = go 0.0 input
  where
    step = fromIntegral fromRate / fromIntegral toRate :: Double

    go :: Double -> [Double] -> [Double]
    go _ [] = [] 
    go pos list@(y:ys)
      | pos < 1.0 = y : go (pos + step) list 
      | otherwise = go (pos - 1.0) ys        

-- EFFECTS
changeAmplitude :: Double -> Audio -> Audio
changeAmplitude f (Audio r c) = Audio r (map (map (* f)) c)

addAmplitude :: Double -> Audio -> Audio
addAmplitude val (Audio r c) = Audio r (map (map (+ val)) c)

changeSpeed :: Double -> Audio -> Audio
changeSpeed f (Audio r c) = Audio (round (fromIntegral r * f)) c

removeByAmplitude :: String -> Double -> Audio -> Audio
removeByAmplitude mode thr (Audio r c) = Audio r (map (map check) c)
  where 
    check x = case mode of 
        "lower"  -> if abs x < thr then 0 else x
        "higher" -> if abs x > thr then 0 else x
        "equal"  -> if abs (x - thr) < 0.0001 then 0 else x 
        _ -> x

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
        (Audio r2 c2) = a2 
        c2' = if r1 == r2 then c2 else map (resample r2 r1) c2
        maxChans = max (length c1) (length c2')
        mixCh ch1 ch2 = zipWithLong (+) ch1 ch2
    in Audio r1 (zipWith mixCh (pad c1 maxChans) (pad c2' maxChans))
  where pad chs n = chs ++ replicate (n - length chs) []