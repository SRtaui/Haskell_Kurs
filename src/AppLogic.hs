module AppLogic where

import AudioLib
import Data.List (foldl')



data Track = Track 
    { trackName :: String
    , trackAudio :: Audio 
    } deriving (Show)



addTrack :: String -> Audio -> [Track] -> [Track]
addTrack name aud tracks = tracks ++ [Track name aud]

removeTrack :: Int -> [Track] -> [Track]
removeTrack idx tracks = 
    let (left, right) = splitAt idx tracks
    in left ++ drop 1 right

modifyTrack :: Int -> (Audio -> Audio) -> [Track] -> [Track]
modifyTrack idx f tracks 
    | idx < 0 || idx >= length tracks = tracks
    | otherwise = 
        let (left, r:right) = splitAt idx tracks
            newTrack = r { trackAudio = f (trackAudio r) }
        in left ++ (newTrack : right)


replaceTrackWithMany :: Int -> [Track] -> [Track] -> [Track]
replaceTrackWithMany idx newItems tracks
    | idx < 0 || idx >= length tracks = tracks
    | otherwise =
        let (left, _:right) = splitAt idx tracks
        in left ++ newItems ++ right

getTrackAudio :: Int -> [Track] -> Maybe Audio
getTrackAudio idx tracks
    | idx < 0 || idx >= length tracks = Nothing
    | otherwise = Just $ trackAudio (tracks !! idx)

renderConcat :: [Track] -> Audio
renderConcat [] = Audio 44100 []
renderConcat tracks = foldl' concatAudio (Audio 44100 []) (map trackAudio tracks) 

renderMix :: [Track] -> Audio
renderMix [] = Audio 44100 []
renderMix tracks = foldl1 mixAudio (map trackAudio tracks)


-- LOGIC
loadWavLogic :: FilePath -> IO (Either String Audio)
loadWavLogic = readWav

synthLogic :: String -> Either String Audio
synthLogic content = case parseComposition content of 
    Left e -> Left (show e) 
    Right n -> Right (synthesize n)


sliceTrackLogic :: Double -> Double -> Track -> [Track]
sliceTrackLogic s e (Track name audio@(Audio rate chans)) =
    let
        
        totalLenSamples = if null chans then 0 else length (head chans)
        totalDur = if rate == 0 then 0 else fromIntegral totalLenSamples / fromIntegral rate
        
       
        makePart suffix tStart tEnd = 
            Track (name ++ suffix) (trimAudio tStart tEnd audio)

       
        partBefore = if s > 0.1 
                     then [makePart "_part1" 0 s] 
                     else []

        
        partSelected = [makePart "_part2" s e]

        
        partAfter = if e < (totalDur - 0.1) 
                    then [makePart "_part3" e totalDur] 
                    else []
    in
        partBefore ++ partSelected ++ partAfter


trimLogic :: Double -> Double -> Audio -> Audio
trimLogic = trimAudio

effectVolumeLogic :: Double -> Audio -> Audio
effectVolumeLogic = changeAmplitude

effectOffsetLogic :: Double -> Audio -> Audio
effectOffsetLogic = addAmplitude

effectSpeedLogic :: Double -> Audio -> Audio
effectSpeedLogic = changeSpeed

effectSetRateLogic :: Int -> Audio -> Audio
effectSetRateLogic newRate (Audio _ c) = Audio newRate c

effectNormalizeLogic :: Audio -> Audio
effectNormalizeLogic = normalize 1.0

gateLogic :: String -> Double -> Audio -> Audio
gateLogic = removeByAmplitude

effectEchoLogic :: Audio -> Audio
effectEchoLogic = applyEcho 0.5 0.8

distortLogic :: Audio -> Audio
distortLogic = applyDistortion 50.0