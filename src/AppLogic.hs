module AppLogic where

import AudioLib
import Data.List (foldl')

-- ============================================================
-- PROJECT STATE
-- ============================================================

data Track = Track 
    { trackName :: String
    , trackAudio :: Audio 
    } deriving (Show)

-- Вспомогательные функции для списков
addTrack :: String -> Audio -> [Track] -> [Track]
addTrack name aud tracks = tracks ++ [Track name aud]

removeTrack :: Int -> [Track] -> [Track]
removeTrack idx tracks = 
    let (left, right) = splitAt idx tracks
    in left ++ drop 1 right

-- Применяет функцию к аудио конкретного трека
modifyTrack :: Int -> (Audio -> Audio) -> [Track] -> [Track]
modifyTrack idx f tracks 
    | idx < 0 || idx >= length tracks = tracks
    | otherwise = 
        let (left, r:right) = splitAt idx tracks
            newTrack = r { trackAudio = f (trackAudio r) }
        in left ++ (newTrack : right)

-- Получить аудио конкретного трека
getTrackAudio :: Int -> [Track] -> Maybe Audio
getTrackAudio idx tracks
    | idx < 0 || idx >= length tracks = Nothing
    | otherwise = Just $ trackAudio (tracks !! idx)

-- Склеить все треки в один (последовательно)
renderConcat :: [Track] -> Audio
renderConcat [] = Audio 44100 [] -- Пустой аудио
renderConcat tracks = foldl1 concatAudio (map trackAudio tracks)

-- Смешать все треки в один (одновременно)
renderMix :: [Track] -> Audio
renderMix [] = Audio 44100 []
renderMix tracks = foldl1 mixAudio (map trackAudio tracks)

-- ============================================================
-- WRAPPERS (LOGIC)
-- ============================================================

loadWavLogic :: FilePath -> IO (Either String Audio)
loadWavLogic = readWav

synthLogic :: String -> Either String Audio
synthLogic content = case parseComposition content of 
    Left e -> Left (show e) 
    Right n -> Right (synthesize n)

-- Эффекты (те же самые, просто экспортируем)
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