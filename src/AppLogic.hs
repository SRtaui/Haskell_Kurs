module AppLogic where

import AudioLib

-- Существующая логика
loadWavLogic = readWav
mixWavLogic current path = do
    res <- readWav path
    return $ fmap (mixAudio current) res
synthLogic content = case parseComposition content of Left e -> Left (show e); Right n -> Right (synthesize n)
effectVolumeLogic = changeAmplitude
effectSpeedLogic = changeSpeed
effectNormalizeLogic = normalize 1.0
effectEchoLogic = applyEcho 0.5 0.8

-- НОВАЯ ЛОГИКА
concatLogic :: Audio -> FilePath -> IO (Either String Audio)
concatLogic current path = do
    res <- readWav path
    return $ fmap (concatAudio current) res

trimLogic :: Double -> Double -> Audio -> Audio
trimLogic = trimAudio

distortLogic :: Audio -> Audio
distortLogic = applyDistortion 50.0

gateLogic :: Double -> Audio -> Audio
gateLogic = removeByAmplitude "lower"
