{-# LANGUAGE RecordWildCards #-}
module Main where

import AudioLib
import AppLogic
import System.IO
import Text.Printf (printf) -- Для красивого форматирования (если доступен), иначе упростим

-- ============================================================
-- UI UTILS
-- ============================================================

prompt :: IO String
prompt = putStr "> " >> hFlush stdout >> getLine

askString :: String -> IO String
askString l = putStrLn l >> prompt

askInt :: String -> IO Int
askInt l = putStrLn l >> prompt >>= \s -> case reads s of 
    [(x,"")] -> return x
    _ -> putStrLn "Error: Invalid integer" >> askInt l

askDouble :: String -> IO Double
askDouble l = putStrLn l >> prompt >>= \s -> case reads s of 
    [(x,"")] -> return x
    _ -> putStrLn "Error: Invalid number" >> askDouble l

-- Рисуем таблицу треков
drawProjectState :: [Track] -> IO ()
drawProjectState [] = putStrLn "\n[ Project is empty. Use 'Add' to load files. ]"
drawProjectState tracks = do
    putStrLn "\n=== PROJECT TRACKS ==="
    putStrLn "ID | Name             | Freq  | Chans | Duration"
    putStrLn "------------------------------------------------"
    mapM_ printTrack (zip [1..] tracks)
    putStrLn "------------------------------------------------"
  where
    printTrack (i, Track name (Audio r c)) = do
        let dur = if r == 0 then 0 else fromIntegral (length (head c)) / fromIntegral r :: Double
        putStrLn $ show i ++ ". | " ++ take 16 (name ++ replicate 20 ' ') ++ " | " ++ show r ++ " | " ++ show (length c) ++ "     | " ++ show dur ++ "s"

-- Запрашиваем ID трека у пользователя (возвращаем индекс 0-based)
askTrackID :: [Track] -> IO (Maybe Int)
askTrackID tracks = do
    i <- askInt "Select Track ID:"
    if i >= 1 && i <= length tracks
        then return (Just (i - 1))
        else putStrLn "Invalid ID" >> return Nothing

-- ============================================================
-- MAIN LOOP
-- ============================================================

main :: IO ()
main = hSetBuffering stdout NoBuffering >> mainMenu []

mainMenu :: [Track] -> IO ()
mainMenu tracks = do
  drawProjectState tracks
  putStrLn "\n1. Add Track (Load/Synth)"
  putStrLn "2. Edit Track (Trim/Remove)"
  putStrLn "3. Apply Effects to Track"
  putStrLn "4. Render & Save Project"
  putStrLn "0. Exit"
  
  c <- prompt
  case c of
    "1" -> addMenu tracks
    "2" -> editMenu tracks
    "3" -> effectsMenu tracks
    "4" -> renderMenu tracks
    "0" -> putStrLn "Goodbye!" >> return ()
    _   -> mainMenu tracks

-- ============================================================
-- MENUS
-- ============================================================

-- ADD MENU
addMenu :: [Track] -> IO ()
addMenu tracks = do
    putStrLn "\n--- Add Track ---"
    putStrLn "1. Load WAV File"
    putStrLn "2. Generate from TXT"
    putStrLn "9. Back"
    c <- prompt
    case c of
        "1" -> do
            path <- askString "Path to WAV:"
            res <- loadWavLogic path
            case res of
                Left e -> print e >> mainMenu tracks
                Right a -> mainMenu (addTrack path a tracks)
        "2" -> do
            path <- askString "Path to TXT notes:"
            content <- readFile path
            case synthLogic content of
                Left e -> print e >> mainMenu tracks
                Right a -> mainMenu (addTrack ("Synth: " ++ path) a tracks)
        "9" -> mainMenu tracks
        _   -> addMenu tracks

-- EDIT MENU (Trim / Delete)
editMenu :: [Track] -> IO ()
editMenu tracks = do
    putStrLn "\n--- Edit Track ---"
    putStrLn "1. Trim Track (Start/End)"
    putStrLn "2. Remove Track from List"
    putStrLn "9. Back"
    c <- prompt
    case c of
        "1" -> do
            mid <- askTrackID tracks
            case mid of
                Nothing -> editMenu tracks
                Just idx -> do
                    s <- askDouble "Start (sec):"
                    e <- askDouble "End (sec):"
                    let newTracks = modifyTrack idx (trimLogic s e) tracks
                    mainMenu newTracks
        "2" -> do
            mid <- askTrackID tracks
            case mid of
                Nothing -> editMenu tracks
                Just idx -> mainMenu (removeTrack idx tracks)
        "9" -> mainMenu tracks
        _   -> editMenu tracks

-- EFFECTS MENU
-- EFFECTS MENU
effectsMenu :: [Track] -> IO ()
effectsMenu tracks = do
    if null tracks 
        then do
            putStrLn "No tracks!" 
            mainMenu tracks 
        else do
            putStrLn "\n--- Effects Processor ---"
            putStrLn "1. Volume Factor (Multiply)"
            putStrLn "2. Speed Factor"
            putStrLn "3. Amplitude Offset (+Val)"
            putStrLn "4. Set SampleRate"
            putStrLn "5. Normalize"
            putStrLn "6. Gate"
            putStrLn "7. Echo"
            putStrLn "8. Distortion"
            putStrLn "9. Back"
            
            c <- prompt
            if c == "9" then mainMenu tracks else do
                
                -- Сначала выбираем эффект, потом трек
                mid <- askTrackID tracks
                case mid of
                    Nothing -> effectsMenu tracks
                    Just idx -> do
                        -- Применяем логику
                        let apply f = mainMenu (modifyTrack idx f tracks)
                        
                        case c of
                            "1" -> askDouble "Factor:" >>= \x -> apply (effectVolumeLogic x)
                            "2" -> askDouble "Factor:" >>= \x -> apply (effectSpeedLogic x)
                            "3" -> askDouble "Offset:" >>= \x -> apply (effectOffsetLogic x)
                            "4" -> askDouble "New Rate:" >>= \x -> apply (effectSetRateLogic (round x))
                            "5" -> apply effectNormalizeLogic
                            "6" -> do
                                m <- askString "Mode (lower/higher/equal):"
                                t <- askDouble "Threshold:"
                                apply (gateLogic m t)
                            "7" -> apply effectEchoLogic
                            "8" -> apply distortLogic
                            _   -> effectsMenu tracks
-- RENDER MENU
renderMenu :: [Track] -> IO ()
renderMenu tracks = do
    putStrLn "\n--- Render & Save ---"
    putStrLn "1. Concat All (Play Sequentially: 1 -> 2 -> 3)"
    putStrLn "2. Mix All (Overlay: 1 + 2 + 3)"
    putStrLn "3. Save Specific Track Only"
    putStrLn "9. Back"
    c <- prompt
    case c of
        "1" -> do
            let final = renderConcat tracks
            path <- askString "Output Filename (e.g. out.wav):"
            writeWav path final
            putStrLn "Saved!"
            mainMenu tracks
        "2" -> do
            let final = renderMix tracks
            path <- askString "Output Filename (e.g. out.wav):"
            writeWav path final
            putStrLn "Saved!"
            mainMenu tracks
        "3" -> do
            mid <- askTrackID tracks
            case mid of 
                Nothing -> renderMenu tracks
                Just idx -> do
                    let (Just a) = getTrackAudio idx tracks
                    path <- askString "Output Filename:"
                    writeWav path a
                    putStrLn "Saved!"
                    mainMenu tracks
        "9" -> mainMenu tracks
        _   -> renderMenu tracks