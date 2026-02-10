{-# LANGUAGE RecordWildCards #-}
module Main where

import AudioLib
import AppLogic
import System.IO

-- Утилиты (prompt, askFile, askDouble - те же)
prompt = putStr "> " >> hFlush stdout >> getLine
askFile l = putStrLn l >> prompt
askDouble l = putStrLn l >> prompt >>= \s -> case reads s of [(x,"")] -> return x; _ -> putStrLn "Err" >> askDouble l
drawHeader Nothing = putStrLn "None"
drawHeader (Just (Audio r c)) = putStrLn $ "Audio: " ++ show r ++ "Hz, " ++ show (length c) ++ "ch, " ++ show (length (head c) `div` r) ++ "s"

main = hSetBuffering stdout NoBuffering >> mainMenu Nothing

mainMenu cur = do
  drawHeader cur
  putStrLn "1. Files & IO"
  putStrLn "2. Synthesis"
  putStrLn "3. Effects"
  putStrLn "4. Editing (Cut/Paste)"  -- НОВЫЙ ПУНКТ
  putStrLn "0. Exit"
  c <- prompt
  case c of
    "1" -> filesMenu cur
    "2" -> synthMenu cur
    "3" -> effectsMenu cur
    "4" -> editMenu cur
    "0" -> return ()
    _   -> mainMenu cur

-- FILES (обновлено)
filesMenu cur = do
    putStrLn "1. Load\n2. Save\n3. Mix (Overlay)\n9. Back"
    c <- prompt
    case c of
        "1" -> askFile "Path:" >>= loadWavLogic >>= \r -> case r of Left e -> print e >> filesMenu cur; Right a -> mainMenu (Just a)
        "2" -> case cur of Nothing -> print "No audio" >> filesMenu cur; Just a -> askFile "Out:" >>= \f -> writeWav f a >> mainMenu cur
        "3" -> case cur of Nothing -> print "No audio" >> filesMenu cur; Just a -> askFile "Path:" >>= mixWavLogic a >>= \r -> case r of Left e -> print e >> filesMenu cur; Right a2 -> mainMenu (Just a2)
        "9" -> mainMenu cur
        _   -> filesMenu cur

-- SYNTH (то же)
synthMenu cur = do
    putStrLn "1. From TXT\n9. Back"
    c <- prompt
    case c of
        "1" -> askFile "Path:" >>= readFile >>= \t -> case synthLogic t of Left e -> print e >> synthMenu cur; Right a -> mainMenu (Just a)
        "9" -> mainMenu cur
        _   -> synthMenu cur

-- EDITING (НОВОЕ МЕНЮ)
editMenu cur = do
    putStrLn "1. Trim (Start End)\n2. Concat (Append File)\n9. Back"
    c <- prompt
    case c of
        "1" -> case cur of 
            Nothing -> print "No audio" >> editMenu cur
            Just a -> do
                s <- askDouble "Start (sec):"
                e <- askDouble "End (sec):"
                mainMenu (Just $ trimLogic s e a)
        "2" -> case cur of
            Nothing -> print "No audio" >> editMenu cur
            Just a -> do
                f <- askFile "File to append:"
                res <- concatLogic a f
                case res of Left e -> print e >> editMenu cur; Right a2 -> mainMenu (Just a2)
        "9" -> mainMenu cur
        _   -> editMenu cur

-- EFFECTS (обновлено)
effectsMenu cur = do
    putStrLn "1. Volume\n2. Speed\n3. Normalize\n4. Gate\n5. Echo\n6. Distortion\n9. Back"
    c <- prompt
    case c of
        "1" -> withAudio cur $ \a -> askDouble "Factor:" >>= \x -> return (effectVolumeLogic x a)
        "2" -> withAudio cur $ \a -> askDouble "Factor:" >>= \x -> return (effectSpeedLogic x a)
        "3" -> withAudio cur $ \a -> return (effectNormalizeLogic a)
        "4" -> withAudio cur $ \a -> askDouble "Threshold:" >>= \x -> return (gateLogic x a)
        "5" -> withAudio cur $ \a -> return (effectEchoLogic a)
        "6" -> withAudio cur $ \a -> return (distortLogic a)
        "9" -> mainMenu cur
        _   -> effectsMenu cur

withAudio Nothing _ = print "No audio" >> effectsMenu Nothing
withAudio (Just a) f = f a >>= \a' -> print "Applied" >> mainMenu (Just a')
