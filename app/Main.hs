{-# LANGUAGE RecordWildCards #-}

module Main where

import AudioLib
import System.IO
import Control.Exception

-- CLI утилиты
prompt :: IO String
prompt = putStr "> " >> hFlush stdout >> getLine

askFile :: String -> IO FilePath
askFile label = putStrLn label >> prompt

askDouble :: String -> IO Double
askDouble label = do
  putStrLn label
  s <- prompt
  case reads s of
    [(x, "")] -> return x
    _ -> putStrLn "Error: invalid number" >> askDouble label

drawHeader :: Maybe Audio -> IO ()
drawHeader cur = do
  putStrLn "\n=== Audio Synthesizer ==="
  case cur of
    Nothing -> putStrLn "Current: none"
    Just (Audio rate chans) -> 
      putStrLn $ "Current: " ++ show rate ++ "Hz, " ++ show (length chans) ++ "ch"
  putStrLn ""

-- Главное меню
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetEncoding stdout utf8
  mainMenu Nothing

mainMenu :: Maybe Audio -> IO ()
mainMenu cur = do
  drawHeader cur
  putStrLn "Main menu:"
  putStrLn "  1. Files"
  putStrLn "  2. Synthesis"
  putStrLn "  3. Effects"
  putStrLn "  0. Exit"
  c <- prompt
  case c of
    "1" -> filesMenu cur
    "2" -> synthMenu cur
    "3" -> effectsMenu cur
    "0" -> putStrLn "Bye" >> return ()
    _ -> putStrLn "Invalid option" >> mainMenu cur

-- Меню файлов
filesMenu :: Maybe Audio -> IO ()
filesMenu cur = do
  drawHeader cur
  putStrLn "Files:"
  putStrLn "  1. Load WAV"
  putStrLn "  2. Save WAV"
  putStrLn "  3. Mix with WAV"
  putStrLn "  9. Back"
  c <- prompt
  case c of
    "1" -> do
      f <- askFile "WAV file path:"
      r <- readWav f
      case r of
        Left e -> putStrLn ("Error: " ++ e) >> filesMenu cur
        Right a -> putStrLn "Loaded" >> mainMenu (Just a)
    "2" ->
      case cur of
        Nothing -> putStrLn "No audio to save" >> filesMenu cur
        Just a -> do
          f <- askFile "Output file:"
          writeWav f a
          putStrLn "Saved"
          mainMenu cur
    "3" ->
      case cur of
        Nothing -> putStrLn "Load audio first" >> filesMenu cur
        Just a1 -> do
          f <- askFile "WAV to mix:"
          r <- readWav f
          case r of
            Left e -> putStrLn ("Error: " ++ e) >> filesMenu cur
            Right a2 -> do
              let mixed = mixAudio a1 a2
              putStrLn "Mixed"
              mainMenu (Just mixed)
    "9" -> mainMenu cur
    _ -> putStrLn "Invalid option" >> filesMenu cur

-- Меню синтеза
synthMenu :: Maybe Audio -> IO ()
synthMenu cur = do
  drawHeader cur
  putStrLn "Synthesis:"
  putStrLn "  1. From TXT file"
  putStrLn "  9. Back"
  c <- prompt
  case c of
    "1" -> do
      f <- askFile "TXT file (format: C4 0.5):"
      content <- readFile f
      case parseComposition content of
        Left e -> putStrLn ("Parse error: " ++ show e) >> synthMenu cur
        Right notes -> do
          let a = synthesize notes
          putStrLn $ "Synthesized " ++ show (length notes) ++ " notes"
          mainMenu (Just a)
    "9" -> mainMenu cur
    _ -> putStrLn "Invalid option" >> synthMenu cur

-- Меню эффектов
effectsMenu :: Maybe Audio -> IO ()
effectsMenu cur = do
  drawHeader cur
  putStrLn "Effects:"
  putStrLn "  1. Volume (factor)"
  putStrLn "  2. Speed (factor)"
  putStrLn "  3. Normalize"
  putStrLn "  4. Gate (threshold)"
  putStrLn "  5. Echo"
  putStrLn "  6. Distortion"
  putStrLn "  9. Back"
  c <- prompt
  case c of
    "1" -> withAudio cur $ \a -> do 
      x <- askDouble "Volume factor (0.5=quieter, 2.0=louder):"
      pure (changeAmplitude x a)
    "2" -> withAudio cur $ \a -> do 
      x <- askDouble "Speed factor (0.5=slower, 2.0=faster):"
      pure (changeSpeed x a)
    "3" -> withAudio cur $ \a -> pure (normalize 1.0 a)
    "4" -> withAudio cur $ \a -> do 
      x <- askDouble "Threshold (0.0-1.0):"
      pure (removeByAmplitude "lower" x a)
    "5" -> withAudio cur $ \a -> pure (applyEcho 0.5 0.8 a)
    "6" -> withAudio cur $ \a -> pure (applyDistortion 50.0 a)
    "9" -> mainMenu cur
    _ -> putStrLn "Invalid option" >> effectsMenu cur
  where
    withAudio :: Maybe Audio -> (Audio -> IO Audio) -> IO ()
    withAudio Nothing _ = putStrLn "No audio loaded" >> effectsMenu cur
    withAudio (Just a) f = do
      a' <- f a
      putStrLn "Applied"
      mainMenu (Just a')
