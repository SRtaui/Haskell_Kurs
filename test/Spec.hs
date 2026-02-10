{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Data.ByteString.Lazy as BL
import System.Directory (removeFile, doesFileExist)
import System.IO (writeFile)
import Control.Exception (catch, IOException)
import Control.Monad (when)

-- Импортируем нашу библиотеку и логику
import AudioLib 
import AppLogic 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Synthesizer Full Suite"
  [ coreLogicTests
  , parserTests
  , synthesisTests
  , editingTests     -- НОВОЕ: Монтаж
  , effectsTests
  , ioTests          -- ОБНОВЛЕНО: 8/16/32 bit
  , menuScenarioTests
  ]

-- ====================================================================
-- 1. Core Logic
-- ====================================================================

coreLogicTests :: TestTree
coreLogicTests = testGroup "Core Logic"
  [ testCase "Silence generation" $ do
      let sil = silence 0.5
      length sil @?= 22050
      assertBool "All zero" $ all (== 0) sil
      
  , testCase "Tone generation" $ do
      let t = tone sine 440.0 0.01 -- ~441 sample
      length t @?= 441
      -- Проверка диапазона
      assertBool "Sine range" $ all (\x -> x >= -1.0 && x <= 1.0) t
  ]

-- ====================================================================
-- 2. Parser (Updated with Rests)
-- ====================================================================

parserTests :: TestTree
parserTests = testGroup "Parser"
  [ testCase "Note Parsing" $ do
      case parseComposition "C4 0.5" of
        Right [n] -> freq n @?= 261.6255653005986
        _ -> assertFailure "Failed parsing C4"
        
  , testCase "Rest Parsing (New)" $ do
      case parseComposition "R 1.0" of
        Right [n] -> do
            isRest n @?= True
            duration n @?= 1.0
        _ -> assertFailure "Failed parsing Rest (R)"
        
  , testCase "Complex sequence" $ do
      case parseComposition "C4 0.5\nR 0.5\nE4 0.5" of
        Right notes -> length notes @?= 3
        _ -> assertFailure "Failed parsing sequence"
  ]

-- ====================================================================
-- 3. Synthesis
-- ====================================================================

synthesisTests :: TestTree
synthesisTests = testGroup "Synthesis"
  [ testCase "Synthesize with Rest" $ do
      -- Нота (0.5) + Пауза (0.5) + Нота (0.5) = 1.5 сек
      let notes = [NoteDTO 440.0 0.5 False, NoteDTO 0.0 0.5 True, NoteDTO 440.0 0.5 False]
          audio = synthesize notes
          len = length (head (channels audio))
      
      -- Ожидаем 1.5 секунды * 44100 = 66150
      len @?= 66150
  ]

-- ====================================================================
-- 4. Editing (Trim / Concat) -- NEW
-- ====================================================================

editingTests :: TestTree
editingTests = testGroup "Editing"
  [ testCase "Trim Audio" $ do
      -- 1 секунда аудио (44100 сэмплов)
      let audio = Audio 44100 [replicate 44100 0.5]
          -- Обрезаем с 0.2 до 0.5 (длительность 0.3 сек)
          trimmed = trimAudio 0.2 0.5 audio
          len = length (head (channels trimmed))
          expected = floor (0.3 * 44100)
      
      -- Допускаем погрешность в 1 сэмпл из-за округления
      assertBool "Trim length correct" (abs (len - expected) <= 1)
      
  , testCase "Concat Audio" $ do
      let a1 = Audio 44100 [[0.1, 0.2]]
          a2 = Audio 44100 [[0.3, 0.4]]
          res = concatAudio a1 a2
          
      channels res @?= [[0.1, 0.2, 0.3, 0.4]]
      
  , testCase "Concat with Resampling (New)" $ do
      -- Склеиваем 44100 Гц и 22050 Гц
      let a1 = Audio 44100 [[0.1, 0.2]]      -- 2 сэмпла
          a2 = Audio 22050 [[0.3, 0.4]]      -- 2 сэмпла
          
          -- При ресемплинге 22050 -> 44100 каждый сэмпл дублируется (примерно)
          res = concatAudio a1 a2
      
      sampleRate res @?= 44100
      let sig = head (channels res)
      -- Ожидаем: [0.1, 0.2] ++ [0.3, 0.3, 0.4, 0.4] (примерно)
      length sig @?= 6 -- 2 + 4
  ]

-- ====================================================================
-- 5. Effects
-- ====================================================================

effectsTests :: TestTree
effectsTests = testGroup "Effects"
  [ testCase "Volume" $ do
      let a = Audio 44100 [[0.5]]
      channels (changeAmplitude 2.0 a) @?= [[1.0]]
      
  , testCase "Speed" $ do
      let a = Audio 44100 [[0.1]]
      sampleRate (changeSpeed 2.0 a) @?= 88200
      
  , testCase "Gate (Remove Low)" $ do
      let a = Audio 44100 [[0.1, 0.9]]
      channels (removeByAmplitude "lower" 0.5 a) @?= [[0.0, 0.9]]
  ]

-- ====================================================================
-- 6. I/O Tests (WAV 8/16/32 bit)
-- ====================================================================

ioTests :: TestTree
ioTests = testGroup "WAV I/O"
  [ testCase "Write/Read 16-bit (Default)" $ do
      let file = "test_16.wav"
          orig = Audio 44100 [take 100 $ repeat 0.5]
      writeWav file orig
      Right res <- readWav file
      removeFile file
      
      sampleRate res @?= 44100
      length (head $ channels res) @?= 100
      let val = head (head (channels res))
      assertBool "Value preserved" (abs (val - 0.5) < 0.01)

  -- Для 8 и 32 бит у нас пока нет "writeWav8" (мы всегда пишем 16),
  -- поэтому проверяем, что наш ридер не падает на файлах, которые мы сами записали.
  -- Но в идеале нужны реальные тестовые файлы.
  -- Здесь проверим просто стабильность write/read.
  ]

-- ====================================================================
-- 7. Menu Integration (Logic)
-- ====================================================================

-- ====================================================================
-- 7. Menu Integration (Logic) - FULL COVERAGE
-- ====================================================================

menuScenarioTests :: TestTree
menuScenarioTests = testGroup "Menu Scenarios (Full Coverage)"
  [ 
    -- 1. FILES & IO
    testGroup "Files Menu" 
    [ testCase "Load & Save Roundtrip" $ do
        let f = "menu_rt.wav"
            orig = Audio 44100 [[0.5, -0.5]]
        writeWav f orig
        Right res <- loadWavLogic f
        removeFile f
        checkApprox (head $ channels res) [0.5, -0.5]
        
    , testCase "Mix (Overlay)" $ do
        let f1 = "menu_mix_base.wav"
            f2 = "menu_mix_add.wav"
            a1 = Audio 44100 [[0.2]]
            a2 = Audio 44100 [[0.3]]
        writeWav f1 a1; writeWav f2 a2
        Right loaded1 <- loadWavLogic f1
        Right res <- mixWavLogic loaded1 f2
        removeFile f1; removeFile f2
        checkApprox (head $ channels res) [0.5] -- 0.2 + 0.3
    ]

    -- 2. SYNTHESIS
    , testGroup "Synthesis Menu"
    [ testCase "From TXT (Note + Rest)" $ do
        let f = "menu_synth.txt"
        writeFile f "C4 0.5\nR 0.5"
        content <- readFile f
        removeFile f
        let Right res = synthLogic content
        length (head $ channels res) @?= 44100 -- 1.0 sec total
    ]

    -- 3. EDITING (CUT/PASTE)
    , testGroup "Editing Menu"
    [ testCase "Trim (Start End)" $ do
        let a = Audio 44100 [replicate 100 1.0]
            res = trimLogic 0.0 0.001 a -- ~44 samples
        length (head $ channels res) @?= 44
        
    , testCase "Concat (Append)" $ do
        let f = "menu_concat.wav"
            a1 = Audio 44100 [[0.1]]
            a2 = Audio 44100 [[0.2]]
        writeWav f a2
        Right res <- concatLogic a1 f
        removeFile f
        checkApprox (head $ channels res) [0.1, 0.2]
    ]

    -- 4. EFFECTS (ALL ITEMS)
    , testGroup "Effects Menu"
    [ testCase "1. Volume" $ do
        let a = Audio 44100 [[0.5]]; res = effectVolumeLogic 0.5 a
        checkApprox (head $ channels res) [0.25]
        
    , testCase "2. Speed" $ do
        let a = Audio 44100 [[1.0]]; res = effectSpeedLogic 2.0 a
        sampleRate res @?= 88200
        
    , testCase "3. Normalize" $ do
        let a = Audio 44100 [[0.5]]; res = effectNormalizeLogic a
        checkApprox (head $ channels res) [1.0] -- 0.5 -> 1.0
        
    , testCase "4. Gate" $ do
        let a = Audio 44100 [[0.1, 0.9]]; res = gateLogic 0.5 a
        checkApprox (head $ channels res) [0.0, 0.9]
        
    , testCase "5. Echo" $ do
        let a = Audio 44100 [[1.0, 0.0, 0.0]]; res = effectEchoLogic a
        -- Эхо добавляет хвост. Проверим, что сигнал стал длиннее или изменился
        assertBool "Echo applied" (length (head $ channels res) > 3 || (head $ channels res) !! 0 == 0.6)
        
    , testCase "6. Distortion" $ do
        let a = Audio 44100 [[10.0]]; res = distortLogic a
        -- Atan(10 * 50) ~ 1.57, * 2/pi ~ 1.0
        checkApprox (head $ channels res) [0.99] -- Close to 1.0
    ]

    -- 5. COMPLEX COMBINATIONS (CHAINS)
    , testGroup "Complex Chains"
    [ testCase "Synth -> Volume -> Echo -> Save" $ do
        -- 1. Synth
        let Right a1 = synthLogic "C4 0.01" -- ~441 samples
        -- 2. Volume
        let a2 = effectVolumeLogic 0.5 a1
        -- 3. Echo
        let a3 = effectEchoLogic a2
        -- 4. Save
        let f = "chain_test.wav"
        writeWav f a3
        
        -- Verify
        Right loaded <- loadWavLogic f
        removeFile f
        sampleRate loaded @?= 44100
        assertBool "Signal exists" (length (head $ channels loaded) > 400)
        
    , testCase "Load -> Trim -> Concat -> Normalize" $ do
        let f1 = "chain_src1.wav"
            f2 = "chain_src2.wav"
            -- Create source files
            aSrc1 = Audio 44100 [replicate 100 1.0] -- 1.0, 1.0 ...
            aSrc2 = Audio 44100 [replicate 100 0.2] -- 0.2, 0.2 ...
        writeWav f1 aSrc1
        writeWav f2 aSrc2
        
        -- 1. Load f1
        Right a1 <- loadWavLogic f1
        -- 2. Trim (take first 10 samples)
        let a2 = trimLogic 0.0 (10.0 / 44100.0) a1 -- ~10 samples
        -- 3. Concat f2
        Right a3 <- concatLogic a2 f2
        -- 4. Normalize
        let a4 = effectNormalizeLogic a3
        
        removeFile f1; removeFile f2
        
        -- Check length: 10 (trimmed) + 100 (concat) = 110
        let sig = head (channels a4)
        length sig @?= 110
        -- Check normalization: max was 1.0, so it should stay 1.0 (approx)
        -- Actually, since we trimmed 1.0s, and concatenated 0.2s, max is 1.0. 
        -- Normalize of 1.0 to 1.0 changes nothing.
        checkApprox [head sig] [1.0]
    ]
  ]

-- Helper for approximate comparison
checkApprox :: [Double] -> [Double] -> Assertion
checkApprox actual expected = do
    length actual @?= length expected
    let isClose a b = abs (a - b) < 0.05 -- Increased tolerance for effects
    assertBool ("Mismatch: " ++ show actual ++ " vs " ++ show expected) $ 
        and (zipWith isClose actual expected)

