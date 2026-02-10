{-# LANGUAGE RecordWildCards #-}
module Main where

import AudioLib
import AppLogic
import System.IO
import Text.Read (readMaybe)


-- UI UTILS 
prompt :: IO String
prompt = putStr "> " >> hFlush stdout >> getLine

askString :: String -> IO String
askString l = putStrLn l >> prompt


askInt :: String -> IO Int
askInt l = do
    putStrLn l
    s <- prompt
    case readMaybe s of
        Just x -> return x
        Nothing -> do
            putStrLn ">>> ОШИБКА: Введите целое число (например: 1, 2, 44100)."
            askInt l


askDouble :: String -> IO Double
askDouble l = do
    putStrLn l
    s <- prompt
    case readMaybe s of
        Just x -> return x
        Nothing -> do
            putStrLn ">>> ОШИБКА: Введите число (например: 0.5, 10.0)."
            askDouble l


drawProjectState :: [Track] -> IO ()
drawProjectState [] = putStrLn "\n[ Проект пуст. Используйте 'Добавить', чтобы загрузить файлы. ]"
drawProjectState tracks = do
    putStrLn "\n=== ТРЕКИ ПРОЕКТА ==="
    putStrLn "ID | Имя              | Гц    | Кан.  | Время"
    putStrLn "------------------------------------------------"
    mapM_ printTrack (zip [1..] tracks)
    putStrLn "------------------------------------------------"
  where
    printTrack (i, Track name (Audio r c)) = do
        let dur = if r == 0 then 0 else fromIntegral (length (head c)) / fromIntegral r :: Double
        putStrLn $ show i ++ ". | " ++ take 16 (name ++ replicate 20 ' ') ++ " | " ++ show r ++ " | " ++ show (length c) ++ "     | " ++ show dur ++ "с"


askTrackID :: [Track] -> IO (Maybe Int)
askTrackID tracks = do
    i <- askInt "Выберите ID трека:"
    if i >= 1 && i <= length tracks
        then return (Just (i - 1))
        else do
            putStrLn $ ">>> ОШИБКА: Трека с ID " ++ show i ++ " не существует."
            return Nothing


-- ГЛАВНЫЙ ЦИКЛ 
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    hSetEncoding stdout utf8 
    hSetEncoding stdin utf8
    mainMenu []

mainMenu :: [Track] -> IO ()
mainMenu tracks = do
  drawProjectState tracks
  putStrLn "\n1. Добавить трек (Загрузка/Синтез)"
  putStrLn "2. Редактировать (Разрезать/Удалить)"
  putStrLn "3. Эффекты"
  putStrLn "4. Рендер и Сохранение"
  putStrLn "0. Выход"
  
  c <- prompt
  case c of
    "1" -> addMenu tracks
    "2" -> editMenu tracks
    "3" -> effectsMenu tracks
    "4" -> renderMenu tracks
    "0" -> putStrLn "До свидания!" >> return ()
    _   -> do 
        putStrLn ">>> Неверный выбор. Попробуйте снова."
        mainMenu tracks


-- МЕНЮ 
addMenu :: [Track] -> IO ()
addMenu tracks = do
    putStrLn "\n--- Добавить трек ---"
    putStrLn "1. Загрузить WAV файл"
    putStrLn "2. Сгенерировать из TXT (Синтезатор)"
    putStrLn "9. Назад"
    c <- prompt
    case c of
        "1" -> do
            path <- askString "Путь к WAV файлу:"
            res <- loadWavLogic path
            case res of
                Left e -> putStrLn (">>> ОШИБКА ЗАГРУЗКИ: " ++ e) >> mainMenu tracks
                Right a -> mainMenu (addTrack path a tracks)
        "2" -> do
            path <- askString "Путь к файлу с нотами (TXT):"
            content <- readFile path 
            case synthLogic content of
                Left e -> putStrLn (">>> ОШИБКА СИНТЕЗА: " ++ e) >> mainMenu tracks
                Right a -> mainMenu (addTrack ("Синтез: " ++ path) a tracks)
        "9" -> mainMenu tracks
        _   -> addMenu tracks


editMenu :: [Track] -> IO ()
editMenu tracks = do
    putStrLn "\n--- Редактирование ---"
    putStrLn "1. Разрезать трек (Slice / Split)"
    putStrLn "2. Удалить трек из списка"
    putStrLn "9. Назад"
    c <- prompt
    case c of
        "1" -> do
            mid <- askTrackID tracks
            case mid of
                Nothing -> editMenu tracks
                Just idx -> do
                    putStrLn "Введите диапазон для вырезания (трек будет разрезан по этим точкам)."
                    s <- askDouble "Точка начала (сек):"
                    e <- askDouble "Точка конца (сек):"
                    
                    if s < 0 || s >= e 
                        then do
                            putStrLn ">>> ОШИБКА: Начало должно быть >= 0, а Конец > Начала!"
                            editMenu tracks
                        else do
                            let targetTrack = tracks !! idx
                            let newParts = sliceTrackLogic s e targetTrack
                            let newTrackList = replaceTrackWithMany idx newParts tracks
                            putStrLn $ "Трек разрезан на " ++ show (length newParts) ++ " частей."
                            mainMenu newTrackList

        "2" -> do
            mid <- askTrackID tracks
            case mid of
                Nothing -> editMenu tracks
                Just idx -> mainMenu (removeTrack idx tracks)
        "9" -> mainMenu tracks
        _   -> editMenu tracks


effectsMenu :: [Track] -> IO ()
effectsMenu tracks = do
    if null tracks 
        then do
            putStrLn "Нет треков для обработки!" 
            mainMenu tracks 
        else do
            putStrLn "\n--- Процессор Эффектов ---"
            putStrLn "1. Громкость (Умножение)"
            putStrLn "2. Скорость воспроизведения (Pitch/Speed)"
            putStrLn "3. Смещение амплитуды (+Значение)"
            putStrLn "4. Установить частоту дискретизации (SampleRate)"
            putStrLn "5. Нормализация"
            putStrLn "6. Гейт (Gate - удаление тишины/шума)"
            putStrLn "7. Эхо (Delay)"
            putStrLn "8. Дисторшн (Distortion)"
            putStrLn "9. Назад"
            
            c <- prompt
            if c == "9" then mainMenu tracks else do
                
                mid <- askTrackID tracks
                case mid of
                    Nothing -> effectsMenu tracks
                    Just idx -> do
                        let apply f = mainMenu (modifyTrack idx f tracks)
                        
                        case c of
                            "1" -> askDouble "Коэффициент (например 0.5 или 2.0):" >>= \x -> apply (effectVolumeLogic x)
                            "2" -> askDouble "Коэффициент скорости (0.5=медленно, 2.0=быстро):" >>= \x -> apply (effectSpeedLogic x)
                            "3" -> askDouble "Значение смещения (-1.0 ... 1.0):" >>= \x -> apply (effectOffsetLogic x)
                            "4" -> askDouble "Новая частота (например 44100):" >>= \x -> apply (effectSetRateLogic (round x))
                            "5" -> apply effectNormalizeLogic
                            "6" -> do
                                putStrLn "Введите режим (lower - ниже порога, higher - выше, equal - равно):"
                                m <- prompt 
                                t <- askDouble "Порог (Threshold):"
                                apply (gateLogic m t)
                            "7" -> apply effectEchoLogic
                            "8" -> apply distortLogic
                            _   -> effectsMenu tracks


renderMenu :: [Track] -> IO ()
renderMenu tracks = do
    putStrLn "\n--- Рендер и Сохранение ---"
    putStrLn "1. Склеить все (Последовательно: 1 -> 2 -> 3)"
    putStrLn "2. Смешать все (Наложение: 1 + 2 + 3)"
    putStrLn "3. Сохранить отдельный трек"
    putStrLn "9. Назад"
    c <- prompt
    case c of
        "1" -> do
            let final = renderConcat tracks
            path <- askString "Имя выходного файла (например out.wav):"
            writeWav path final
            putStrLn "Файл сохранен!"
            mainMenu tracks
        "2" -> do
            let final = renderMix tracks
            path <- askString "Имя выходного файла (например mix.wav):"
            writeWav path final
            putStrLn "Файл сохранен!"
            mainMenu tracks
        "3" -> do
            mid <- askTrackID tracks
            case mid of 
                Nothing -> renderMenu tracks
                Just idx -> do
                    let (Just a) = getTrackAudio idx tracks
                    path <- askString "Имя файла:"
                    writeWav path a
                    putStrLn "Файл сохранен!"
                    mainMenu tracks
        "9" -> mainMenu tracks
        _   -> renderMenu tracks