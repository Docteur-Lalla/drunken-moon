module Music where

import Resources
import Graphics.UI.SDL.Mixer
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Data.IORef

-- Contient le temps de lecture d'une musique en secondes
bgm_pos :: IORef Double
bgm_pos = unsafePerformIO $ newIORef 0

-- Contient la musique de fond actuelle
actual_bgm :: IORef (Maybe BGM)
actual_bgm = unsafePerformIO $ newIORef Nothing

{-
  Fonction appelée à chaque tour de boucle, elle gère le fait que la
  musique tourne en boucle, on teste d'abord si une musique de fond
  tourne, puis, si uen musique tourne et est finie, on la relance à
  partir de la fin de l'intro.
-}
manageBGM :: Double -> IO ()
manageBGM time =
  do
    bgm <- readIORef actual_bgm
    
    modifyIORef' bgm_pos (+time)
    pos <- readIORef bgm_pos
    
    case bgm of
      Nothing   -> return ()
      Just (m, i, r) -> if pos >= r
                        then
                          do
                            setMusicPosition i
                            writeIORef bgm_pos i
                        else
                          return ()


-- Lance une musique donnée
startBGM :: String -> IO ()
startBGM name =
  do
    m <- getMusic name
    
    -- Vérifie si la musique éxiste
    case m of
      Nothing   -> return ()
      
      -- si oui, on stop la musique de fond, on la change par la musique voulue et on relance la musique
      Just r@(m, _, _) -> do
        stopBGM
        writeIORef actual_bgm (Just r)
        playMusic m 1

-- Stop la musique de fond
stopBGM :: IO ()
stopBGM =
  do
    haltMusic
    
    writeIORef actual_bgm Nothing
    writeIORef bgm_pos 0



