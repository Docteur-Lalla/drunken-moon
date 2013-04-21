module Time (wait) where

import Music

import Data.IORef
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Graphics.UI.SDL as SDL
import Data.Word

-- Temps passé
the_time :: IORef Word32
the_time = unsafePerformIO $ newIORef 0

{-
  Remplace la fonction delay, calcule la différence de temps entre
  deux appels de la fonction, si la différence est plus petite que
  x, on attend y secondes.
  
  /!\ Cette fonction gère en plus la musique de fond, et définit le
  moment de loop de la musique, elle doit donc être utilisée à la
  place de delay pour le fonctionnement de la musique /!\
-}
wait step =
  do
    before <- readIORef the_time
    now <- getTicks
    if (before+step > now)
    then delay (y before now step)
    else return ()
      
    actual_time <- getTicks
    manageBGM (toSec (delta actual_time before))
    writeIORef the_time actual_time
    
    where
      y b n x = (delta b n) + x
      delta b n = b - n
      toSec x = (fromIntegral x) / 1000
