{-
 * Copyright (c) 2013, Kévin Le Bon
 * All rights reserved.
 * Copyright (c) 2013, Arthur Blanleuil
 * All rights reserved.

 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of Kévin Le Bon nor the names of its contributors
 *       may be used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE REGENTS AND CONTRIBUTORS BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 -}
 
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
