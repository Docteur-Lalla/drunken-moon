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

-- Joue un son au canal 'ch', si ch = -1 alors on prend un canal libre 
-- canal libre = canal qui ne jou pas de son pour le moment
{-
  liste des canaux pour chaque son:
    sons_du_menu: 1
    joueur_lance_projectile: 2
    enemi_lance_projectile: 3
    spellcard_lancee: 4
    boss_meurt: 5
-}
playSound :: String -> Channel -> IO ()
playSound name ch =
  do
    s <- getSound name
    case s of
      Nothing -> return 0
      Just chunk -> playChannel ch chunk 0
    return ()

playSelect :: IO ()
playSelect = playSound "select" 1

playOk :: IO ()
playOk = playSound "ok" 1

playCancel :: IO ()
playCancel = playSound "cancel" 1

playPlayerBullet :: IO ()
playPlayerBullet = playSound "playerbullet" 2

playEnemyBullet :: IO ()
playEnemyBullet = playSound "enemybullet" 3

playSpellCard :: IO ()
playSpellCard = playSound "spellcard" 4

playBossDie :: IO ()
playBossDie = playSound "bossdie" 5
