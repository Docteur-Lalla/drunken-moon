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

module Game (newGame) where

import Score (writeScore)
import Resources
import Music
import Time
import GameData as GD
import Bullet

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as MIx

import Scripting.Lua as Lua
import LuaWrapper as HsLua
import System.Exit
import Data.IORef
import Data.Time
import System.IO.Unsafe

-- Fonction getDifficulty demandant la difficulté au joueur.

getDifficulty :: IO (Maybe Int)
getDifficulty = do
                  putStrLn "Quel mode choisissez-vous ?"
		  putStrLn "1 : Facile"
		  putStrLn "2 : Normal"
		  putStrLn "3 : Difficile"
		  putStrLn "4 : Lunatique"
		  putStrLn "5 : Extra"
                  c <- getLine
		  return (analyze c)

-- Conversion de la String en Maybe Int (Int si correct ou Nothing s'il y a erreur).

analyze :: String -> Maybe Int
analyze "1" = Just 0
analyze "2" = Just 1
analyze "3" = Just 2
analyze "4" = Just 3
analyze "5" = Just 4
analyze _ = Nothing

-- Conversion de Maybe Int vers String.

stringOfDiff :: Maybe Int -> String
stringOfDiff (Just 0) = "Easy"
stringOfDiff (Just 1) = "Normal"
stringOfDiff (Just 2) = "Hard"
stringOfDiff (Just 3) = "Lunatic"
stringOfDiff (Just 4) = "Extra"
stringOfDiff Nothing = "Undefined"

-- Crée une partie.

newGame :: Surface -> IO ()
newGame scr =
  do
    lua <- Lua.newstate
    Lua.openlibs lua

    Lua.registerhsfunction lua "hsPrint" HsLua.hsPrint
    HsLua.dofile lua "rc/game.lua"

    HsLua.fcall lua "main" 0 0
    
    enableKeyRepeat 0 0

    t0 <- getCurrentTime
    loop t0
    
    Lua.close lua

-- Blit le perso
blitPerso :: IO ()
blitPerso =
  do
    -- Récupère l'écran
    scr <- SDL.getVideoSurface
    -- Récupère les coordonnées
    p@(Player _ _ (x, y) _ _ _) <- readIORef joueur
    
    -- Affiche le perso et rafraichis l'image
    SDL.fillRect scr (Just (Rect 0 0 500 640)) (Pixel 0x000000)
    player <- getImage "player"
    Resources.displaySurface player scr (x - 16) (y - 25)
    return ()

patt = Simple fx fy fr 0 60000 "ball"
       
       where fx t = 300.0
             fy t = 300.0
	     fr t = 12.0

-- Boucle gère les contrôles du joueur
loop :: UTCTime -> IO ()
loop t0 =
  do
    evt <- SDL.pollEvent
    case evt of
      Quit                     -> exitWith ExitSuccess
      KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
      KeyDown (Keysym sym _ _) -> manageKeyDown sym
      KeyUp (Keysym sym _ _)   -> manageKeyUp sym
      _                        -> return ()
    
    -- Modifie les coordonnées en fonction de la vitesse (ralenti si appuis sur Shift)
    mod <- getModState
    if KeyModLeftShift `elem` mod
    then modifyIORef' joueur (setPosition 1)
    else modifyIORef' joueur (setPosition 3)
    
    -- Affiche le perso, attend 20ms, et boucle
    t1 <- getCurrentTime
    let t = truncate $ (diffUTCTime t1 t0) * 1000

    scr <- SDL.getVideoSurface

    blitPerso
    displayBullets scr t [patt]
    SDL.flip scr

    wait 20
    
    player <- readIORef joueur

    case playerBulletCollision (fromIntegral t) [patt] player of
      True  -> return ()
      False -> loop t0

    where
      -- L'utilisateur a appuyé sur une touche
      manageKeyDown sym =
        case sym of
          SDLK_UP    -> setDirUp True
          SDLK_DOWN  -> setDirDown True
          SDLK_LEFT  -> setDirLeft True
          SDLK_RIGHT -> setDirRight True
          SDLK_w     -> do
            playPlayerBullet           
            setFiring True
          SDLK_x     -> playSpellCard
          _          -> return ()

      -- L'utilisateur a relaché une touche
      manageKeyUp sym =
        case sym of
          SDLK_UP    -> setDirUp False
          SDLK_DOWN  -> setDirDown False
          SDLK_LEFT  -> setDirLeft False
          SDLK_RIGHT -> setDirRight False
          SDLK_w     -> do
            haltChannel 2
            setFiring False
          _          -> return ()

-- Le joueur
joueur :: IORef Player
joueur = unsafePerformIO $ newIORef (Player False (False, False, False, False) (100, 100) 0 0 0)

-- Donnée définissant les directions
data Dir = UP | DOWN | LEFT | RIGHT deriving (Eq)

-- Fonction qui modifie le tuple 'dir' d'un joueur en fonction de d et b
setDirection :: Dir -> Bool -> Player -> Player
setDirection d b (p@(Player isf (up, down, left, right) pos pow bomb l))
  | d == UP    = Player isf (b, down, left, right) pos pow bomb l
  | d == DOWN  = Player isf (up, b, left, right) pos pow bomb l
  | d == LEFT  = Player isf (up, down, b, right) pos pow bomb l
  | d == RIGHT = Player isf (up, down, left, b) pos pow bomb l
  | otherwise  = p

-- FONCTIONS DE MODIFICATION DE DIRECTION
setDirUp :: Bool -> IO ()
setDirUp v = modifyIORef' joueur (setDirection UP v)

setDirDown :: Bool -> IO ()
setDirDown v = modifyIORef' joueur (setDirection DOWN v)

setDirLeft :: Bool -> IO ()
setDirLeft v = modifyIORef' joueur (setDirection LEFT v)

setDirRight :: Bool -> IO ()
setDirRight v = modifyIORef' joueur (setDirection RIGHT v)

setFiring :: Bool -> IO ()
setFiring v = modifyIORef' joueur (\(Player _ b c d e f) -> (Player v b c d e f))

-- Fonction calculant les coordonnées d'un joueur en fonction de ses coord actuelles et de sa direction
setPosition :: Int -> Player -> Player
setPosition v pl@(Player q r@(h, b, g, d) (ox, oy) m o p)
  | (h == True) && oy <= 15  = pl
  | (b == True) && oy >= 618 = pl
  | (g == True) && ox <= 12  = pl
  | (d == True) && ox >= 485 = pl
  | otherwise                = Player q r (ox+x, oy+y) m o p

  where
    y = if (h == b) then 0 else if h then (-v) else v
    x = if (g == d) then 0 else if g then (-v) else v

