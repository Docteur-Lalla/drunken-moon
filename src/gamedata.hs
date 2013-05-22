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

module GameData where

import Data.IORef
import System.IO.Unsafe
import Data.List as List

type TimeFunction = Float -> Float

-- Le joueur est représenté par sa position, la direction de son déplacement, ses vies, ses bombes et s'il tire.
data Player = Player { isFiring  :: Bool
                      ,isBombing :: Bool
                      ,dir       :: (Bool, Bool, Bool, Bool)
                      ,pos       :: (Int, Int)
                      ,power     :: Int
                      ,bombs     :: Int
                      ,lives     :: Int 
                     }

-- Un ennemi est représenté par sa position dans le temps, son sprite et sa vie.
data Ennemy = Ennemy { pos_x  :: TimeFunction
                      ,pos_y  :: TimeFunction
		      ,sprite :: String
		      ,life   :: Int
		     }

-- Droite symbolisant le côté gauche du cône de tir du joueur.
firingFunction :: Player -> Float -> Maybe Float
firingFunction (Player _ _ _ (x,y) _ _ _) = \t -> if t < fy then Just (3.0 * t + fx) else Nothing

  where fx = fromIntegral x
        fy = fromIntegral y

-- Droite symbolisant le côté droit du cône de tir du joueur.
reversedFiringFunction :: Player -> Float -> Maybe Float
reversedFiringFunction (Player _ _ _ (x,y) _ _ _) = \t -> if t < fy then Just (500 - fx - 3.0 * t) else Nothing

  where fx = fromIntegral x
        fy = fromIntegral y

-- Prédicat vérifiant si un ennemi placé en (ex, ey) est touché par le tir.
collisionFireEnnemy :: Player -> Float -> Ennemy -> Bool
collisionFireEnnemy player t (Ennemy ex ey _ _) =
  case (test2 (ex t) (reversedFiringFunction player (ey t)), test (firingFunction player (ey t)) (ex t)) of
    (True, True) -> True
    (_, _)       -> False

  where test Nothing  _  = False
        test (Just x) ex = x < ex

	test2 _ Nothing   = False
	test2 ex (Just x) = x > ex

-- Vérifie si un ennemi est touché et retranche 1 à sa vie.
manageFireEnnemyCollision :: Player -> Float -> [Ennemy] -> [Ennemy]
manageFireEnnemyCollision pl t []                      = []
manageFireEnnemyCollision pl t (e@(Ennemy x y s l):es) =
  if collisionFireEnnemy pl t e
    then (Ennemy x y s (l - 1)) : manageFireEnnemyCollision pl t es
    else e : manageFireEnnemyCollision pl t es

-- Elimine les ennemis ayant 0 de vie.
killEnnemies :: [Ennemy] -> [Ennemy]
killEnnemies []                    = []
killEnnemies ((Ennemy _ _ _ 0):es) = killEnnemies es
killEnnemies (e:es)                = e : killEnnemies es

-- Score lié au nombre d'ennemis tués.
ennemyScore :: [Ennemy] -> [Ennemy] -> Int
ennemyScore startE actualE = List.length startE - List.length actualE

-- Définition d'une référence représentant le joueur.
playerRef :: IORef Player
playerRef = unsafePerformIO $ newIORef (Player False False (False, False, False, False) (250, 350) 0 3 0)

-- Fonction recréant un joueur tel qu'il est au démarrage du jeu.
resetPlayer :: IO ()
resetPlayer = modifyIORef' playerRef reset
  where reset pl = (Player False False (False, False, False, False) (250, 350) 0 3 0)

-- Donnée définissant les directions.
data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq)

-- Fonction qui modifie le tuple 'dir' du joueur en fonction de la direction et l'état choisi.
setDirection :: Direction -> Bool -> Player -> Player
setDirection dir bool (Player e z f g h i j) = Player e z (screenCollisions g (newDirs dir bool f)) g h i j
  where
    newDirs dir bool (a, b, c, d) =
      case dir of
        UP    -> (bool, b, c, d)
        DOWN  -> (a, bool, c, d)
        LEFT  -> (a, b, bool, d)
        RIGHT -> (a, b, c, bool)

-- Cette fonction gère les collisions avec les bords de l'écran, et empèche le joueur d'en sortir
screenCollisions (x, y) (a, b, c, d) = (e, f, g, h)
  where
    e = if y <= 15  then False else a
    f = if y >= 618 then False else b
    g = if x <= 12  then False else c
    h = if x >= 485 then False else d

-- FONCTIONS DE MODIFICATION DE DIRECTION.
setDirUp :: Bool -> IO ()
setDirUp v = modifyIORef' playerRef (setDirection UP v)

setDirDown :: Bool -> IO ()
setDirDown v = modifyIORef' playerRef (setDirection DOWN v)

setDirLeft :: Bool -> IO ()
setDirLeft v = modifyIORef' playerRef (setDirection LEFT v)

setDirRight :: Bool -> IO ()
setDirRight v = modifyIORef' playerRef (setDirection RIGHT v)

-- Fait tirer/lancer une bombe au personnage
setFiring :: Bool -> IO ()
setFiring v = modifyIORef' playerRef (\(Player _ a b c d e f) -> (Player v a b c d e f))

setBombing :: Bool -> IO ()
setBombing v = modifyIORef' playerRef (\(Player a _ b c d e f) -> (Player a v b c d e f))

-- change le montant de bombes du joueur
changeBombAmount :: Int -> IO ()
changeBombAmount v = modifyIORef' playerRef (\(Player a b c d e _ f) -> (Player a b c d e v f))

-- Fonction calculant les coordonnées du joueur en fonction de ses coordonnées actuelles et de sa direction.
setPosition :: Int -> Player -> Player
setPosition v pl@(Player q z r@(h, b, g, d) (ox, oy) m o p) = Player q z (screenCollisions newpos r) newpos m o p
  where
    y = if (h == b) then 0 else if h then (-v) else v
    x = if (g == d) then 0 else if g then (-v) else v

    newpos = (ox+x, oy+y)
