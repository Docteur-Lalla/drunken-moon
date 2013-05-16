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
import Reimu as LV1

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as MIx

import System.Exit
import Data.IORef
import Data.Time
import System.IO.Unsafe

-- Crée une partie.
newGame :: Surface -> IO ()
newGame scr =
  do
    enableKeyRepeat 0 0

    t0 <- getCurrentTime
    loop t0 LV1.run []

-- Affiche le personnage à l'écran et son cône de tir si celui-ci est nécessaire.
displayPlayer :: IO ()
displayPlayer =
  do
    -- Récupère l'écran.
    scr <- SDL.getVideoSurface
    -- Récupère les coordonnées.
    p@(Player _ _ (x, y) _ _ _) <- readIORef playerRef
    
    -- Affiche le personnage et rafraichit l'image.
    SDL.fillRect scr (Just (Rect 0 0 500 640)) (Pixel 0x000000)
    player <- getImage "player"
    Resources.displaySurface player scr (x - 16) (y - 25)

    if (isFiring p) == False
      then return ()
      else displayFire scr p

-- Affiche le cône de tir du joueur.
displayFire :: Surface -> Player -> IO ()
displayFire scr p@(Player _ _ (x,y) _ _ _) =
  do
    fire <- getImage "fire"
    let posx = x - 200 -- Ajuste la position du cône pour qu'il soit placer juste au dessus du joueur.

    Resources.displaySurface fire scr posx y
    return ()

-- Boucle gérant les contrôles du joueur.
loop :: UTCTime -> [Pattern] -> [Ennemy] -> IO ()
loop t0 patts ennemies =
  do
    evt <- SDL.pollEvent
    case evt of
      Quit                     -> exitWith ExitSuccess -- Gestion de la fermeture (appui sur 'q').
      KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess -- Gestion de la fermeture (clic sur la croix).
      KeyDown (Keysym sym _ _) -> manageKeyDown sym
      KeyUp (Keysym sym _ _)   -> manageKeyUp sym
      _                        -> return ()
    
    -- Modifie les coordonnées en fonction de la vitesse (ralenti si appuis sur Shift/Majuscule).
    mod <- getModState
    if KeyModLeftShift `elem` mod
    then modifyIORef' playerRef (setPosition 1)
    else modifyIORef' playerRef (setPosition 3)
    
    -- Affiche le personnage, attend 20ms, et boucle.
    t1 <- getCurrentTime
    let t = truncate $ (diffUTCTime t1 t0) * 1000
    
    -- Teste les collisions entre le cône de tir et les ennemis.
    player <- readIORef playerRef
    let new_ennemies = if isFiring player
                         then killEnnemies $ manageFireEnnemyCollision player (fromIntegral t) ennemies
                         else ennemies

    scr <- SDL.getVideoSurface

    -- Affichage du personnage, des projectiles et mise à jour de l'écran.
    displayPlayer
    displayBullets scr t patts
    SDL.flip scr

    -- Pause stratégique.
    wait 20

    -- Ne reboucle que si la hitbox du personnage est sauve.
    case playerBulletCollision (fromIntegral t) patts player of
      True  -> return ()
      False -> loop t0 (cleanBulletList t patts) new_ennemies

    where
      -- L'utilisateur a appuyé sur une touche.
      manageKeyDown sym =
        case sym of
          SDLK_UP    -> setDirUp True
          SDLK_DOWN  -> setDirDown True
          SDLK_LEFT  -> setDirLeft True
          SDLK_RIGHT -> setDirRight True
          SDLK_w     -> do
            playPlayerBullet           
            setFiring True -- Le joueur est en train de tirer.
          SDLK_x     -> playSpellCard
          _          -> return ()

      -- L'utilisateur a relaché une touche.
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
