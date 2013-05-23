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
import Font
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
import Data.Word
import Data.Maybe
import System.IO.Unsafe

-- Calcul le score actuel du joueur.
totalScore :: Int -> Int -> Player -> Int
totalScore graze ennemies (Player _ _ _ (x, y) _ b l) = 100 * (graze + ennemies) + 10000 * (l + b)

-- Crée une partie.
newGame :: Surface -> IO ()
newGame scr =
  do
    enableKeyRepeat 0 0

    generalLoop scr 1 StartStage
    return ()

-- Affiche le personnage à l'écran et son cône de tir si celui-ci est nécessaire.
displayPlayer :: Surface -> IO ()
displayPlayer scr =
  do
    -- Récupère les coordonnées.
    p@(Player _ _ _ (x, y) _ _ _) <- readIORef playerRef
    
    -- Affiche le personnage et rafraichit l'image.
    SDL.fillRect scr (Just (Rect 0 0 500 640)) (Pixel 0x000000)
    player <- getImage "player"
    Resources.displaySurface player scr (x - 16) (y - 25)

    if (isFiring p) == False
      then return ()
      else displayFire scr p

displayEnnemies :: Float -> Surface -> [Ennemy] -> IO ()
displayEnnemies _ _   []                         = return ()
displayEnnemies t scr ((Ennemy fx fy skin _):es) =
  do
    sprite <- getImage skin
    let x = truncate $ fx t
    let y = truncate $ fy t

    Resources.displaySurface sprite scr x y
    displayEnnemies t scr es

displayFire :: Surface -> Player -> IO ()
displayFire scr p@(Player _ _ _ (x,y) _ _ _) =
  do
    fire <- getImage "fire"
    let posx = x - 200

    Resources.displaySurface fire scr posx y
    return ()

-- Affiche le score du joueur, ses vies et ses bombes
displayHUD :: Int -> Int -> Player -> Surface -> IO ()
displayHUD g e p scr =
  do
    --Affiche le score aligné à droite
    fonte <- veramono 12
    renderRightText fonte (show $ totalScore g e p) white scr (495, 400)

    displayLives (lives p) scr
    displayBombs (bombs p) scr

-- Affiche les vies sous forme de petits coeurs
displayLives :: Int -> Surface -> IO ()
displayLives 0 _ = return ()
displayLives l scr =
  do
    coeur <- getImage "heart_icon"

    -- Recupère la largeur de l'image
    (Rect _ _ w _) <- getClipRect (fromJust coeur)

    -- affiche les coeurs à la suite avec un décalage de 5px entre chaque coeur
    displaySurface coeur scr (x' w l) y'

    displayLives (l-1) scr
  where
    x' w l = 5 + (5 + w) * (l - 1)
    y' = 400

-- Affiche les bombes sous forme de petites etoiles
displayBombs :: Int -> Surface -> IO ()
displayBombs 0 _ = return ()
displayBombs b scr =
  do
    bombe <- getImage "bomb_icon"
    (Rect _ _ w h) <- getClipRect (fromJust bombe)

    displaySurface bombe scr (x' w b) (y' h)
    displayBombs (b - 1) scr  
  
  where
    x' w b = 5 + (5 + w) * (b - 1)
    y' h = 400 + 5 + h

data GameState
  = Finished
  | Dead
  -- | Leave Pourquoi pas un abandon de la partie ?
  | StartStage
  | StageComplete

-- Variable définissant le dernier niveau ... pour l'instant 1 ...
lastLvl = 1

-- Boucle générale, gère les différentes étapes du jeu (fadein/out entre les stages, mort, fin du jeu, abandon)
generalLoop :: Surface -> Int -> GameState -> IO (GameState)
generalLoop scr lvl state =
  do
    
    {-
      Agis en fonction de l'état 'state':
        - StartStage -> lance le prochain niveau (lvl définit quel niveau lancer)
        - StageComplete -> quand le niveau est fini, on test si c'était le dernier
          si oui, on a fini le jeu sinon on lance le prochain stage
        - Finished/Dead/Leave -> Fin du jeu, soit on l'a fini, soit on est mort, soit on a abandonné 
          (retourne à newGame, qui va agir en fonction de la façon dont le jeu s'est terminé)
    -}
    
    case state of
      StartStage    -> start lvl
      StageComplete -> if lvl == lastLvl 
                       then return Finished
                       else next lvl
      x             -> do
                         startBGM "main_theme"
                         fadeOut scr
                         return x
       
  where
    -- start effectue un fondu en faisant apparaitre le niveau, puis lance loop (le niveau en lui même)
    start lvl =
      do
      
        fadeIn scr
        LV1.back_music
        t0 <- getCurrentTime
        st <- loop t0 LV1.run LV1.ennemies 0 0
        generalLoop scr lvl st
    
    -- next va noircir l'écran, pusi lancer le stage suivant
    next lvl =
      do
        fadeOut scr
        generalLoop scr (lvl+1) StartStage

-- Fonctions de fadeIn/fadeOut de l'écran entre chaque niveau
-- fadeIn -> effectue un fondu faisant apparaitre le niveau
fadeIn :: Surface -> IO ()
fadeIn scr = fadeAux 255
  where
    fadeAux :: Int -> IO ()
    fadeAux x = if x <= 0 then return ()
    else
      do

        -- Creation du filtre transparent
        f <- createRGBSurface [HWSurface] 500 640 32 0xFF000000 0xFF0000 0xFF00 0

        -- Coloration du filtre en blanc
        c <- mapRGB (surfaceGetPixelFormat f) 0xFF 0xFF 0xFF
        SDL.fillRect f screenRect c

        -- Affichage du background
        SDL.fillRect scr screenRect (Pixel 0x000000)
        displayPlayer scr

        -- Ajustement de la transparence du filtre
        setAlpha f [SrcAlpha] (fromInteger (toInteger x) :: Word8)

        -- Affichage du filtre
        displaySurface (Just f) scr 0 0

        -- Rafraichissement de l'écran
        SDL.flip scr

        -- Gestion stable des fps et de la musique
        wait 20

        -- On recommence avec une opacité moins élevée
        fadeAux (x-3)
      where
        screenRect = (Just (Rect 0 0 500 640))

-- Fonction inverse de fadeIn -> cache l'écran
fadeOut :: Surface -> IO ()
fadeOut scr =  fadeAux 0
  where
    fadeAux :: Int -> IO ()
    fadeAux x = if x >= 255 then return ()
    else
      do
        f <- createRGBSurface [HWSurface] 500 640 32 0xFF000000 0xFF0000 0xFF00 0

        c <- mapRGB (surfaceGetPixelFormat f) 0xFF 0xFF 0xFF
        SDL.fillRect f screenRect c

        SDL.fillRect scr screenRect (Pixel 0x000000)
        displayPlayer scr

        setAlpha f [SrcAlpha] (fromInteger (toInteger x) :: Word8)

        displaySurface (Just f) scr 0 0

        SDL.flip scr

        wait 20

        fadeAux (x+3)
      where
        screenRect = (Just (Rect 0 0 500 640))

-- Boucle gérant les contrôles du joueur.
loop :: UTCTime -> [Pattern] -> [Ennemy] -> Int -> Int -> IO (GameState)
loop t0 patts ennemies graze escore =
  do 
    evt <- SDL.pollEvent
    
    {-
      Gestion des evenements clavier

      La pause est gérée par l'appui de la touche Echap
      on récupère donc le temps de la pause (si pause il y a eu) ici.
    -}
    tpause <- case evt of
      Quit                     -> exitWith ExitSuccess -- Gestion de la fermeture (appui sur 'q').
      KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess -- Gestion de la fermeture (clic sur la croix).
      KeyDown (Keysym sym _ _) -> manageKeyDown sym
      KeyUp (Keysym sym _ _)   -> do
        manageKeyUp sym
        return 0
      _                        -> return 0
    
    -- Modifie les coordonnées en fonction de la vitesse (ralenti si appuis sur Shift/Majuscule).
    mod <- getModState
    if KeyModLeftShift `elem` mod
    then modifyIORef' playerRef (setPosition 3)
    else modifyIORef' playerRef (setPosition 6)

    -- Ajoute le temps de la pause et le temps de début du niveau.
    let t0' = addUTCTime tpause t0
    
    -- Affiche le personnage, attend 20ms, et boucle.
    t1 <- getCurrentTime
    let t = truncate $ (diffUTCTime t1 t0') * 1000

    -- Teste les collisions entre le cône de tir et les ennemis.
    player <- readIORef playerRef
    let new_ennemies = if isFiring player
                         then killEnnemies $ manageFireEnnemyCollision player (fromIntegral t) ennemies
                         else ennemies
    
    -- Teste le fait qu'on utilise une bombe ou non, et détruit les bullets pris dans la bombe
    new_patterns <- if isBombing player
                    then do
                      setBombing False
                      if (bombs player) > 0
                        then do
                          playSpellCard
                          changeBombAmount ((bombs player) - 1)
                          return (applyBomb t patts)
                        else return patts
                    else return patts
    
    -- Actualise le score des enemis tués et le score de graze
    let new_graze = grazeCount (fromIntegral t / 1000.0) new_patterns player + graze
    let new_escore = (ennemyScore ennemies new_ennemies) + escore

    scr <- SDL.getVideoSurface
    
   
    -- Affichage du personnage, des projectiles, du HUD et mise à jour de l'écran.
    displayPlayer scr
    displayEnnemies (fromIntegral t) scr new_ennemies
    displayBullets scr t patts
    
    displayHUD new_graze new_escore player scr

    SDL.flip scr

    -- Pause stratégique.
    wait 20

    -- Ne reboucle que si la hitbox du personnage est sauve.
    case playerBulletCollision (fromIntegral t) patts player of
      True  -> do
        die
        changeBombAmount 3
        playPlayerDie
        if (lives player) > 0
          then loop t0' (cleanBulletList t new_patterns) new_ennemies new_graze new_escore
          else return (Dead)
      False -> loop t0' (cleanBulletList t new_patterns) new_ennemies new_graze new_escore

    where
      {-
        L'utilisateur a appuyé sur une touche
        
        Comme cette fonction gère le fait qu'on entre en pause
        elle va renvoyer une différence de temps (le temps de la pause)
        si on sort d'une pause tp = temps de la pause
        sinon, tp = 0 car on a pas effectué de pause

      -}
      manageKeyDown :: SDLKey -> IO (NominalDiffTime)
      manageKeyDown sym = do
        if sym == SDLK_ESCAPE
        then getCurrentTime >>= gamePause -- Lance la pause
        else do
          case sym of
            SDLK_UP     -> setDirUp True
            SDLK_DOWN   -> setDirDown True
            SDLK_LEFT   -> setDirLeft True
            SDLK_RIGHT  -> setDirRight True
            SDLK_w      -> do
              playPlayerBullet
              setFiring True -- Le joueur est en train de tirer.
            SDLK_x      -> setBombing True
            _           -> return ()
          return 0

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

{-
  gère la pause pendant une partie
  on quitte la pause à l'appui de n'importe quelle touche
-}
gamePause :: UTCTime -> IO (NominalDiffTime)
gamePause t0 =
  do
    evt <- SDL.pollEvent
    case evt of
      KeyDown _ -> do
        t1 <- getCurrentTime
        return (diffUTCTime t1 t0)
      _         -> do
        wait 20
        gamePause t0
