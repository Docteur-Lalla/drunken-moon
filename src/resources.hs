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

module Resources where

import Data.IORef
import Data.Maybe
import Data.List as L
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Mixer as MIX
import Graphics.UI.SDL.Image as IMG
import Data.Word

-- Type de donnée contenant une musique, la position de fin d'intro et la position de loop
type BGM = (Music, Double, Double)

-- Types de données pour gérer l'environnement
type Resource a = (String, a)
type Image = Resource Surface
type RMusic = Resource BGM
type Sound = Resource Chunk

type Environment dt = [Resource dt]
type ImageEnvironment = Environment Surface
type MusicEnvironment = Environment BGM
type SoundEnvironment = Environment Chunk


{-
  Variables nécéssaires au jeu telles que les images, les sons
-}

-- La liste de musiques
music_map :: IORef MusicEnvironment
music_map = unsafePerformIO $ newIORef []

-- La liste des sons
sound_map :: IORef SoundEnvironment
sound_map = unsafePerformIO $ newIORef []

-- La liste d'images
image_map :: IORef ImageEnvironment
image_map = unsafePerformIO $ newIORef []

-- Fonction qui vérifie si une resource a un certain nom
hasName :: String -> Resource a -> Bool
hasName str (name,_) = str == name

-- Ajoute une resource chargée dans un environnement, si une resource
-- du même nom éxiste, l'environnement reste inchangé.
addRes :: Resource a -> IORef (Environment a) -> IO ()
addRes r@(name,_) env =
  do
    l <- readIORef env
    
    if (L.any (hasName name) l) then
      return ()
    else
      modifyIORef' env (r:)

-- Récupère une resource nommée 'name' dans un environment 'env'
getRes :: IORef (Environment a) -> String -> IO (Maybe a)
getRes env name =
  do
    l <- readIORef env
    return $ L.lookup name l



-- Charge une musique dans la mémoire sous la forme d'un BGM
addMusic :: String -> String -> Double -> Double -> IO ()
addMusic name src i r =
  do
    m <- MIX.loadMUS src
    addRes (name, (m, i, r)) music_map

-- Récupère une musique depuis la mémoire
getMusic :: String -> IO (Maybe BGM)
getMusic = getRes music_map

-- Charge une image dans la mémoire
addImage :: String -> String -> IO ()
addImage name src =
  do
    i <- (IMG.load src) >>= displayFormat
    addRes (name, i) image_map

-- Récupère une image depuis la mémoire
getImage :: String -> IO (Maybe Surface)
getImage = getRes image_map

-- Charge un son dans la mémoire
addSound :: String -> String -> IO ()
addSound name src =
  do
    s <- MIX.loadWAV src
    addRes (name, s) sound_map

-- Récupère un son depuis la mémoire
getSound :: String -> IO (Maybe Chunk)
getSound = getRes sound_map

-- Initialise l'environnement
initEnvironment :: IO ()
initEnvironment =
  do
    -- Ajoute les musiques
	addMusic "extra_stage_boss" "rc/musics/nightofnights.ogg" 24.75 27.4
	
	-- Ajoute les images
	addImage "suika" "rc/images/Suika.jpeg"
	addImage "sun" "rc/images/sun.jpg"
	
freeEnvironment :: IO ()
freeEnvironment =
  do
    fmap (map (\(_, (m,_,_)) -> MIX.freeMusic m)) (readIORef music_map)
    fmap (map (\(_, i) -> SDL.freeSurface i)) (readIORef image_map)
    return ()

-- Fonction pour afficher une surface plus facilement
displaySurface :: (Maybe Surface) -> Surface -> Int -> Int -> IO Bool
displaySurface Nothing _ _ _ = return False
displaySurface (Just src) dst x y = SDL.blitSurface src Nothing dst (Just (Rect x y 0 0))
