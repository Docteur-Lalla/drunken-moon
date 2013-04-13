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

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as IMG

-- Types de données pour gérer l'environnement

newtype Res a = Res (String, a)
--type Sound = Res Music
--type SoundEnv = [Sound]

type Image = Res Surface
type ImgEnv = [Image]

type Env = ImgEnv

addImage :: Image -> ImgEnv -> ImgEnv
addImage (Res ("", _)) e = e
addImage i@(Res (n, s)) e
	| exists n e = i : (rmImage n e)
	| otherwise  = i : e
	
rmImage :: String -> ImgEnv -> ImgEnv
rmImage _ [] = []
rmImage "" e = e
rmImage id ((i@(Res (n, _))):xs)
	| n == id   = xs
	| otherwise = i:(rmImage id xs)

exists :: String -> ImgEnv -> Bool
exists _ [] = False
exists "" _ = False
exists id ((Res (n, _)):xs) = if n == id then True else exists id xs

getImage :: ImgEnv -> String -> Maybe Surface
getImage [] _ = Nothing
getImage _ "" = Nothing
getImage ((Res (n, s)):xs) id
	| id == n   = Just s
	| otherwise = getImage xs id
	
-- Initialise l'environnement

initEnv :: IO (Env)
initEnv = do
	suika <- IMG.load "rc/images/Suika.jpeg"
	sun <- IMG.load "rc/images/sun.jpg"
	
	let env = addImage (Res ("suika", suika)) $ addImage (Res ("sun", sun)) []
	return env

-- Fonction pour afficher une surface plus facilement
displaySurface :: (Maybe Surface) -> Surface -> Int -> Int -> IO Bool
displaySurface Nothing _ _ _ = return False
displaySurface (Just src) dst x y = SDL.blitSurface src Nothing dst (Just (Rect x y 0 0))
