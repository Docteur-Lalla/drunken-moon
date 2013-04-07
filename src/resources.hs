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

-- Gestion de l'environnement

type MapSurface = (String, Surface)
type Environment = [MapSurface]

getResource :: String -> Environment -> Maybe Surface
getResource id [] = Nothing
getResource id ((name, surface):xs)
	| id == name = Just surface
	| otherwise  = getResource id xs
	
exists :: String -> Environment -> Bool
exists _ [] = False
exists id ((name, _):xs)
	| id == name = True
	| otherwise  = exists id xs
	
addResource :: String -> Surface -> Environment -> Environment
addResource "" _ e = e
addResource id s e
	| exists id e = e
	| otherwise   = (id, s) : e

rmResource :: String -> Environment -> Environment
rmResource _ [] = []
rmResource id (x:xs)
	| id == (fst x) = rmResource id xs
	| otherwise     = x : rmResource id xs

updateResource :: String -> Surface -> Environment -> Environment
updateResource "" _ e = e
updateResource id s e
	| exists id e = (id, s) : new
	| otherwise = (id, s) : e
	where
		new = rmResource id e
			
initRes :: IO Environment
initRes = do
	suika <- IMG.load "rc/images/Suika.jpeg"
	sun <- IMG.load "rc/images/sun.jpg"
	
	
	return $ addResource "sun" sun $ addResource "suika" suika []
		
-- Pourquoi pas une de display ?

displaySurface :: Maybe Surface -> Surface -> Int -> Int -> IO Bool
displaySurface Nothing _ _ _ = return False
displaySurface (Just src) dst x y = SDL.blitSurface src Nothing dst (Just (Rect x y 0 0))
