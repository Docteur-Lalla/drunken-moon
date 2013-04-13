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

module Font where

import Resources
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

fontDir :: String
fontDir = "/usr/share/fonts/TTF/"

noir = Color 0 0 0
blanc = Color 0xFF 0xFF 0xFF

-- Vera, Liberation et DejaVu sont des polices de caractère.
-- Ces fonctions permettent d'ouvrir les-dites polices plus élégamment.

vera :: Int -> IO Font
vera n = TTF.openFont (fontDir ++ "Vera.ttf") n

liberation :: Int -> IO Font
liberation n = TTF.openFont (fontDir ++ "LiberationSerif-Regular.ttf") n

dejavu :: Int -> IO Font
dejavu n = TTF.openFont (fontDir ++ "DejaVuSerif.ttf") n

-- Affiche un texte centré.

renderCenteredText :: Font -> String -> Color -> Surface -> Int -> IO ()
renderCenteredText f s c scr y = do
					(x, _) <- TTF.textSize f s
					text <- TTF.tryRenderTextBlended f s c
					Resources.displaySurface text scr (centered'x x) y
					
					return ()
					where
						centered'x x = 250 - (quot x 2)

-- Police -> Lignes -> Couleur -> Ecran -> Coordonnées -> Espace entre chaque ligne -> Retour
renderAlignedText :: Font -> [String] -> Color -> Surface -> (Int, Int) -> Int -> IO ()
renderAlignedText _ [] _ _ _ _ = return ()
renderAlignedText f (s:ss) c scr (x, y) step = do
	(_, h) <- TTF.textSize f s
	text <- TTF.tryRenderTextBlended f s c
	Resources.displaySurface text scr x y

	renderAlignedText f ss c scr (x, new'y h) step
					       
	where new'y h = y + h + step -- position + pas + taille du texte.
