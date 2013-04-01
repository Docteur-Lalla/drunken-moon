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

module Score (showScores, writeScore, askUsername) where

import Font

import System.IO
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as IMG

import Data.List as LIST

-- Ouvre le fichier rc/score et y lit les scores. La ByteString obtenue est coupée à chaque retour à la ligne.

getScoreLines :: IO [String]
getScoreLines = do
		withFile "rc/score" ReadMode (\file -> do
			contents <- hGetContents file
			if (LIST.null contents)
			then return []
			else return (lines contents))
				
		  
		  
-- Affiche les scores enregistrés.

showScores :: Surface -> IO ()
showScores screen = do

		display screen
		SDL.flip screen
		
		evt <- SDL.waitEvent
		case evt of
			Quit                     -> return ()
			KeyDown (Keysym sym _ _) -> manageKey sym
			_                        -> reloop
		
		where
			reloop = showScores screen
			
			manageKey key =
				case key of
					SDLK_q -> return ()
					_   -> reloop
					
			display screen = do
			
				line <- getScoreLines
				
				suika <- IMG.load "rc/images/Suika.jpeg"
				SDL.fillRect screen Nothing pixel
				SDL.blitSurface suika Nothing screen (Just (Rect x y 500 640))
				
				font <- Font.dejavu 15
				text <- TTF.renderTextBlended font "** SCORES **" noir
				SDL.blitSurface text Nothing screen (Just (Rect 200 120 0 0))
				
				forEach line dispLine 0
				
				where
					pixel = Pixel 0xFFFFFF
					
					x = 500 - 333
					y = 640 - 327
					
					forEach [] _ _     = return ()
					forEach (x:xs) f i = do
						f x i
						forEach xs f (i+1)
					
					dispLine l i = do
						if (LIST.null l)
						then return ()
						else do
							font <- Font.dejavu 12

							(w, h) <- TTF.textSize font score
							tscore <- TTF.renderTextBlended font (score) noir
							tname <- TTF.renderTextBlended font (nom) noir
							
							SDL.blitSurface tname Nothing screen (Just (Rect 150 (150+i*13) 0 0))
							SDL.blitSurface tscore Nothing screen (Just (Rect (350 - w) (150+i*13) 0 0))
							return ()
							
							where
								ligne = LIST.words l
								nom = LIST.last ligne
								score = LIST.head ligne
						
-- Enregistre un nouveau score.

writeScore :: Int -> IO ()
writeScore n = Prelude.putStrLn "Nyu !"

-- Demande à l'utilisateur de rentrer son pseudo

askUsername screen = do 
	SDL.enableKeyRepeat 500 20
	aux ""
	where
		aux str = do
			
			display screen
			SDL.flip screen
			
			evt <- SDL.waitEvent
			case evt of
				Quit                     -> return str --pas certain mais surement utiliser exitWith
				KeyDown (Keysym sym _ c) -> manageKey sym c
				_                        -> aux str

			where
				manageKey sym c = case sym of
					SDLK_RETURN -> return str
					SDLK_BACKSPACE -> aux (rmlst str)
					SDLK_SPACE -> aux (str++" ")
					_ -> if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
						then aux (str++[c])
						else aux str
						
					where
						rmlst [] = []
						rmlst a = LIST.init a
						
				display screen = do
					SDL.fillRect screen Nothing (Pixel 0xFFFFFF)
					font <- Font.dejavu 15
					text <- TTF.tryRenderTextBlended font str noir
					case text of
						Nothing -> return ()
						Just t -> do 
							SDL.blitSurface t Nothing screen (Just (Rect 100 100 0 0))
							return ()
					
