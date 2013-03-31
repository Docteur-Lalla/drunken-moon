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

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as IMG

import qualified Data.ByteString as BS
import Data.ByteString.Char8 as BSC

-- Ouvre le fichier rc/score et y lit les scores. La ByteString obtenue est coupée à chaque retour à la ligne.

getScoreLines :: IO [BS.ByteString]
getScoreLines = do
		  contents <- BS.readFile "rc/score"
		  return (BSC.split '\n' contents)
		  
		  
		  
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
			
				lines <- getScoreLines
				
				suika <- IMG.load "rc/images/Suika.jpeg"
				SDL.fillRect screen Nothing pixel
				SDL.blitSurface suika Nothing screen (Just (Rect x y 500 640))
				
				font <- Font.dejavu 15
				text <- TTF.renderTextBlended font "** SCORES **" noir
				SDL.blitSurface text Nothing screen (Just (Rect 180 120 0 0))
				
				forEach lines dispLine 0
				
				where
					pixel = Pixel 0xFFFFFF
					
					x = 500 - 333
					y = 640 - 327
					
					forEach [] _ _     = return ()
					forEach (x:xs) f i = do
						f x i
						forEach xs f (i+1)
					
					dispLine l i = do
					
						font <- Font.dejavu 9
						text <- TTF.tryRenderTextBlended font (unpack l) noir
						
						case text of
							Nothing -> return ()
							Just t  -> do
								SDL.blitSurface t Nothing screen (Just (Rect 150 (150+i*10) 0 0))
								return ()
						
-- Enregistre un nouveau score.

writeScore :: Int -> IO ()
writeScore n = Prelude.putStrLn "Nyu !"

-- Demande à l'utilisateur de rentrer son pseudo

askUsername screen = aux ""
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
					SDLK_BACKSPACE -> aux (rmlst str "")
					SDLK_SPACE -> aux (str++" ")
					_ -> if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
						then aux (str++[c])
						else aux str
					
					where
						rmlst [] _ = []
						rmlst (x:[]) buf = buf
						rmlst (x:xs) buf = rmlst xs (buf++[x])
						
				display screen = do
				
					SDL.fillRect screen Nothing (Pixel 0xFFFFFF)
					font <- Font.dejavu 15
					text <- TTF.tryRenderTextBlended font str noir
					case text of
						Nothing -> return ()
						Just t -> do 
							SDL.blitSurface t Nothing screen (Just (Rect 100 100 0 0))
							return ()
					
