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

module Score (showScores, writeScore) where

import Font
import Resources

import System.IO
import System.Exit
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

showScores :: Surface -> Environment -> IO ()
showScores screen env = do

		display screen
		SDL.flip screen
		
		evt <- SDL.waitEvent
		case evt of
			Quit                     -> exitWith ExitSuccess
			KeyDown (Keysym sym _ _) -> manageKey sym
			_                        -> reloop
		
		where
			reloop = showScores screen env
			
			manageKey key =
				case key of
					SDLK_q -> return ()
					_   -> reloop
					
			display screen = do
			
				line <- getScoreLines
				
				let suika = getResource "suika" env
				SDL.fillRect screen Nothing pixel
				displaySurface suika screen x y
				
				font <- Font.dejavu 15
				text <- TTF.tryRenderTextBlended font "** SCORES **" noir
				displaySurface text screen 200 120
				
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

							(w, h) <- TTF.textSize font "|j"
							tscore <- TTF.tryRenderTextBlended font (score) noir
							tname <- TTF.tryRenderTextBlended font (nom) noir
							
							displaySurface tname screen 150 (150+i*13)
							displaySurface tscore screen (350 - w) (150+i*13)
							return ()
							
							where
								ligne = LIST.words l
								nom = LIST.last ligne
								score = LIST.head ligne

-- Etats du writeScore

data State
	= ChngName
	| ScoreMenu
	| Return
	deriving (Eq)

-- Enregistre un nouveau score.

writeScore = writeScore' "" ScoreMenu 0
writeScore' :: String -> State -> Int -> Surface -> Environment -> Int -> IO ()
writeScore' str state choix screen env score = do

	displayWriteScore screen env state str score choix
	
	evt <- SDL.waitEvent
	case state of
		ChngName -> do
			ret <- manageChngName str evt
			loopChngName ret
		ScoreMenu -> do
			ret <- manageScoreMenu choix evt
			loopScoreMenu ret
		Return -> return ()
			
		where
			loopChngName (st, s) = writeScore' s st choix screen env score
			loopScoreMenu (st, c) = writeScore' str st c screen env score
		
-- Affiche l'écan à la saisie du pseudo
	
displayWriteScore :: Surface -> Environment -> State -> String -> Int -> Int -> IO ()
displayWriteScore s env st name sc c = do
	dispBG s env
	dispNewScore s name sc
	
	if st == ScoreMenu
	then do
		dispScoreMenu s c
	else return ()	
	
	SDL.flip s
	
	return ()
	
-- Affiche l'arrière plan

dispBG :: Surface -> Environment -> IO ()
dispBG screen env = do
	SDL.fillRect screen Nothing (Pixel 0xFFFFFF)
	displaySurface (getResource "suika" env) screen 167 313
	return ()

-- Affiche le score et le pseudo

dispNewScore :: Surface -> String -> Int -> IO ()
dispNewScore screen name score = do
	font <- dejavu 15
	(w, h) <- TTF.textSize font "|j"
	tscore <- TTF.tryRenderTextBlended font ("Pseudo : "++name) noir
	tname <- TTF.tryRenderTextBlended font ("Score : "++(show score)) noir
	
	displaySurface tname screen 0 0
	displaySurface tscore screen 0 (h+5)
	
	return ()

-- Affiche le menu avec les choix "Accepter", "Changer Pseudo", et "Quitter"

dispScoreMenu :: Surface -> Int -> IO ()
dispScoreMenu screen choix = return ()

-- Gère les Eventsf pendant la phase de saisie du pseudo

manageChngName :: String -> Event -> IO (State, String)
manageChngName str evt = case evt of
	Quit -> exitWith ExitSuccess
	KeyDown (Keysym sym _ c) -> manageKey sym c
	_ -> manageChngName str evt

	where
		manageKey sym c = case sym of
			SDLK_RETURN -> return (ScoreMenu, str)
			SDLK_BACKSPACE -> return (ChngName, rmlst str)
			SDLK_SPACE -> return (ChngName, str++" ")
			_ -> if ((c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
				then return (ChngName, str++[c])
				else return (ChngName, str)
				
			where
				rmlst [] = []
				rmlst a = LIST.init a

-- Gère les Events au menu du writeScore

manageScoreMenu :: Int -> Event -> IO (State, Int)
manageScoreMenu c evt = case evt of
	Quit -> exitWith ExitSuccess
	KeyDown (Keysym sym _ _) -> manageKey sym
	_ -> manageScoreMenu c evt
	where
		manageKey sym = case sym of
			SDLK_UP -> if (c <= 0)
					then return (ScoreMenu, 2)
					else return (ScoreMenu, c-1)
			SDLK_DOWN -> if (c >= 2)
					then return (ScoreMenu, 0)
					else return (ScoreMenu, c+1)
			SDLK_RETURN -> case c of
					0 -> return (ChngName, c)
					1 -> return (Return, c)
					2 -> return (Return, c)
