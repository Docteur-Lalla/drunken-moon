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
import System.Exit
import System.IO
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Image as IMG
import qualified Data.List as List
import Data.Char

-- Ouvre le fichier rc/score et y lit les scores. La String obtenue est coupée à chaque retour à la ligne.
scoreDir = "rc/score"

getScoreLines :: IO [String]
getScoreLines = withFile scoreDir ReadMode $ (\file -> do
	contents <- hGetContents file
	if null contents then return []
	else return (lines contents))
		  
		  
		  
-- Affiche les scores enregistrés.
showScores :: Surface -> ImageEnvironment -> IO ()
showScores screen env = do
		displayShowScores screen env
		SDL.flip screen
		
		event <- SDL.waitEvent
		case event of
			Quit                     -> exitWith ExitSuccess
			KeyDown (Keysym sym _ _) -> manageKey sym
			_                        -> reloop
		
		where reloop = showScores screen env
			
		      manageKey key = case key of
					SDLK_q -> return ()
					_   -> reloop

title :: Surface -> IO ()
title screen = do
                 font <- Font.vera 28
		 TTF.setFontStyle font [ StyleBold ]

                 Font.renderCenteredText font "Scores" color screen 35

		 where color = Color 0x00 0x00 0x00

-- Affiche l'écran des scores.
displayShowScores :: Surface -> ImageEnvironment -> IO ()
displayShowScores scr env = do
	SDL.fillRect scr Nothing pblanc
	let suika = getImage env "suika"
	Resources.displaySurface suika scr x y

        title scr
	displayScoreLines scr
	
	where pblanc = Pixel 0xFFFFFF
					
	      x = 500 - 333
	      y = 640 - 327

compareScores :: (String, String) -> (String, String) -> Ordering
compareScores (_, n1) (_, n2) = compare (read n1 :: Int) (read n2 :: Int)

--Affiche les scores dans l'ordre décroissant.
displayScoreLines :: Surface -> IO ()
displayScoreLines scr = do
                          lines <- getScoreLines
			  
			  -- Chaque entrée de la liste est de la forme (pseudo, score).
			  scores' <- return (toStringPair $ map words lines)
			  let scores = List.reverse $ List.sortBy compareScores scores' -- Trie les scores.

			  let wm = getMaxWidth (scores ++ [minlen] ) -- La taile de ligne la plus longue.
			  formatted <- return $ fillLines wm scores

			  -- Affichage des scores avec la police à largeur fixe VeraMono.
			  font <- Font.veramono 12
			  (char_width, _) <- TTF.textSize font " "

			  Font.renderAlignedText font formatted Font.black scr (coords char_width wm) 10

			  where getMaxWidth :: [ (String, String) ] -> Int
			        getMaxWidth [] = 0
				getMaxWidth ((p, n):xs) = let len = List.length (p ++ n)
							  in if len > next
							    then len
							    else next

							  where next = getMaxWidth xs

				minlen = (replicate 20 ' ', replicate 30 ' ') -- Taille minimale (20 et 30 chars).

				-- Crée les lignes à afficher.
				fillLines :: Int -> [ (String, String) ] -> [ String ]
				fillLines _ [] = []
				fillLines len ((p, n):xs) = let diff = len - List.length (p ++ n) - 1
				                            in let format = (p ++ replicate diff ' ' ++ n)

							    in format : fillLines len xs

				-- Coordonnées d'affichage de la première ligne de score.
				coords cw wm = let size = wm * cw
				                 in (quot (500 - size) 2, 150)

				toStringPair :: [[String]] -> [ (String, String) ]
				toStringPair [] = []
				toStringPair ((n:p:_):xs) = (p, n) : toStringPair xs

--Enregistre un nouveau score.
-- Etats de la fonction (menu | écrire | accepter pseudo | quitter).
data State
	= Writing String
	| Menu Int
	| Save
	| Abort
	deriving (Eq)
	
writeScore = writeScore' "" (Writing "")

-- writeScore' :: Ecran -> Environnement -> Score -> Nom -> Etat -> IO ()
writeScore' :: String -> State -> Surface -> ImageEnvironment -> Int -> IO ()
writeScore' name state scr env score = do

	-- Affiche le menu de sauvegarde de score.
	displayWriteScore scr env state score name
	SDL.flip scr

	-- Agit en fonction de l'état actuel.
	case state of
	  Menu c -> reloopMenu c
	  Writing _ -> reloopWriting
	  Save -> saveScore score name
	  Abort -> return ()
		
	where reloopMenu c = do
			       ret <- manageMenuEvent c
			       writeScore' name ret scr env score
			
	      reloopWriting = do
			        ret <- manageWritingEvent name
			        case ret of
				  Writing newname -> writeScore' newname ret scr env score
				  Menu c -> writeScore' name ret scr env score

-- Affiche l'écran, en fonction de l'état (soit on écrit, soit on est sur le menu).
displayWriteScore :: Surface -> ImageEnvironment -> State -> Int -> String -> IO ()
displayWriteScore scr env state score name = do
	SDL.fillRect scr Nothing pblanc
	displaySurface (getImage env "suika") scr x y
	
	-- Petit message et menu si on y est, sinon pas de menu.
	fontMsg <- vera 18
	case state of
		Writing _ -> Font.renderCenteredText fontMsg "Entrez votre pseudo." black scr 20
		Menu c -> do
			Font.renderCenteredText fontMsg "Que faites vous?" black scr 20
			displayWritingMenu scr env c
		_ -> return ()
			
	-- Affiche le score et le pseudo.
	fontScore <- dejavu 15
	Font.renderAlignedText fontScore scoreetnom black scr (150, 50) 5
	
	where pblanc = Pixel 0xFFFFFF
	      scoreetnom = ["Score : "++(show score), "Pseudo : "++name]
		
	      x = 500 - 333
	      y = 640 - 327
		
-- Affiche le menu.
displayWritingMenu :: Surface -> ImageEnvironment -> Int -> IO ()
displayWritingMenu scr env c = do	
	font' <- Font.dejavu 14
	TTF.setFontStyle font' [ StyleBold ]
	
	Font.renderAlignedText font' choices black scr (80, 100) 10
	displaySelector font' 10 c scr env

	where choices = ["Renommer", "Sauvegarder", "Quitter"]
		
-- Affiche le sélecteur du menu.
displaySelector :: Font -> Int -> Int -> Surface -> ImageEnvironment -> IO ()
displaySelector f step c scr env = do
                                 (_, h) <- TTF.textSize f "Nyu"

                                 let select = getImage env "sun"
				 Resources.displaySurface select scr x (y step h)
				 
				 return ()

				 where position step h = Rect x (y step h) 500 640
				       x = 20
				       y step h = 100 + (step + h) * c - 20

-- gère les évènements du menu.
manageMenuEvent :: Int -> IO (State)
manageMenuEvent c = do
	event <- SDL.waitEvent
	case event of
		Quit -> exitWith ExitSuccess
		KeyDown (Keysym sym _ _) -> manageKey sym
		_ -> manageMenuEvent c
		
	where manageKey sym = case sym of
	 		        SDLK_UP -> if c <= 0 then return $ Menu 2 else return $ Menu (c-1)
			        SDLK_DOWN -> if c >= 2 then return $ Menu 0 else return $ Menu (c+1)
			        SDLK_RETURN -> case c of
				                 0 -> return (Writing "")
			 	                 1 -> return Save
				                 2 -> return Abort
				                 _ -> manageMenuEvent c
			        _ -> manageMenuEvent c

-- Gère les évènementslors de la saisie du pseudo.
manageWritingEvent :: String -> IO (State)
manageWritingEvent name = do
	event <- SDL.waitEvent
	case event of
		Quit -> exitWith ExitSuccess
		KeyDown (Keysym sym _ c) -> return $ manageKey sym c
		_ -> manageWritingEvent name
		
	where manageKey sym c = case sym of
	 		          SDLK_RETURN -> Menu 0
			          SDLK_BACKSPACE -> Writing (rmlst name)
			          _ -> if isAlphaNum c then Writing (name++[c]) else Writing name
			
	      rmlst [] = []
	      rmlst [x] = []
	      rmlst (x:xs) = x : (rmlst xs)

-- Sauvegarde le score dans le fichier.
saveScore :: Int -> String -> IO ()
saveScore a b = appendFile scoreDir ((show a)++" "++b)
