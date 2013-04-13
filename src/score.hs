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
import qualified Data.List as LIST
import Data.Char

-- Ouvre le fichier rc/score et y lit les scores. La ByteString obtenue est coupée à chaque retour à la ligne.

scoreDir = "rc/score"
getScoreLines :: IO [String]
getScoreLines = withFile scoreDir ReadMode $ (\file -> do
	contents <- hGetContents file
	if null contents then return []
	else return (lines contents))
		  
		  
		  
-- Affiche les scores enregistrés.

showScores :: Surface -> Env -> IO ()
showScores screen env = do

		displayShowScores screen env
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
						
-- Affiche l'écran des scores
displayShowScores :: Surface -> Env -> IO ()
displayShowScores scr env = do
	SDL.fillRect scr Nothing pblanc
	let suika = getImage env "suika"
	Resources.displaySurface suika scr x y
	
	displayScoreLines scr
	
	where
		pblanc = Pixel 0xFFFFFF
					
		x = 500 - 333
		y = 640 - 327

--Affiche les scores dans l'ordre décroissant		
displayScoreLines :: Surface -> IO ()
displayScoreLines scr = do
	lines <- getScoreLines
	
	-- Trie le tableau de scores et affiche les lignes une par une
	let liste_trie = sortS $ map words lines
	forEach liste_trie dispLine 0
	
	where
		-- Trie le tableau
		sortS [] = []
		sortS (pivot@(s1:[_]):xs) = gauche ++ [pivot] ++ droite
			where
				gauche = sortS [x | x@(s2:[_]) <- xs, (read s2 :: Int) > (read s1 :: Int)]
				droite = sortS [x | x@(s2:[_]) <- xs, (read s2 :: Int) <= (read s1 :: Int)]
		
		-- Affiche une ligne avec l'offset 'i'
		dispLine (s:[n]) i = do
			font <- Font.dejavu 9
			tname <- TTF.tryRenderTextBlended font n noir
			tscore <- TTF.tryRenderTextBlended font s noir
			
			(w, h) <- TTF.textSize font s
			Resources.displaySurface tname scr xn (y h i)
			Resources.displaySurface tscore scr (xs w) (y h i)
			
			where
				xn = 100
				xs w = 500 - 100 - w
				y h i = 150 + (h + 2) * i
		
		-- Applique la fonction f à chaque élément d'un tableau avec l'index 'i' en paramètre supplémentaire
		forEach [] _ _     = return ()
		forEach (x:xs) f i = do
			f x i
			forEach xs f (i+1)	
			
	
		
--Enregistre un nouveau score.

-- Etats de la fonction (menu | écrire | accepter pseudo | quitter)
data State
	= Writing String
	| Menu Int
	| Save
	| Abort
	deriving (Eq)
	
writeScore = writeScore' "" (Writing "")

-- writeScore' :: Ecran -> Environnement -> Score -> Nom -> Etat -> IO ()
writeScore' :: String -> State -> Surface -> Env -> Int -> IO ()
writeScore' name state scr env score = do

	--Affiche le menu de sauvegarde de score
	displayWriteScore scr env state score name
	SDL.flip scr
	--Agis en fonction de l'état actuel
	case state of
		Menu c -> reloopMenu c
		Writing _ -> reloopWriting
		Save -> saveScore score name
		Abort -> return ()
		
	where
		reloopMenu c = do
			ret <- manageMenuEvt c
			writeScore' name ret scr env score
			
		reloopWriting = do
			ret <- manageWritingEvt name
			case ret of
				Writing newname -> writeScore' newname ret scr env score
				Menu c -> writeScore' name ret scr env score

-- Affiche l'écran, en fonction de l'état (soit on écrit, soit on est sur le menu)
displayWriteScore :: Surface -> Env -> State -> Int -> String -> IO ()
displayWriteScore scr env state score name = do
	SDL.fillRect scr Nothing pblanc
	displaySurface (getImage env "suika") scr x y
	
	-- Petit message et menu si on y est, sinon pas de menu
	fontMsg <- vera 18
	case state of
		Writing _ -> Font.renderCenteredText fontMsg "Entrez votre pseudo." noir scr 20
		Menu c -> do
			Font.renderCenteredText fontMsg "Que faites vous?" noir scr 20
			displayWritingMenu scr env c
		_ -> return ()
			
	-- Affiche le score et le pseudo
	fontScore <- dejavu 15
	Font.renderAlignedText fontScore scoreetnom noir scr (150, 50) 5
	
	where
		pblanc = Pixel 0xFFFFFF
		scoreetnom = ["Score : "++(show score), "Pseudo : "++name]
		
		x = 500 - 333
		y = 640 - 327
		
-- Affiche le menu
displayWritingMenu :: Surface -> Env -> Int -> IO ()
displayWritingMenu scr env c = do	
	font' <- Font.dejavu 14
	TTF.setFontStyle font' [ StyleBold ]
	
	Font.renderAlignedText font' choices noir scr (80, 100) 10
	displaySelector font' 10 c scr env

	where
		choices = ["Renommer", "Sauvegarder", "Quitter"]
		
-- Affiche le sélecteur du menu
displaySelector :: Font -> Int -> Int -> Surface -> Env -> IO ()
displaySelector f step c scr env = do
                                 (_, h) <- TTF.textSize f "Nyu"

                                 let select = getImage env "sun"
				 Resources.displaySurface select scr x (y step h)
				 
				 return ()

				 where position step h = Rect x (y step h) 500 640
				       x = 20
				       y step h = 100 + (step + h) * c - 20

-- gère les évènements du menu
manageMenuEvt :: Int -> IO (State)
manageMenuEvt c = do
	evt <- SDL.waitEvent
	case evt of
		Quit -> exitWith ExitSuccess
		KeyDown (Keysym sym _ _) -> manageKey sym
		_ -> manageMenuEvt c
		
	where
		manageKey sym = case sym of
			SDLK_UP -> if c <= 0 then return $ Menu 2 else return $ Menu (c-1)
			SDLK_DOWN -> if c >= 2 then return $ Menu 0 else return $ Menu (c+1)
			SDLK_RETURN -> case c of
				0 -> return (Writing "")
				1 -> return Save
				2 -> return Abort
				_ -> manageMenuEvt c
			_ -> manageMenuEvt c

-- gère les évènementslors de la saisie du pseudo
manageWritingEvt :: String -> IO (State)
manageWritingEvt name = do
	evt <- SDL.waitEvent
	case evt of
		Quit -> exitWith ExitSuccess
		KeyDown (Keysym sym _ c) -> return $ manageKey sym c
		_ -> manageWritingEvt name
		
	where
		manageKey sym c = case sym of
			SDLK_RETURN -> Menu 0
			SDLK_BACKSPACE -> Writing (rmlst name)
			_ -> if isAlphaNum c then Writing (name++[c]) else Writing name
			
		rmlst [] = []
		rmlst [x] = []
		rmlst (x:xs) = x : (rmlst xs)

-- sauvegarde le score dans le fichier
saveScore a b = appendFile scoreDir ((show a)++" "++b)
