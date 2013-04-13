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
<<<<<<< HEAD
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
		  
=======

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
				
>>>>>>> dacbbcebf28e8860479ede607fcf3318ce74eab0
		  
		  
-- Affiche les scores enregistrés.

<<<<<<< HEAD
showScores :: Surface -> Env -> IO ()
showScores screen env = do

		displayShowScores screen env
=======
showScores :: Surface -> Environment -> IO ()
showScores screen env = do

		display screen
>>>>>>> dacbbcebf28e8860479ede607fcf3318ce74eab0
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
<<<<<<< HEAD
						
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
=======
					
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
>>>>>>> dacbbcebf28e8860479ede607fcf3318ce74eab0

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

<<<<<<< HEAD
-- sauvegarde le score dans le fichier
saveScore a b = appendFile scoreDir ((show a)++" "++b)
=======
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
>>>>>>> dacbbcebf28e8860479ede607fcf3318ce74eab0
