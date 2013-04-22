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
import Music
import Time

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
showScores :: Surface -> IO ()
showScores screen = do
		displayShowScores screen
		SDL.flip screen
		
		event <- SDL.pollEvent
		case event of
			Quit                     -> exitWith ExitSuccess
			KeyDown (Keysym sym _ _) -> manageKey sym
			_                        -> reloop
		
		where reloop = do
			  wait 20
			  showScores screen
			
		      manageKey key = case key of
					SDLK_q -> 
					  do
					    playCancel
					    return ()
					_   -> reloop

title :: Surface -> IO ()
title screen =
  do
	font <- Font.vera 28
	TTF.setFontStyle font [ StyleBold ]

	Font.renderCenteredText font "Scores" color screen 35

	where color = Color 0x00 0x00 0x00

-- Affiche l'écran des scores.
displayShowScores :: Surface -> IO ()
displayShowScores scr = do
	-- Affichage de Suika dans le fond.
	SDL.fillRect scr Nothing pblanc
	suika <- getImage "suika"
	Resources.displaySurface suika scr x y

	-- Affichage du titre ("Scores") et des scores sous forme de tableau.
	title scr
	displayScoreLines scr
	
	where pblanc = Pixel 0xFFFFFF
					
	      x = 500 - 333
	      y = 640 - 327

-- Fonction permettant de comparer deux scores.
-- Nos paires (Pseudo, Score) ne permettent pas de comparaisons directe, on doit extraire le score pour cela.
compareScores :: (String, String) -> (String, String) -> Ordering
compareScores (_, n1) (_, n2) = compare (read n1 :: Int) (read n2 :: Int) -- read x convertit de String vers Int.

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
{- 
	State:
		Etats de la fonction (menu | écrire | sauvegarder | quitter)
		les états vont servir à savoir quels sont les actions effectuées par
		l'utilisateur, et ce qui doit être affiché à l'écran. 
		
		la boucle writeScore demande un State et agit en fonction de cet état.
-}
data State
	= Writing String
	| Menu Int
	| Save
	| Abort
	deriving (Eq)
	
-- Fonction demandant un pseudo à l'utilisateur, et si il veut changer son pseudo ou sauvegarder ou non son score.
writeScore = writeScore' "" (Writing "")

writeScore' :: String -> State -> Surface -> Int -> IO ()
writeScore' name state scr score = do

	-- Affiche le menu de sauvegarde de score.
	displayWriteScore scr state score name
	SDL.flip scr

	-- Agit en fonction de l'état actuel.
		--Menu -> gère les changements d'option (choix), ou l'appuis sur entrée pour choisir une option
		--Writing -> gère la saisie du pseudo
		--Save -> sauvegarde le score
		--Abort -> quitte sans sauvegarder
	case state of
	  Menu c -> reloopMenu c
	  Writing _ -> reloopWriting
	  Save -> saveScore score name
	  Abort -> return ()
		
	where
		--Fonction demandant un choix, et qui retourne un nouvel état par la fonction manageMenuEvent (gère le menu)
		reloopMenu c = do
			ret <- manageMenuEvent c
			wait 20
			writeScore' name ret scr score
			       
		--Fonction qui gère l'étape de saisie du pseudo
		reloopWriting = do
			ret <- manageWritingEvent name
			wait 20
			case ret of
				Writing newname -> writeScore' newname ret scr score
				Menu c -> writeScore' name ret scr score



-- Affiche l'écran, en fonction de l'état (soit on écrit, soit on est sur le menu).
displayWriteScore :: Surface -> State -> Int -> String -> IO ()
displayWriteScore scr state score name = do
	SDL.fillRect scr Nothing pblanc
	suika <- getImage "suika"
	displaySurface suika scr x y
	
	--Affiche un message en titre, et affiche le menu si l'utilisateur est sur le menu
	--Sinon, si l'utilisateur est entrain d'écrire le pseudo, cache le menu
	fontMsg <- vera 18
	case state of
		Writing _ -> Font.renderCenteredText fontMsg "Entrez votre pseudo." black scr 20
		Menu c -> do
			Font.renderCenteredText fontMsg "Que faites vous?" black scr 20
			displayWritingMenu scr c
		_ -> return ()
			
	-- Affiche le score et le pseudo.
	fontScore <- dejavu 15
	Font.renderAlignedText fontScore scoreetnom black scr (150, 50) 5
	
	where pblanc = Pixel 0xFFFFFF
	      scoreetnom = ["Score : "++(show score), "Pseudo : "++name]
		
		--Coordonnées de l'image de fond
	      x = 500 - 333
	      y = 640 - 327
		
		
		
-- Affiche le menu.
displayWritingMenu :: Surface -> Int -> IO ()
displayWritingMenu scr c = do	
	font' <- Font.dejavu 14
	TTF.setFontStyle font' [ StyleBold ]
	
	--Affiche les 3 choix possibles
	Font.renderAlignedText font' choices black scr (80, 100) 10
	
	--Affiche le sélecteur
	displaySelector font' 10 c scr

	where
		--Choix du menu
		choices = ["Renommer", "Sauvegarder", "Quitter"]
		
		
		
-- Affiche le sélecteur du menu.
displaySelector :: Font -> Int -> Int -> Surface -> IO ()
displaySelector f step c scr = do
				--Récupère h, la hauteur d'une ligne de texte
				(_, h) <- TTF.textSize f "Nyu"

				--Récupère l'image du sélecteur
				select <- getImage "sun"
                                 
                --Affiche l'image
				Resources.displaySurface select scr x (y step h)
				 
				return ()

				where 
				    x = 20
				    
				    --Ordonnée du sélecteur calculée à partir du choix du menu
				    y step h = 100 + (step + h) * c - 20



-- Fonction gérant les évènements du menu.
manageMenuEvent :: Int -> IO (State)
manageMenuEvent c = do
	event <- SDL.pollEvent
	
	--Gestion des évènements (fermeture de l'application, ou appuis sur une touche du clavier)
	case event of
		Quit -> exitWith ExitSuccess
		KeyDown (Keysym sym _ _) -> manageKey sym
		_ -> return (Menu c)
		
	where 
		--Fonction qui agit en fonction de la touche appuyée par l'utilisateur
		manageKey sym = case sym of
					--Si c'est la flèche du haut, ou du bas, on change de choix
	 		        SDLK_UP -> if c <= 0 then return $ Menu 2 else return $ Menu (c-1)
			        SDLK_DOWN -> if c >= 2 then return $ Menu 0 else return $ Menu (c+1)
					
					--Si on appuie sur entrée, on renvoie un état en fonction du choix sur lequel on était
			        SDLK_RETURN -> case c of
				                 0 -> return (Writing "")
			 	                 1 -> return Save
				                 2 -> return Abort
				                 _ -> return (Menu c)
			        _ -> return (Menu c)



--Fonction gérant les évènements lors de la saisie du pseudo.
manageWritingEvent :: String -> IO (State)
manageWritingEvent name = do
	event <- SDL.pollEvent
	
	--Regarde si on clique sur la croix (fermeture de la fenêtre), ou si on appuis sur une touche
	case event of
		Quit -> exitWith ExitSuccess
		KeyDown (Keysym sym _ c) -> return $ manageKey sym c
		_ -> return (Writing name)
		
	where 
		--Fonction qui agit en fonction de la touche appuyée
		manageKey sym c = case sym of
					--L'appuis sur entrée provoque un retour au menu
	 		          SDLK_RETURN -> Menu 0
	 		        --L'appuis sur la touche "supprimer" efface le dernier caractère
			          SDLK_BACKSPACE -> Writing (rmlst name)
			        --Sinon, si on a appuyé sur une lettre ou un chiffre, on rajoute le caractère au nom
			          _ -> if isAlphaNum c then Writing (name++[c]) else Writing name
			
		--Fonction qui enlève le dernier élément d'une liste (ici le dernier caractère de notre nom)
		rmlst [] = []
		rmlst [x] = []
		rmlst (x:xs) = x : (rmlst xs)

-- Sauvegarde le score dans le fichier.
saveScore :: Int -> String -> IO ()
saveScore a b = appendFile scoreDir ((show a) ++ " " ++ b ++ "\n")
