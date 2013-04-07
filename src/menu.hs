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

module Menu where

import Font
import Score (showScores)
import Game (newGame)
import Resources

import System.Exit
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as IMG
import Graphics.UI.SDL.TTF as TTF

{- 
Cré l'environnement du menu
	* screen définit la fenètre
	* choice définit le choix de l'utilisateur
	* env définit les ressources (images/sons)
-}
data Env = MenuEnv { screen :: Surface, choice :: Int, env :: Environment }

title :: Surface -> IO ()
title screen = do
                 font <- Font.dejavu 20
		 TTF.setFontStyle font [ StyleBold ]
		 
		 font2 <- Font.dejavu 15
		 
		 text <- TTF.renderTextBlended font "Drunken Moon" color
		 SDL.blitSurface text Nothing screen (Just rect)

		 newgame <- TTF.renderTextBlended font2 "New Game" color
		 voirscores <- TTF.renderTextBlended font2 "Scores" color
		 quitter <- TTF.renderTextBlended font2 "Quitter" color

		 SDL.blitSurface newgame Nothing screen (Just $ Rect 210 155 0 0)
		 SDL.blitSurface voirscores Nothing screen (Just $ Rect 210 175 0 0)
		 SDL.blitSurface quitter Nothing screen (Just $ Rect 210 195 0 0)
		 return ()

		 where color = Color 0x00 0x00 0x00
		       rect = Rect 150 30 500 640
		      
		      
{-		      
	Display effectué en premier pour la récursion terminale,
	les 'return ()' sont remplacés par des reloop (rappel avec même Env)
	les autres cas sont des 'loop $ (nouvel Env)'
		ou des appels d'autres étapes (zb: 'Score.showScores screen')
		
	revenir de Scores à Menu, ou quitter le menu reviendrait à envoyer 'return ()'
-}
loop :: Env -> IO ()
loop (MenuEnv screen choice env) = do

		display screen env
		SDL.flip screen
		
		event <- waitEvent
		case event of
		  Quit -> exitWith ExitSuccess
		  KeyDown (Keysym _ _ 'q') -> return ()
		  KeyDown (Keysym sym _ _) -> manageKey sym
		  _			   -> reloop

		where 
			manageKey key = case key of
					SDLK_DOWN -> if (choice >= 2)
								then reloop
								else loop $ MenuEnv screen (choice+1) env
					SDLK_UP   -> if (choice <= 0)
								then reloop
								else loop $ MenuEnv screen (choice-1) env
					SDLK_RETURN -> case choice of
									0 -> do 
										Game.newGame
										reloop
									1 -> do
										Score.showScores screen env
										reloop
									2 -> return ()									
					_            -> reloop

			display screen env = do
					 let suika = getResource "suika" env
					 SDL.fillRect screen Nothing pixel
					 displaySurface suika screen x y
					 SDL.fillRect screen (Just (Rect 200 (172+20*choice) 100 1)) (Pixel 0x000000)

					 title screen

					 where color = Color 0x00 0x00 0x00
					       pixel = Pixel 0xFFFFFF
					       x = 500 - 333
					       y = 640 - 327

			reloop = loop $ MenuEnv screen choice env
