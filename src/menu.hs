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

module Menu (loop) where

import Font

import System.Exit
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as IMG
import Graphics.UI.SDL.TTF as TTF

title :: Surface -> IO ()
title screen = do
                 font <- Font.vera 28
		 TTF.setFontStyle font [ StyleBold ]

                 Font.renderCenteredText font "Drunken Moon" color screen 35

		 where color = Color 0x00 0x00 0x00

{-
    Boucle principale du menu.
    Il affiche les éléments de celui-ci (titre, choix, image de fond et sélecteur)

    manageKey : gère les évènements clavier
    display : gère l'affichage
    displaySelector : affiche le sélecteur
 -}

displaySelector :: Font -> Int -> Int -> Surface -> IO ()
displaySelector f step c scr = do
                                 (_, h) <- TTF.textSize f "Nyu"

                                 select <- IMG.load "rc/images/sun.jpg"
				 SDL.blitSurface select Nothing scr $ Just (position step h)
				 
				 return ()

				 where position step h = Rect x (y step h) 500 640
				       x = 20
				       y step h = 100 + (step + h) * c - 20

-- La fonction display effectue l'affichage de l'écran de menu : fond, titre, sélecteur et menu.
display :: Int -> Surface -> IO ()
display choice screen = do
                          -- D'abord le fond.
                          suika <- IMG.load "rc/images/Suika.jpeg"
	                  SDL.fillRect screen Nothing pixel
	                  SDL.blitSurface suika Nothing screen (Just (Rect x y 500 640))
		          
			  -- Puis le titre.
			  title screen

		          -- Enfin, le menu et le sélecteur.
			  font' <- Font.dejavu 14
		          TTF.setFontStyle font' [ StyleBold ]

		          Font.renderAlignedText font' choices color screen (80, 100) 10
			  displaySelector font' 10 choice screen

                          where color = Color 0x00 0x00 0x00
		                pixel = Pixel 0xFFFFFF
		                x = 500 - 333
		                y = 640 - 327
		                choices = ["Nouvelle partie", "Scores", "Quitter"]

loop :: Surface -> Int -> IO ()
loop screen choice = do
		       display choice screen
	     	       SDL.flip screen

                       event <- waitEvent
		       case event of
		         Quit -> exitWith ExitSuccess
		         KeyDown (Keysym _ _ 'q') -> return ()
		         KeyDown (Keysym key _ _) -> manageKey key
		         _			   -> reloop

		       where reloop = loop screen choice
		             loopwith = loop screen

		             -- Gestion des évènements du menu par la fonction manageKey.
			     manageKey key = case key of
		                               SDLK_DOWN	-> if choice >= 2
					                             then loopwith 0
								     else loopwith (choice + 1)
					       SDLK_UP		-> if choice <= 0
					                             then loopwith 2
								     else loopwith (choice - 1)
					       SDLK_RETURN	-> case choice of
					                             0 -> do
								            -- Game.newGame
									    reloop
								     1 -> do
								            -- Score.showScores
									    reloop
								     2 -> return ()
					       _		-> reloop
