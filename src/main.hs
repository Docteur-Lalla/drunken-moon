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

import Score
import Game

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF

-- Fonction menu qui détermine si on lance une partie ou si on montre les scores.

menu :: String -> IO ()
menu "1" = Game.newGame
menu "2" = Score.showScores
menu c = putStrLn ("Le choix '" ++ c ++ "' n'existe pas !")

-- Fonction main servant de point de départ au programme.
{-
main = do
         putStrLn "Bonsoir ! Que voulez-vous faire ?"
	 putStrLn "1 : Nouvelle partie"
	 putStrLn "2 : Voir les meilleurs scores"
	 putStrLn "3 : Quitter"
	 choice <- getLine
	 if choice == "3"
	   then return ()
	   else do
	          menu choice
		  main
-}
main = withInit [InitVideo] $
          do
	    ttf <- TTF.init
	    case ttf of
	      False -> putStrLn "SDL_TTF cannot be initialized."
	      True -> do
	                screen <- SDL.setVideoMode 500 640 32 [HWSurface]
	                SDL.setCaption "Drunken Moon" "Drunken Moon"
	                enableUnicode True
	                Game.loop screen
			TTF.quit
