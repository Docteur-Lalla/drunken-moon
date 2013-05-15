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
import Menu
import Resources
import Music

import Graphics.UI.SDL as SDL
-- import Graphics.UI.SDL.Framerate as FPS
import Graphics.UI.SDL.Mixer as MIX
import Graphics.UI.SDL.TTF as TTF

-- Fonction main servant de point de départ au programme.

main = withInit [InitVideo, InitAudio] $
  do
    ttf <- TTF.init
    case ttf of
      False -> putStrLn "SDL_TTF cannot be initialized."
      True -> do
      		-- Initialisation du module vidéo du programme.
		-- Création d'une fenêtre de 500x640 en 32 bits.
                screen <- SDL.setVideoMode 500 640 32 [HWAccel, DoubleBuf]
                SDL.setCaption "Drunken Moon" "Drunken Moon"

                --Initialisatio du module Audio du programme.
                openAudio 44100 AudioS16Sys 2 4096
                MIX.allocateChannels 16
                MIX.volume (-1) 20

		-- Activation de la répétition des touches et de l'Unicode.
		SDL.enableKeyRepeat 500 20
                enableUnicode True

		-- Initialisation de l'environnement (images et musiques).
                initEnvironment

                -- Lancement de la musique de fond
                startBGM "main_theme"

		-- Lancement du menu (coeur du programme) puis fermeture du jeu.
	        Menu.loop screen 0

	        Resources.freeEnvironment    
	        MIX.closeAudio

		SDL.quit
