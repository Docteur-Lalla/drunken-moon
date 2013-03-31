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
                 font <- Font.vera 20
		 TTF.setFontStyle font [ StyleBold ]

                 Font.renderCenteredText font "Drunken Moon" color screen 35

		 where color = Color 0x00 0x00 0x00

loop :: Surface -> IO ()
loop screen = do
                event <- waitEvent
		case event of
		  Quit -> exitWith ExitSuccess
		  KeyDown (Keysym _ _ 'q') -> exitWith ExitSuccess
		  KeyDown (Keysym _ _ key) -> manageKey key
		  _			   -> return ()
		display screen
		SDL.flip screen
		loop screen

		where manageKey key = return ()

		      display screen = do
					 suika <- IMG.load "rc/images/Suika.jpeg"
					 SDL.fillRect screen Nothing pixel
					 SDL.blitSurface suika Nothing screen (Just (Rect x y 500 640))

					 title screen

					 where color = Color 0x00 0x00 0x00
					       pixel = Pixel 0xFFFFFF
					       x = 500 - 333
					       y = 640 - 327
