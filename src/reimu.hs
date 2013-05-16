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

module Reimu(run) where

import Bullet

vline n = Complex (map ball [-450, -300, -150, 0, 150, 300, 450, 600]) nullTimeFunction nullTimeFunction
          where ball posy = (Simple fx (fy posy) fr (5 * n) 5000 "ball", 0)
	        fx t = 1.0 * fromIntegral n
	        fy posy t = 1.0 * fromIntegral (posy + n)
	        fr t = 12.0

vline' n = Complex (map ball [-450, -300, -150, 0, 150, 300, 450, 600]) nullTimeFunction nullTimeFunction
           where ball posy = (Simple fx (fy posy) fr (5 * n) 5000 "ball", 0)
	         fx t = 500 - 1.0 * fromIntegral n
	         fy posy t = 1.0 * fromIntegral (posy + n)
	         fr t = 12.0

grid = Complex ((map lines [0, 10..500]) ++ (map lines' [0, 10..500])) nullTimeFunction nullTimeFunction
       where lines n = (vline n, 0)
             lines' n = (vline' n, 0)

run = bulletList grid
