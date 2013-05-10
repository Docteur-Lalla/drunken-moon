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

module GameData where

-- Le joueur est représenté par sa position, la direction de son déplacement, ses vies, ses bombes et s'il tire.
data Player = Player { isFiring :: Bool
                      ,dir      :: (Bool, Bool, Bool, Bool)
                      ,pos      :: (Int, Int)
                      ,power    :: Int
                      ,bombs    :: Int
                      ,lives    :: Int 
                     }

-- Droite symbolisant le côté gauche du cône de tir du joueur.
firingFunction :: Player -> Float -> Maybe Float
firingFunction (Player _ _ (x,y) _ _ _) = \t -> if t < fy then Just (3.0 * t + fx) else Nothing

  where fx = fromIntegral x
        fy = fromIntegral y

-- Droite symbolisant le côté droit du cône de tir du joueur.
reversedFiringFunction :: Player -> Float -> Maybe Float
reversedFiringFunction (Player _ _ (x,y) _ _ _) = \t -> if t < fy then Just (500 - fx - 3.0 * t) else Nothing

  where fx = fromIntegral x
        fy = fromIntegral y

-- Prédicat vérifiant si un ennemi placé en (ex, ey) est touché par le tir
fired :: Player -> (Float, Float) -> Bool
fired player (ex, ey) = case (test2 ex (reversedFiringFunction player ey), test (firingFunction player ey) ex) of
                          (True, True) -> True
			  (_, _)       -> False

  where test Nothing  _  = False
        test (Just x) ex = x < ex

	test2 _ Nothing   = False
	test2 ex (Just x) = x > ex
