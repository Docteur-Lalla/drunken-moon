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

module Bullet where

import Data.List as List

-- Les paramètres des projectiles (coordonnées et rayon) sont fonctions du temps.
type TimeFunction = Int -> Int

nullTimeFunction :: TimeFunction
nullTimeFunction _ = 0

{-
  Il existe deux types de patterns :
    - Les projectiles (patterns simples).
    - Les patterns complexes mettant en scène plusieurs projectiles et sous-patterns.
 -}

data Pattern
  = Simple { x :: TimeFunction, y :: TimeFunction, r :: TimeFunction, life :: Int, skin :: String }
  | Complex { patts :: [(Pattern, Int)], x :: TimeFunction, y :: TimeFunction }

-- Une spellcard met en scène plusieurs patterns selon un angle et une date d'apparition.
data Spellcard = Spellcard { spawns :: [Int], patterns :: [(Int, Pattern)] }

-- Un boss a une musique spécifique, une apparence et une liste de spellcards à lancer (survival ou classique).
data Boss = Boss { music :: String, face :: String, spellstart :: [Int], spell :: [(Spellcard, Bool)] }

-- Aplanir la gestion des dates d'apparition.
absoluteSpawn :: Int -> Pattern -> Pattern
absoluteSpawn sp p@(Simple _ _ _ _ _) = p
absoluteSpawn sp (Complex (hd:tl) x y) = let new_patt (p, spawn) = (absoluteSpawn (sp+spawn) p, sp+spawn)
                                                  in Complex (new_patt hd : map new_patt tl) x y

-- Récupère les sous-patterns des sous-patterns pour en faire une unique liste.
pattList :: Pattern -> [ (Pattern, Int) ]
pattList (Simple _ _ _ _ _)   = []
pattList (Complex (x:xs) _ _) = x : (List.concat $ map pattList (firstof xs))
  where firstof [] = []
        firstof ((p, sp):xs) = p : firstof xs

-- Aplatir une arborescence de patterns.
absoluteBullet :: TimeFunction -> TimeFunction -> Pattern -> Pattern
absoluteBullet x' y' (Simple x y r l s) = Simple fx fy r l s
                                        
					where fx t = x t + x' t
					      fy t = y t + y' t

absoluteBullet x' y' p@(Complex ((patt, sp):xs) x y) = Complex fullpatt fx fy

  where spnewpatts = absoluteSpawn 0 p
        abs'patts x y = absoluteBullet x y spnewpatts
	fullpatt = pattList (abs'patts fx fy)

	fx t = x t + x' t
	fy t = y t + y' t
