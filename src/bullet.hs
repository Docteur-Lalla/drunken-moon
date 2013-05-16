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
import Graphics.UI.SDL as SDL

import Resources
import GameData

-- Fonction du temps nulle. Permet une écriture plus rapide de fonctions neutres.
nullTimeFunction :: TimeFunction
nullTimeFunction _ = 0.0

{-
  Il existe deux types de patterns :
    - Les projectiles (patterns simples).
    - Les patterns complexes mettant en scène plusieurs projectiles et sous-patterns.
 -}

data Pattern
  = Simple { x :: TimeFunction, y :: TimeFunction, r :: TimeFunction, spawn :: Int, life :: Int, skin :: String }
  | Complex { patts :: [(Pattern, Int)], x :: TimeFunction, y :: TimeFunction }

data Spellcard = Spellcard { patterns :: [ ([Int], Pattern) ] }

-- Un boss a une musique spécifique, une apparence et une liste de spellcards à lancer (survival ou classique).
data Boss = Boss { music :: String, face :: String, spellstart :: [Int], spell :: [(Spellcard, Bool)] }

-- Modifie le pattern pour que les projectiles aient leurs coordonnées absolues et ne dépendent plus de leur père.
absoluteBullet :: TimeFunction -> TimeFunction -> Pattern -> Pattern

{-
 - Un pattern simple voit ses fonctions de position altérées par celle de leurs pères.
 - Un pattern complexe altère ses enfants.

 Dans le cas d'un pattern complexe, on sépare les patterns enfants de leur date de spawn (firstof et secondof),
 on exécute absoluteBullet sur chacun d'entre eux (map) et on recrée une paire (pattern, spawn) avec les nouveaux
 patterns (zip).
 -}

absoluteBullet fx fy (Simple x y r sp l s) = Simple (\t -> x t + fx t) (\t -> y t + fy t) r sp l s
absoluteBullet fx fy (Complex pl x y)      = Complex np nullTimeFunction nullTimeFunction

  where x' t = fx t + x t
        y' t = fy t + y t

	mp = map (absoluteBullet x' y') (firstof [] pl)

	firstof acc []         = acc
	firstof acc ((p,_):xs) = firstof (acc ++ [p]) xs

	secondof acc []         = acc
	secondof acc ((_,s):xs) = secondof (acc ++ [s]) xs

	zip acc [] []           = acc
	zip acc (h1:t1) (h2:t2) = zip (acc ++ [(h1,h2)]) t1 t2

	np = zip [] mp (secondof [] pl)

-- Modifie le pattern pour que chaque sous-pattern gère son spawn de manière indépendante.
absoluteSpawn :: Int -> Pattern -> Pattern
absoluteSpawn sp (Simple x y r spn l s) = Simple x y r (sp + spn) l s
absoluteSpawn sp (Complex p x y)        = Complex newp x y

  where new_patt sp (patt, spn) = (absoluteSpawn (sp+spn) patt, 0)
        newp = map (new_patt sp) p

-- Aplatit l'arbre de patterns en une liste de projectiles (pattern simples).
listOfTree :: Pattern -> [Pattern]
listOfTree p@(Simple _ _ _ _ _ _) = [p]
listOfTree (Complex pl _ _)       = List.concat $ map listOne pl -- On mappe la fonction listOne.

  where listOne (patt, sp) = listOfTree patt -- On définie la procédure pour un couple unique (pattern, spawn).

-- bulletList permet d'obtenir la listes des projectiles avec des dates et fonctions absolues.
bulletList :: Pattern -> [Pattern]
bulletList patt = listOfTree bl

  where sppatt = absoluteSpawn 0 patt
        bl     = absoluteBullet nullTimeFunction nullTimeFunction sppatt

-- Affichage des projectiles à l'écran.
displayBullets :: Surface -> Int -> [Pattern] -> IO ()
displayBullets _ _ []                              = return ()
displayBullets scr t ((Complex _ _ _):xs)          = displayBullets scr t xs
displayBullets scr t ((Simple fx fy fr sp l s):xs) =
  do
    ball <- getImage s -- On récupère l'image liée au nom du projectile.
    if t - sp > 0
      then Resources.displaySurface ball scr x y -- On l'affiche.
      else return True

    displayBullets scr t xs -- On affiche les projectiles suivants.

    where x = truncate $ (fx (fromIntegral (t - sp)))
          y = truncate $ (fy (fromIntegral (t - sp)))

-- Nettoyage de la liste des projectiles selon leur durée de vie (fonction auxiliaire).
cleanBulletList' :: Int -> [Pattern] -> [Pattern] -> [Pattern]
cleanBulletList' _ acc []                              = acc
cleanBulletList' t acc ((Complex _ _ _):xs)            = cleanBulletList' t acc xs
cleanBulletList' t acc (ball@(Simple _ _ _ sp l _):xs) =
  if t - sp > l -- Si la durée de vie de la balle a dépassé le temps fixé, on la supprime de la liste.
    then cleanBulletList' t acc xs
    else cleanBulletList' t (acc ++ [ball]) xs

-- Fonction appelée dans le reste du programme pour nettoyer la liste de projectiles.
cleanBulletList :: Int -> [Pattern] -> [Pattern]
cleanBulletList t patt = cleanBulletList' t [] patt

-- Test de collision entre les projectiles et le joueur.
playerBulletCollision :: Float -> [Pattern] -> Player -> Bool
playerBulletCollision _ [] _                                                     = False
playerBulletCollision t ((Simple fx fy fr s _ _):xs) p@(Player _ _ (x, y) _ _ _)
  | t < fromIntegral s = playerBulletCollision t xs p
  | otherwise = if ret then True else playerBulletCollision t xs p

  {-
   Si l'addition du rayon du projectile et de celui de la hitbox du personnage est supérieure
   à la distance entre les centres de ceux-ci (position des deux objets), alors il y a collision.
   -}
  
  where x' = fx t
        y' = fy t
	r' = fr t

        xf = fromIntegral x
	yf = fromIntegral y

	dist = sqrt $ (x' - xf) ^ 2 + (y' - yf) ^ 2
	ret = dist < (r' + 5)
