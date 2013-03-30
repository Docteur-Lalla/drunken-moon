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

module Game (newGame) where

-- Fonction getDifficulty demandant la difficulté au joueur.

getDifficulty :: IO (Maybe Int)
getDifficulty = do
                  putStrLn "Quel mode choisissez-vous ?"
		  putStrLn "1 : Facile"
		  putStrLn "2 : Normal"
		  putStrLn "3 : Difficile"
		  putStrLn "4 : Lunatique"
		  putStrLn "5 : Extra"
                  c <- getChar
		  return (analyze c)

-- Conversion du caractère en Maybe Int (Int si correct ou Nothing s'il y a erreur).

analyze :: Char -> Maybe Int
analyze '1' = Just 0
analyze '2' = Just 1
analyze '3' = Just 2
analyze '4' = Just 3
analyze '5' = Just 4
analyze _ = Nothing

-- Conversion de Maybe Int vers String.

stringOfDiff :: Maybe Int -> String
stringOfDiff (Just 0) = "Easy"
stringOfDiff (Just 1) = "Normal"
stringOfDiff (Just 2) = "Hard"
stringOfDiff (Just 3) = "Lunatic"
stringOfDiff (Just 4) = "Extra"
stringOfDiff Nothing = "Undefined"

-- Crée une partie.

newGame :: IO ()
newGame = do
            difficulty <- getDifficulty
	    putStrLn ("Vous jouez en mode " ++ (stringOfDiff $ difficulty) ++ ".")
