{-
 -}

module LuaWrapper where

import Scripting.Lua as Lua

-- Charge un fichier lua et exécute son contenu.
dofile :: Lua.LuaState -> String -> IO ()
dofile lua fname =
  do
    rt <- Lua.loadfile lua fname

    case rt of -- Test le retour de loadfile.
      0 -> do
             Lua.call lua 0 0 -- Exécute le contenu du fichier 'fname'.
	     return ()

      _ -> putStrLn $ "File " ++ fname ++ " does not exists."

-- Appelle une fonction lua.
fcall :: Lua.LuaState -> String -> Int -> Int -> IO Int
fcall lua fname args rts =
  do
    Lua.getglobal lua fname
    Lua.call lua args rts
