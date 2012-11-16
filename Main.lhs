The Main module provides a small test harness for the type inference
engine.   You say

	ghci Main.lhs

Then, to the prompt you can type

	tcs "\\x . x"

to type-check a string, or 

	tcf "foo.test"

to type-check the contents of a file (which should be an expression)



\begin{code}
module Main where

import TcTerm
import TcMonad
import BasicTypes
import Parser
import Text.PrettyPrint.HughesPJ
import Text.ParserCombinators.Parsec

import IO (hPutStrLn, stderr)
import System (getArgs, exitWith, ExitCode(..))

main :: IO ()
main = do args <- getArgs
          case args of
              [] -> getContents >>= tcs
              [f] -> tcf f
              _ -> do hPutStrLn stderr "Usage: foo [ FILE ]"
                      exitWith (ExitFailure 1)

-------------------------------------
--	tcs type-checks an expression passed directly as a string
--	ghci> tcs "\\x. x"

tcs :: String -> IO ()	-- Strings
tcs s = tc_help (parseString s)

s1, s2 :: String

s1 = "\\x. \\y. x"	-- A tiny example

s2 = "let add = (\\x. \\y. x) :: forall a. a -> a -> a in \
     \ let id  = (\\x. x) :: forall a. a -> a in \
     \ add id id"

-------------------------------------
--	tcf type-checks an expression in a file
--	ghci> tcs "foo.test"

tcf :: String -> IO ()	-- Files
tcf f = tc_help (parseFile f)



-------------------------------------
--	The initial type environment. 
--	You can extend this as you like

tyvarA :: TyVar
tyvarA = BoundTv "a"

initTypeEnv :: [(Name,Sigma)]
initTypeEnv
      = [ ("+",    intType --> intType --> intType)
	, ("if",    ForAll [tyvarA ] (boolType --> TyVar tyvarA --> TyVar tyvarA))
	, ("True",  boolType)
	, ("False", boolType)
	]

-------------------------------------
--	From here down is helper stuff

tc_help :: IO (Maybe Term) -> IO ()
tc_help get_term
  = do  { mb_e <- get_term
	; case mb_e of {
	    Nothing -> return () ;
	    Just e  -> do {
	  res <- runTc initTypeEnv (typecheck e)
	; case res of
		Left err -> putStrLn (docToString err)
		Right ty -> putStrLn (docToString (sep [pprParendTerm e, nest 2 (dcolon <+> ppr ty)]))
   }}}


parseFile :: String -> IO (Maybe Term)
parseFile filename
  = do { r <- parseFromFile parseTerm filename
       ; case r of
	   Left err -> do { putStrLn ("Parse error: " ++ show err) 
			  ; return Nothing }
	   Right ans -> return (Just ans) }

parseString :: String -> IO (Maybe Term)
parseString str
  = do { let r = parse parseTerm "<interactive>" str
       ; case r of
	   Left err -> do { putStrLn ("Parse error: " ++ show err) 
			  ; return Nothing }
	   Right ans -> return (Just ans) }
\end{code}
