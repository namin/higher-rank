Parser for the little language of the paper

  exp ::= integer
	| var
	| exp1 exp2
	| let var = exp1 in exp2
	| exp :: sig
	| ( exp )

  sig = rho | forall tv1 .. tvn . rho
  rho = tv  | Int | Bool | sig -> rho

\begin{code}
module Parser where

import BasicTypes hiding( dot )

-- import Text.Read( lexP, Read(..), Lexeme(..) )
-- import Text.ParserCombinators.Parser
-- import GHC.Read( parens )

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( haskellDef )

langDef :: P.LanguageDef st
langDef = haskellDef {	P.reservedNames = ["let", "in", "forall"],
			P.reservedOpNames = ["::", "\\", ".", "->"] }

lexer      :: P.TokenParser st
lexer       = P.makeTokenParser langDef
    
parens     :: CharParser st a -> CharParser st a
parens      = P.parens lexer
integer    :: CharParser st Integer
integer     = P.integer lexer
reservedOp :: String -> CharParser st ()
reservedOp  = P.reservedOp lexer
reserved   :: String -> CharParser st ()
reserved    = P.reserved lexer
identifier :: CharParser st String
identifier  = P.identifier lexer
dot        :: CharParser st String
dot	        = P.dot lexer
whiteSpace :: CharParser st ()
whiteSpace  = P.whiteSpace lexer
lexeme     :: CharParser st ()
lexeme      = P.whiteSpace lexer

parserToReadS :: Parser a -> ReadS a
parserToReadS p s = case parse (whiteSpace >> p) "" s of
			Left _err -> []
			Right a  -> [(a,"")]


-----------------------------
instance Read Term where
  readsPrec _ = parserToReadS readTerm

instance Read Type where
  readsPrec _ = parserToReadS readSigma

-----------------------------
parseTerm :: Parser Term
parseTerm = do	{ whiteSpace
		; t <- readTerm
		; eof
		; return t }

readTerm :: Parser Term
readTerm = choice [try ann, non_ann]

non_ann :: Parser Term
non_ann = choice [lam, rlet, app]

atom :: Parser Term
atom = choice [parens readTerm, lit, var]

lit :: Parser Term
lit = do { i <- integer; return (Lit (fromInteger i)) }

var :: Parser Term
var = do { v <- identifier; return (Var v) }


app :: Parser Term
app = do { (fun:args) <- many1 atom; 
	   return (foldl App fun args) }

lam :: Parser Term
lam = do { reservedOp "\\" ;
	   ann_lam <|> ord_lam }

ord_lam :: Parser Term
ord_lam = do { v <- identifier ;
	       dot ;
	       body <- readTerm ; 
	       return (Lam v body) }

ann_lam :: Parser Term
ann_lam = do { (v,ty) <- parens (do {
			    v <- identifier ;
			    reservedOp "::" ;
			    ty <- readSigma ;
			    return (v, ty) }) ;
	       dot ;
	       body <- readTerm ; 
	       return (ALam v ty body) }

rlet :: Parser Term
rlet = do { reserved "let" ;
	    v <- identifier ;
	    reservedOp "=" ;
	    rhs <- readTerm ;
   	    reserved "in" ;
	    body <- readTerm ;
	    return (Let v rhs body) }

ann :: Parser Term
ann = do { term <- non_ann ;
	   reservedOp "::" ;
	   ty <- readSigma ;
	   return (Ann term ty) }

---------------------------------
--	Types
---------------------------------

readSigma :: Parser Sigma	-- Not necessarily with parens
readSigma = choice [try (parens readSigma), sigma, readRho]

atomSigma :: Parser Sigma
atomSigma = choice [try (parens sigma), atomRho]

sigma :: Parser Sigma
sigma = do { reserved "forall" ;
	     tvs <- readTvs ;
	     dot ;
	     rho <- readRho ;
	     return (ForAll (map BoundTv tvs) rho) }

--------------
readRho :: Parser Rho	-- Not necessarily with parens
readRho = choice [try rfun, atomRho]

rfun :: Parser Rho
rfun = do { arg <- atomSigma ; reservedOp "->";
	    res <- readRho; return (Fun arg res) }

atomRho :: Parser Rho
atomRho = choice [try tvar, tcon, parens readRho]

--------------
readTau :: Parser Tau	-- Not necessarily with parens
readTau = choice [try tfun, atomTau]

atomTau :: Parser Tau
atomTau = choice [try tvar, tcon, parens readTau]

tvar, tfun, tcon :: Parser Tau
tvar = do { v <- identifier;
	    if (v == "Int" || v == "Bool")
	     then fail "" else return ();
	    return (TyVar (BoundTv v)) }
tfun = do { arg <- atomTau ; reservedOp "->";
	    res <- readTau; return (Fun arg res) }
tcon = choice [try $ do { "Int"  <- identifier; return intType },
	       do { "Bool" <- identifier; return boolType }]

--------------
readTvs :: Parser [Name]
readTvs = many identifier
\end{code}
