module LessWrong.HL.Parser where

import           Control.Applicative      (many, some, (<|>))
import           Data.Text                (Text)
import           Text.Megaparsec          (eof, optional, parse,
                                           parseErrorPretty)
import           Text.Megaparsec.Text     (Parser)

import           LessWrong.COC.Context    (Context (..))
import           LessWrong.COC.Error      (CalculusError (..))
import           LessWrong.COC.Parser     (parens, symbol, term, variable)
import           LessWrong.COC.Type       (Name, Term, Var (..))

import           LessWrong.HL.Parser.Type

parseREPL :: Text -> Either CalculusError Interactive
parseREPL txt =
  case parse (repl <* eof) "(repl)" txt of
    Right int -> Right int
    Left  err -> Left . ParsingError $ parseErrorPretty err

parseDeclaration :: Text -> Either CalculusError Declaration
parseDeclaration txt =
  case parse (declaration <* eof) "(source)" txt of
    Right decl -> Right decl
    Left  err  -> Left . ParsingError $ parseErrorPretty err

repl :: Parser Interactive
repl =  (uncurry BIND <$> binding) <|> (CODE <$> term) <|> (DECL <$> declaration)

binding :: Parser (Var, Term)
binding = (,) <$> (symbol ":let" *> variable <* symbol "=") <*> term

declaration :: Parser Declaration
declaration = (Ind <$> inductive) <|> (Rec <$> record) <|> (Alg <$> algebraic) <|> parens declaration

inductive :: Parser Inductive
inductive = decl "inductive" Inductive

record :: Parser Record
record = decl "record" Record

algebraic :: Parser Algebraic
algebraic = do symbol "data"
               V dname <- variable
               params <- (extractName <$>) <$> many variable
               conses <- (symbol "=" *> constructors) <|> pure []
               pure $ Algebraic dname params (Context conses)
  where constructors = (:) <$> constructor <*> many (symbol "|" *> constructor)

-- Helper declaration components functions

constructor :: Parser (Var, [TypeApp])
constructor = (,) <$> variable <*> many (tvar <|> parens tapp)
  where tvar = TVar . extractName <$> variable
        napp = tvar <|> parens tapp
        tapp = foldl TApp <$> napp <*> many napp

decl :: String -> (Name -> Context Term -> Context Term -> a) -> Parser a
decl keyword cons = do symbol keyword
                       V dname <- variable
                       params <- many contextDecl
                       symbol "="
                       conses <- some contextDecl
                       pure $ cons dname (Context params) (Context conses)

contextDecl :: Parser (Var, Term)
contextDecl = parens ((,) <$> variable <*> (symbol ":" *> term))

extractName :: Var -> Name
extractName (V x) = x
