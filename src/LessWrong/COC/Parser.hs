{-# LANGUAGE TupleSections #-}
module LessWrong.COC.Parser where

import           Control.Applicative   (some, (<|>))
import           Control.Monad         (void, when)
import           Data.Text             (Text, pack)
import           Text.Megaparsec       (between, digitChar, eof, letterChar,
                                        parse, parseErrorPretty, spaceChar, try,
                                        upperChar)
import           Text.Megaparsec.Lexer (skipBlockComment, skipLineComment,
                                        space)
import qualified Text.Megaparsec.Lexer as L (lexeme, symbol)
import           Text.Megaparsec.Text  (Parser)

import           LessWrong.COC.Error
import           LessWrong.COC.Type    hiding (uni)

-- Lexer

spaceConsumer :: Parser ()
spaceConsumer = space (void spaceChar) lineComment blockComment
  where lineComment  = skipLineComment "--"
        blockComment = skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

arrow :: Parser ()
arrow = void $ symbol "->" <|> symbol "."

lambda :: Parser ()
lambda = void $ symbol "lambda" <|> symbol "\\" <|> symbol "λ"

forall :: Parser ()
forall = void $ symbol "forall" <|> symbol "∀" <|> symbol "π"

universe :: Parser Uni
universe = ((symbol "☐-" <|> symbol "[]-") >> some digitChar >>= pure . Box . read) <|>
           ((symbol "☐" <|> symbol "[]") >> pure (Box 1)) <|>
           (symbol "*" >> pure Star)

delimiter :: Parser ()
delimiter = void $ symbol ":"

variable :: Parser Var
variable = (V . pack <$>) . lexeme . try $ (some letterChar >>= check) <|> symbol "_"
  where check x = do when (x `elem` reserved) $
                       fail $ "keyword '" ++ x ++ "' cannot be a variable name"
                     pure x

reserved :: [String]
reserved = ["lambda", "forall", "inductive", "record", "data"]

-- Parser

parseTerm :: Text -> Either CalculusError Term
parseTerm txt =
  case parse (term <* eof) "(source)" txt of
    Right term -> Right term
    Left  err  -> Left . ParsingError $ parseErrorPretty err

term :: Parser Term
term = try applicationTerm <|> naturalTerm

applicationTerm :: Parser Term
applicationTerm = do alg <- naturalTerm
                     dat <- some nonArrowTerm
                     pure $ foldl App alg dat

naturalTerm :: Parser Term
naturalTerm = try arrowTerm <|> nonArrowTerm

arrowTerm :: Parser Term
arrowTerm = Pi noname <$> arrowTpe <* arrow <*> term

nonArrowTerm :: Parser Term
nonArrowTerm = lambdaTerm <|> piTerm <|> varTerm <|> uniTerm <|> parens term

lambdaTerm :: Parser Term
lambdaTerm = quaTerm lambda Lam

piTerm :: Parser Term
piTerm = quaTerm forall Pi

arrowTpe :: Parser Term
arrowTpe = varTerm <|> uniTerm <|> parens term

quaTerm :: Parser () -> (Var -> Term -> Term -> Term) -> Parser Term
quaTerm start cons = do (var, tpe) <- start *> parens ((,) <$> variable <* delimiter <*> term)
                        arrow
                        body <- term
                        pure $ cons var tpe body
                        
varTerm :: Parser Term
varTerm = Var <$> variable

uniTerm :: Parser Term
uniTerm = Uni <$> universe
