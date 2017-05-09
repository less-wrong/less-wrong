module LessWrong.COC.Parser where

import           Control.Applicative   (some, (<|>))
import           Control.Monad         (void)
import           Data.Text             (Text, pack)
import           Text.Megaparsec       (between, letterChar, parse,
                                        parseErrorPretty, spaceChar, try)
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
lambda = void $ symbol "\\" <|> symbol "lambda" <|> symbol "λ"

forall :: Parser ()
forall = void $ symbol "\\/" <|> symbol "forall" <|> symbol "∀" <|> symbol "π"

uni :: Parser Const
uni = (symbol "*" >> pure Star) <|> ((symbol "☐" <|> symbol "[]") >> pure Box)

delimiter :: Parser ()
delimiter = void $ symbol ":"

variable :: Parser Var
variable = lexeme (V . pack <$> (some letterChar <|> symbol "_"))

-- Parser

parseTerm :: Text -> Either CalculusError Term
parseTerm txt =
  case parse term "(source)" txt of
    Right term -> Right term
    Left  err  -> Left . ParsingError $ parseErrorPretty err

term :: Parser Term
term = foldl1 App <$> some naturalTerm

naturalTerm :: Parser Term
naturalTerm = parens term <|> lambdaTerm <|> piTerm <|> varTerm <|> uniTerm

lambdaTerm :: Parser Term
lambdaTerm = quaTerm lambda Lam

piTerm :: Parser Term
piTerm = quaTerm forall Pi

simplePiTerm :: Parser Term
simplePiTerm = do from <- term
                  arrow
                  to <- try simplePiTerm <|> term
                  pure $ Pi noname from to

quaTerm :: Parser () -> (Var -> Term -> Term -> Term) -> Parser Term
quaTerm start f = do start
                     (var, tpe) <- parens $ (,) <$> variable <* delimiter <*> term
                     arrow
                     body <- try simplePiTerm <|> term
                     pure $ f var tpe body

varTerm :: Parser Term
varTerm = Var <$> variable

uniTerm :: Parser Term
uniTerm = Const <$> uni
