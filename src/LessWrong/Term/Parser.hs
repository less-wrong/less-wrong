module LessWrong.Term.Parser
  ( parseTerm
  ) where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (void)
import           Data.Text           (Text, pack)
import           Text.Parsec
import           Text.Parsec.Text

import           LessWrong.Term      (Name, Term (..))

parseTerm :: Text -> Either ParseError Term
parseTerm = parse termP "(input)"

termP :: Parser Term
termP = foldl1 App <$> many1 (spaced natTermP)

natTermP :: Parser Term
natTermP = (parens termP) <|> variableP <|> lambdaP

variableNameP :: Parser Name
variableNameP = pack <$> s
  where s = (:) <$> oneOf ['a'..'z'] <*> many (letter <|> digit <|> char '\'')

variableP :: Parser Term
variableP = Var <$> variableNameP

lambdaP :: Parser Term
lambdaP = do vars <- lambda *> many1 (spaced variableNameP) <* dot
             expr <- termP
             pure $ foldr Lam expr vars

-- Helpers

dot :: Parser ()
dot = void $ spaced (char '.')

lambda :: Parser ()
lambda = void $ spaced (char '\\' <|> char 'λ' <|> char 'L')

spaced :: Parser a -> Parser a
spaced x = x <* spaces

parens :: Parser a -> Parser a
parens x = spaced (char '(') *> x <* spaced (char ')')
