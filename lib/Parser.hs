module Parser where 

import Text.Megaparsec 
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad.Combinators.Expr

data Proposition = 
    And Proposition Proposition 
    | Or Proposition Proposition 
    | Not Proposition 
    | If Proposition Proposition 
    | Iff Proposition Proposition 
    | Xor Proposition Proposition 
    | Nand Proposition Proposition 
    | Nor Proposition Proposition 
    | Boolean Char
    deriving (Eq)
    
data CalcCommand = Exit deriving (Eq)

type Parser = Parsec Void String

parseCalcInput :: Parser (Either CalcCommand Proposition)
parseCalcInput = eitherP (Exit <$ string "!") parseProposition

spaceP :: Parser () 
spaceP = L.space hspace1 empty empty

lexemeP :: Parser a -> Parser a 
lexemeP  = L.lexeme spaceP

parseProposition :: Parser Proposition 
parseProposition = 
    head <$> (manyTill expression eof)

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

parseVar :: Parser Proposition
parseVar = lexemeP ((Boolean <$> (char 'T' <|> char 'F' <|> char '1' <|> char '0')) <?> "var")

expression :: Parser Proposition 
expression = lexemeP (makeExprParser parseTerm table <?> "expression")
 
parseTerm :: Parser Proposition 
parseTerm = lexemeP (parens expression <|> parseVar <?> "term")

table :: [[Operator Parser Proposition]]
table = [ [ Prefix  (Not <$ choice ((lexemeP . string) <$>  ["~", "¬", "!"]))
          ]
        , [ InfixL  (And <$ choice ((lexemeP . string) <$>  ["&", "∧", "·"]))
          , InfixL  (If <$ choice ((lexemeP . string) <$>  ["->", "⇒", "→", "⊃"])) 
          ]
        , [ InfixL  (Iff <$ choice ((lexemeP . string) <$>  ["<->","⇔", "↔", "≡"])) 
          , InfixL  (Or <$ choice ((lexemeP . string) <$>  ["∥", "∨", "+", "||"]))
          , InfixL  (Nor <$ choice ((lexemeP . string) <$>  ["⊽"]))
          , InfixL  (Xor <$ choice ((lexemeP . string) <$>  ["⊕", "⊻", "↮", "≢"]))
          , InfixL  (Nand <$ choice ((lexemeP . string) <$>  ["⊼"]))
          ] 
        ]



