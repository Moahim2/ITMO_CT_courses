module HW5.Parser
  ( parse
  ) where

import HW5.Base (HiAction (..), HiExpr (..), HiFun (..), HiValue (..))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Cont (void)
import Data.Char (isAlpha, isAlphaNum, isSpace, ord)
import Data.Functor (($>))
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, choice, chunk, eof, many, manyTill,
                        notFollowedBy, runParser, satisfy, sepBy, sepBy1, some, try, (<?>), (<|>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (ParseErrorBundle)


type HiParser a = Parsec Void String a


-- |Parse the entire input string (to the end of the string).
parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser (hiFullExpr <* eof) "SYSTEM_INPUT"

----------------------------------------------------------------------------
-- Utility parsers

ws :: HiParser ()
ws = void . many $ satisfy isSpace

skipWs :: HiParser a -> HiParser a
skipWs = between ws ws

comma :: HiParser Char
comma = satisfy (== ',')

-- |Parse array of value separated by sep (with ignoring whitespaces)
listValues :: String       -- ^ prefix of array
           -> HiParser a   -- ^ parser for sep
           -> String       -- ^ suffix of array
           -> HiParser v   -- ^ parser for value
           -> HiParser [v] -- ^ returned array of values.
listValues bl sep br parserValue = between (chunk bl) (chunk br) $ skipWs $ sepBy (skipWs parserValue) sep

----------------------------------------------------------------------------
-- Main parser (via @makeExprParser@ and @table@ of operators)

-- |Parse any correct Expr (<=> 'correctExpr' in next documentations).
hiFullExpr :: HiParser HiExpr
hiFullExpr = makeExprParser anyTerm table
  where
    table = [ manyUnaryPostfix $
                [   simpleUnary "!" HiExprRun
                  , dotSyntax
                  , application ]
              ,
                [   applyWithPrefixAnotherOp binaryL "/" HiFunDiv "="
                  , applyBin binaryL "*" HiFunMul ]
              , [   applyBin binaryL "+" HiFunAdd
                  , applyBin binaryL "-" HiFunSub ]
              , [   applyWithPrefixAnotherOp binaryN "<" HiFunLessThan "="
                  , applyWithPrefixAnotherOp binaryN ">" HiFunGreaterThan "="
                  , applyBin binaryN ">=" HiFunNotLessThan
                  , applyBin binaryN "<=" HiFunNotGreaterThan
                  , applyBin binaryN "==" HiFunEquals
                  , applyBin binaryN "/=" HiFunNotEquals ]
              , [   applyBin binaryR "&&" HiFunAnd ]
              , [   applyBin binaryR "||" HiFunOr] ]
      where
        -- helpers for making operator's parsers with different priority

        -- * Creators of binary operators

        binaryL name f correctSuffix = InfixL (binaryImpl name f correctSuffix)
        binaryR name f correctSuffix = InfixR (binaryImpl name f correctSuffix)
        binaryN name f correctSuffix = InfixN (binaryImpl name f correctSuffix)

        -- * Creators of unary operators

        simpleUnary name constr = constr <$ (skipWs $ chunk name)
        -- |Combine many single unary operator parsers to
        -- one postfix operator's parser with common priority and left-associative.
        manyUnaryPostfix singleUnaryOps = return . Postfix . composeUnaryOps . try . choice $ singleUnaryOps

----------------------------------------------------------------------------
-- Functions for constructing operator's parsers

-- |Parse @name@ with ignoring @correctSuffix@ and apply this name to @converter@
binaryImpl :: String      -- ^ name of operator
           -> a           -- ^ converter to expr
           -> HiParser b  -- ^ parser for all correct suffix
           -> HiParser a  -- ^ result parser.
binaryImpl name f correctSuffix = f <$ try (skipWs $ chunk name <* correctSuffix)

-- |Make binary function with applying arguments to it.
binaryApplier :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryApplier fun a b = HiExprApply (HiExprValue $  HiValueFunction fun) [a, b]

-- |Make binary operator's parser (taking into account another operator with its prefix)
applyWithPrefixAnotherOp :: (String -> (HiExpr -> HiExpr -> HiExpr)
                            -> HiParser () -> Operator p HiExpr
                            )                 -- ^ combinator of operator's parser and creator for resulting entity
                         -> String            -- ^ name of operator
                         -> HiFun             -- ^ binary function entity
                         -> String            -- ^ name of suffix of another operator with a prefix equal to our operator
                         -> Operator p HiExpr -- ^ result operator's parser.
applyWithPrefixAnotherOp binaryT name fun suffixAnother = binaryT name (binaryApplier fun) $
 notFollowedBy $ chunk suffixAnother

-- |Make binary operator's parser (which does not expect another operator with its prefix)
applyBin :: (String -> (HiExpr -> HiExpr -> HiExpr)
            -> HiParser () -> Operator p HiExpr) -- ^ combinator of operator's parser and creator for resulting entity
         -> String                               -- ^ name of operator
         -> HiFun                                -- ^ binary function entity
         -> Operator p HiExpr                    -- ^ result operator's parser.
applyBin binaryT name fun = binaryT name (binaryApplier fun) $ return ()

-- |Help-function for combine (left-associative) operators the same priority.
composeUnaryOps :: HiParser (HiExpr -> HiExpr) -> HiParser (HiExpr -> HiExpr)
composeUnaryOps singleUnaryOp = foldr1 (flip (.)) <$> some singleUnaryOp

----------------------------------------------------------------------------
-- Operator's parsers

-- |Parse unary operator (() <=> apply) on format (correctExpr1, correctExpr2, ..., correctExprN)
application :: HiParser (HiExpr -> HiExpr)
application = flip HiExprApply <$> listValues "(" comma ")" hiFullExpr

-- |Parse unary operator (.) on format .(a-Z)(-[a-Z0-9])*
dotSyntax :: HiParser (HiExpr -> HiExpr)
dotSyntax = flip HiExprApply . return . HiExprValue . HiValueString . T.pack . mergeOnMinus <$> afterDot
  where
    mergeOnMinus = foldr1 (\l r -> l ++ "-" ++ r)
    afterDot = char '.' *> faqDot

    -- parser all after dot from FAQ file
    faqDot = ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'

----------------------------------------------------------------------------
-- Term's parsers

anyTerm :: HiParser HiExpr
anyTerm = choice $ try <$>
  [  hiExprValue
   , bracketTerm
   , listTerm
   , dictTerm
   ]

-- |Parse construction as (correctExpr)
bracketTerm :: HiParser HiExpr
bracketTerm = between (skipWs $ satisfy (== '(')) (skipWs $ satisfy (== ')')) hiFullExpr <?> "expr in bracket"

-- |Parse [A, B, ..., Z] to function list(), where A, B, ..., Z - correctExpr
listTerm :: HiParser HiExpr
listTerm = HiExprApply (HiExprValue (HiValueFunction HiFunList)) <$>
 (listValues "[" comma "]" hiFullExpr) <?> "list"

-- |Parse  { k1 : a1, k2 : a2, ..., kn : an } to HiExprDict, where ki, ai - correctExpr
dictTerm :: HiParser HiExpr
dictTerm = HiExprDict <$> (listValues "{" comma "}" keyToValue) <?> "dict"
    where
      keyToValue = do
        keyExpr <- hiFullExpr
        void $ satisfy(== ':')
        valExpr <- hiFullExpr
        return (keyExpr, valExpr)

hiExprValue :: HiParser HiExpr
hiExprValue = HiExprValue <$> (skipWs $ choice $ try <$>
 [  hiValueFunction
  , hiValueNumber
  , hiValueBool
  , hiValueNull
  , hiValueString
  , hiValueBytes
  , hiValueAction
  ])

----------------------------------------------------------------------------
-- Value's parsers

hiValueFunction :: HiParser HiValue
hiValueFunction = HiValueFunction <$> hiFun <?> "fun"
  where
    hiFun = choice $
      map ((\(str, hiName) -> chunk str >> return hiName) . (\hF -> (show hF, hF))) [minBound..maxBound]

-- |Parser for floats (maybe with dot or without) (optional with prefixes +-)
hiValueNumber :: HiParser HiValue
hiValueNumber = HiValueNumber . toRational <$> L.signed (return ()) L.scientific <?> "number"

hiValueBool :: HiParser HiValue
hiValueBool = chunk "true" $> HiValueBool True <|> chunk "false" $> HiValueBool False <?> "bool"

hiValueNull :: HiParser HiValue
hiValueNull = chunk "null" $> HiValueNull <?> "null"

-- |Parse string between '"' '"'
hiValueString :: HiParser HiValue
hiValueString = HiValueString . T.pack <$> (char '"' >> manyTill L.charLiteral (char '"')) <?> "string"

-- |Parse [# b1 b2 ... bn #] to constructor of HiValueBytes, where bi - byte exactly in format 'xx' and x - hex-digit.
hiValueBytes :: HiParser HiValue
hiValueBytes = HiValueBytes . BS.pack <$>
  (listValues "[#" (return ()) "#]" hexTwoDigit) <?> "bytearray"
  where
    -- own hex-number parsers for greater security
    hexDigit = do
      c <- anySingle
      let d1 = ord c - ord '0'
      let d2 = ord c - ord 'a'
      if d1 >= 0 && d1 < 10
        then return $ fromIntegral d1
        else if d2 >= 0 && d2 < 6
          then return . fromIntegral $ d2 + 10
          else fail "not byte char"

    hexTwoDigit = try $ do
      a <- hexDigit
      b <- hexDigit
      return $ a * 16 + b

hiValueAction :: HiParser HiValue
hiValueAction = choice
  ([ chunk "cwd" $> HiValueAction HiActionCwd
   , chunk "now" $> HiValueAction HiActionNow
   ]) <?> "action"
