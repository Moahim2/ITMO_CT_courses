module HW5.Pretty
  ( prettyValue
  ) where

import HW5.Base (HiAction (..), HiValue (..))

import qualified Data.Text as T

import Data.Map (assocs)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import Data.Word (Word8)
import GHC.Exts (toList)
import GHC.Real (Ratio ((:%)))
import Prettyprinter (Doc, concatWith, pretty, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)
import Text.Printf (printf)

{-|
  Pretty output for HiValue.
  It takes one argument HiValue and return document (pretty showing).
  === Patterns (further, all the spaces (and other symbols) are met and indicated correctly in the right output):
    - HiValueNumber num ->
        >>> prettyValue (HiValueNumber 42 % 2)
        21
        >>> prettyValue (HiValueNumber 42 % 10)
        4.2
        >>> prettyValue (HiValueNumber 42 % 11)
        3 + 9/11
        >>> prettyValue (HiValueNumber -42 % 11)
        -3 - 9/11
        >>> prettyValue (HiValueNumber 42 % 53)
        42/53
    - HiValueFun fun -> show fun
    - HiValueBool b -> true or false
    - HiValueNull -> null
    - HiValueString text -> "text"
    - HiValueList [v1, v2, ..., vn]  -> [p1, p2, ..., pn] , where pi = prettyValue vi
    - HiValueBytes [b1 b2 ... bn] -> [#b1 b2 bn#]
    - HiValueDict {k1:a1, k2:a2, ..., kn:an} -> {pk1: pa1, pk2: pa2, ..., pkn: pan} , where pki = prettyValue pki
                                                                                            pai = prettyValue kai
    - HiValueAction (actionName arg1 arg2 argn) -> actionName(arg1, arg2, ..., argn) or actionName if zero args
    - HiValueTime time -> parse-time("@timeValueFormat@")
-}
prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber num@(a :% b)) = let (q, r) = quotRem a b in
   pretty $ case fromRationalRepetendUnlimited num of
  (sc, Nothing) -> case r of
    0 -> show q
    _ -> formatScientific Fixed Nothing sc
  _             -> case compare q 0 of
                      EQ -> printFraction r
                      GT -> printInParts "+"
                      LT -> printInParts "-"
                      where
                        printInParts sign = show q ++ " " ++ sign ++ " " ++ printFraction (abs r)
  where
    printFraction signRem = show signRem ++ "/"  ++ show b
prettyValue (HiValueBool True) = pretty "true"
prettyValue (HiValueBool False) = pretty "false"
prettyValue HiValueNull = pretty "null"
prettyValue (HiValueFunction hiFun) = pretty $ show hiFun
prettyValue (HiValueString text) = pretty $ '"' `T.cons` text `T.snoc` '"'
prettyValue (HiValueList list) = prettyListI "[" "," "]" (prettyValue <$> list)
prettyValue (HiValueBytes bytes) = prettyListI "[#" "" "#]" (pretty . printHex <$> toList bytes)
prettyValue (HiValueAction (HiActionRead path)) = prettyFunStyleOneStrArg "read" path
prettyValue (HiValueAction (HiActionWrite path byteStr)) = prettyFunStyle "write" $
 [(prettyStr path), (prettyValue (HiValueBytes byteStr))]
prettyValue (HiValueAction (HiActionMkDir path)) = prettyFunStyleOneStrArg "mkdir" path
prettyValue (HiValueAction (HiActionChDir path)) = prettyFunStyleOneStrArg "cd" path
prettyValue (HiValueAction HiActionCwd) = pretty "cwd"
prettyValue (HiValueAction HiActionNow) = pretty "now"
prettyValue (HiValueAction (HiActionRand a b)) = prettyFunStyle "rand" $ pretty <$> [a, b]
prettyValue (HiValueAction (HiActionEcho text)) = prettyFunStyleOneStrArg "echo" $ text
prettyValue (HiValueTime time) = prettyFunStyleOneStrArg "parse-time" $ show time
prettyValue (HiValueDict dict)  = prettyListI "{" "," "}" $
 (\(k, a) -> prettyValue k <> pretty ":" <+> prettyValue a) <$> assocs dict

-- |Make document of fun-name and one arg <=> fun-name(arg)
prettyFunStyleOneStrArg :: Show a => String -> a -> Doc AnsiStyle
prettyFunStyleOneStrArg name path = prettyFunStyle name (prettyStr <$> [path])

-- |Concatenate all documents in fun-apply style <=> fun-name(a1, a2, ..., an)
prettyFunStyle :: Foldable t => String -> t (Doc AnsiStyle) -> Doc AnsiStyle
prettyFunStyle name listDoc = prettyListI (name ++ "(") "," ")" listDoc

-- |Concatenate all documents among themselves with sep, with prefix and suffix
prettyListI :: Foldable t        -- ^ container
            => String            -- ^ prefix
            -> String            -- ^ separator
            -> String            -- ^ suffix
            -> t (Doc AnsiStyle) -- ^ docs
            -> Doc AnsiStyle     -- ^ result doc
prettyListI bl sep br listDoc = pretty bl <> concatWith (\l r -> l <> pretty sep <+> r) listDoc <> pretty br

prettyStr :: Show a => a -> Doc AnsiStyle
prettyStr = pretty . show

printHex :: Word8 -> String
printHex = printf "%02x"
