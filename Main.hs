{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import           Control.Lens
import           Control.Lens.SemiIso
import qualified Data.Attoparsec.Text as AP
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Scientific (Scientific)
import           Data.SemiIsoFunctor
import qualified Data.Syntax as S
import qualified Data.Syntax.Attoparsec.Text as S
import           Data.Syntax.Char (SyntaxText)
import qualified Data.Syntax.Char as S
import qualified Data.Syntax.Combinator as S
import           Data.Syntax.Indent (Indent)
import qualified Data.Syntax.Indent as S
import qualified Data.Syntax.Pretty as S
import           Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Text.PrettyPrint as P

-- | A JSON value.
data Value = Object (Map Text Value)
           | Array [Value]
           | String Text
           | Number Scientific
           | Bool Bool
           | Null
    deriving (Show)

$(makePrisms ''Value)

-- | Converts between a map and a list.
mapFromList :: Ord i => Iso' (Map i v) [(i, v)]
mapFromList = iso Map.toList Map.fromList

-- | A JSON string, with escaping.
string :: SyntaxText syn => Indent syn Text
string =  S.packed /$/ S.char '"' */ simany character /* S.char '"'
      /?/ "Error in \"string\"."
  where 
    character =  S.char '\\' */ escapedChar
             /|/ S.satisfy (\x -> x /= '"' && x /= '\\')
             /?/ "Invalid escape character."
    escapedChar = S.choice $ map (\(v, e) -> exact v /$/ S.char e)
                      [ ('"', '"'), ('\\', '\\'), ('/', '/'), ('\b', 'b')
                      , ('\f', 'f'), ('\n', 'n') , ('\r', 'r'), ('\t', 't')]

object :: SyntaxText syn => Indent syn Value
object = _Object . mapFromList 
      /$~ S.char '{' 
      /*/ S.indented (
              S.breakLine /*/
              S.sepBy field (S.spaces_ /* S.char ',' /* S.breakLine)
          )
      /*/ S.breakLine /*/ S.char '}'
      /?/ "Error in \"object\"."
  where field = string /* S.spaces_ /* S.char ':' /* S.spaces /*/ value

array :: SyntaxText syn => Indent syn Value
array = _Array 
     /$~ S.char '[' 
     /*/ S.indented (
             S.breakLine /*/
             S.sepBy value (S.spaces_ /* S.char ',' /* S.breakLine)
         )
     /*/ S.breakLine /*/ S.char ']'
     /?/ "Error in \"array\"."

value :: SyntaxText syn => Indent syn Value
value =  _String /$/ string
     /|/ _Number /$/ S.scientific
     /|/ object
     /|/ array
     /|/ _Bool . exact True /$/ S.string "true"
     /|/ _Bool . exact False /$/ S.string "false"
     /|/ _Null /$/ S.string "null"
     /?/ "Error in \"value\"."

main :: IO ()
main = do
    -- Load the standard input.
    t <- T.getContents
    
    let parser = S.getParser $ S.runIndent value
        printer = S.runPrinter $ S.runIndent value

    -- Try to parse it.
    case AP.parseOnly (AP.skipSpace *> parser <* AP.skipSpace <* AP.endOfInput) t of
      Left err  -> putStrLn err
      Right val -> do
        -- If parsing succeeded print the JSON value.
        print val

        -- Try to pretty print it.
        -- (Printing cannot really fail in this example)
        case printer val of
          Left err  -> putStrLn err
          Right doc -> putStrLn (P.render doc)

    return ()
