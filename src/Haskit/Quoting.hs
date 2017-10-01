{-# LANGUAGE TemplateHaskell #-}
module Haskit.Quoting(expr) where
import           Data.Generics.Aliases
import           Haskit.Parser
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           Text.Megaparsec           (runParser)

expr = QuasiQuoter { quoteExp = quoteExprExp }

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s = do
    loc <- TH.location
    let pos = (TH.loc_filename loc,
               fst (TH.loc_start loc),
               snd (TH.loc_start loc))
    let Right(expr) = runParser aExpr "" s
    dataToExpQ (const Nothing `extQ` antiExprExp) expr

antiExprExp :: AExpr -> Maybe (TH.Q TH.Exp)
antiExprExp e = Just $ [| e |]
