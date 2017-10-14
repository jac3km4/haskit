{-# LANGUAGE TemplateHaskell #-}
module Haskit.Quoting
  ( def
  ) where
import           Data.Binary.Put           (runPut)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (toStrict)
import           Data.Generics.Aliases
import qualified Haskit.CodeGen            as CG
import qualified Haskit.Parser             as P
import           Instances.TH.Lift
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           LuaJIT.ByteCode           as BC
import           Text.Megaparsec           (runParser)

def :: QuasiQuoter
def = QuasiQuoter {quoteExp = quoteExprExp}

quoteExprExp :: String -> TH.ExpQ
quoteExprExp s = do
  -- loc <- TH.location
  -- let pos =
        -- (TH.loc_filename loc, fst (TH.loc_start loc), snd (TH.loc_start loc))
  let Right (d) = runParser P.definition "" s
  let m = toStrict . runPut . BC.putByteCode $ CG.compile [d]
  dataToExpQ (const Nothing `extQ` antiExprExp) m

antiExprExp :: BS.ByteString -> Maybe (TH.Q TH.Exp)
antiExprExp e = Just $ [|BC.loadByteCode' e|]
