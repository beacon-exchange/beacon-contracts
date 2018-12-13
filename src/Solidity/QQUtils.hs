module Solidity.QQUtils (
  -- module H,
  -- module B,
  B.Markup,
  ToMarkup(..),
  sol,
  txt,
  renderFile,
  renderMarkupStrict,
) where

import RIO

import RIO.Text.Lazy as T (toStrict)

import Text.Heterocephalus as H
import Text.Blaze as B
import Text.Blaze.Renderer.Text as B

import Language.Haskell.TH
import Language.Haskell.TH.Quote as TH

renderMarkupStrict :: B.Markup -> Text
renderMarkupStrict = T.toStrict . B.renderMarkup

solSetting :: HeterocephalusSetting
solSetting = textSetting { parseOptions = createParseOptions '%' '$' }

sol, txt :: TH.QuasiQuoter
txt = H.compile solSetting
sol = txt

renderFile :: FilePath -> Q Exp
renderFile = compileFile solSetting
