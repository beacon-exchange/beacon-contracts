module Main where

import RIO
import RIO.Text.Lazy (toStrict)

import Text.Heterocephalus as H
import Text.Blaze.Renderer.Text as B

main_sol :: Text
main_sol = toStrict $ renderMarkup $(H.compileTextFile "templates/Main.sol")

safemath_sol :: Text
safemath_sol = toStrict $ renderMarkup
  $(H.compileTextFile "templates/SafeMath.sol")

main :: IO ()
main = do
  writeFileUtf8Builder "artifacts/sol/Main.sol" (display main_sol)
  writeFileUtf8Builder "artifacts/sol/SafeMath.sol" (display safemath_sol)
