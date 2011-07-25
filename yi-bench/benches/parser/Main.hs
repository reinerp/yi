module Main where

import Yi.Bench.Parser.InsertOneChar
import qualified Data.Rope as R

import Yi.Buffer.Misc(Mode, BufferM)
import Yi.Mode.Haskell
import Yi.Modes

import Criterion.Main
import Control.DeepSeq

benchFromFile :: NFData a
              => String      -- ^ filename
              -> Mode syntax -- ^ mode
              -> BufferM ()   -- ^ what action to run
              -> Eval a      -- ^ what to evaluate
              -> IO Pure
benchFromFile fp mode edit eval = do
  rope <- R.readFile fp
  return $ parserBench rope mode Middle [(Middle, 1000)] edit eval

main = do
  lhs1 <- benchFromFile "HsExpr.lhs" literateMode insertOneChar evalPango
{-  lhs2 <- benchFromFile "HsExpr.lhs" literateMode doNothing evalPango
  lhs3 <- benchFromFile "HsExpr.lhs" literateMode insertOneChar evalSyntax
  lhs4 <- benchFromFile "HsExpr.lhs" literateMode doNothing evalSyntax
  lhs5 <- benchFromFile "HsExpr.lhs" literateMode insertOneChar evalStrokes-}
  lhs5 <- benchFromFile "HsExpr.lhs" literateMode insertOneChar evalFilteredStrokes
  lhsRef <- benchFromFile "HsExpr.lhs" fundamentalMode insertOneChar evalPango

  clever <- benchFromFile "Editor.hs" cleverMode insertOneChar evalPango
  clever2 <- benchFromFile "Editor.hs" cleverMode insertOneChar evalFilteredStrokes
  precise <- benchFromFile "Editor.hs" preciseMode insertOneChar evalPango
  precise2 <- benchFromFile "Editor.hs" preciseMode insertOneChar evalFilteredStrokes
  precise3 <- benchFromFile "Editor.hs" preciseMode insertOneChar evalSyntax
  fast <- benchFromFile "Editor.hs" fastMode insertOneChar evalPango
  hsRef <- benchFromFile "Editor.hs" fundamentalMode insertOneChar evalPango
  
  defaultMain [
    bgroup "literate" [
      bench "lhs1" lhs1,
{-      bench "lhs2" lhs2,
      bench "lhs3" lhs3,
      bench "lhs4" lhs4,
      bench "lhs5" lhs5,-}
      bench "lhs5" lhs5,
      bench "ref" lhsRef
    ],
    bgroup "hs" [
      bench "clever" clever,
      bench "clever2" clever2,
      bench "precise" precise,
      bench "precise2" precise2,
      bench "precise3" precise3,
      bench "fast" fast,
      bench "ref" hsRef
    ]
   ]
