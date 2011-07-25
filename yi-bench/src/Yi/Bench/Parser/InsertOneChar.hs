{-# LANGUAGE BangPatterns #-}

module Yi.Bench.Parser.InsertOneChar where

import Debug.Trace as T

import Control.Monad.State
import Yi.Bench.Parser as BP
import Yi.Bench.NFInstances()
import qualified Data.Rope as Rope
import qualified Data.Map as Map
import Criterion
import Control.DeepSeq
import Graphics.Rendering.Pango(PangoAttribute)

import Prelude(zip, length)
import Yi hiding(End)
import Yi.Prelude
import Yi.Buffer.Misc
import Yi.Syntax(Stroke)

data BufferPlace 
  = Beginning
  | Middle
  | End
  | At Int

-- | List of viewports: start of the viewport, and the viewport size.
type Viewports = [(BufferPlace, Int)]

type Eval a = WindowRef -> Region -> BenchM a

{- | A benchmark designed to test incremental syntax-highlighting.

 * as initialisation, the buffer is created and the parser is run for the current buffer state
 
 * the benchmark times the insertion of one character and the 
-}
parserBench :: NFData a =>
               Rope.Rope   -- ^ the original buffer
            -> Mode syntax -- ^ the syntax-highlighting mode
            -> BufferPlace -- ^ where to insert the character
            -> Viewports   -- ^ where to regenerate syntax-highlighting for
            -> BufferM ()  -- ^ what action to run
            -> Eval a      -- ^ what to evaluate
            -> Pure        -- ^ the benchmark
parserBench rope mode _ipoint views edit eval
  | Rope.length rope < 2 = error "insertOneCharBench: rope should be at least length 2"
  | otherwise =
  let
    fromPlace Beginning = 0
    fromPlace Middle = Rope.length rope `div` 2
    fromPlace End = Rope.length rope - 1
    fromPlace (At pt) = pt

    pangoEvalAttrs = updateSyntax >> mapRegions (\key rgn -> eval key rgn >>= (\xs -> evaluate (rnf xs)))

    (sharedTok, _) = flip BP.run (newToken rope) $ do
      -- set the views
      put  
        . Map.fromList
        . zip [WindowRef 0..]
        . fmap (\(place, len) -> mkRegion (Point $ fromPlace place) (Point $ fromPlace place + len))
        $ views

      -- set the mode
      withBufferB (setMode mode)
      
      -- set the point, appropriately for each view
      mapRegions $ \key rgn -> withBufferB (moveTo (regionStart rgn)) -- Point $ fromPlace ipoint))

      -- evaluate the syntax at the current views
      pangoEvalAttrs

    bench tok = fst $ flip BP.run tok $ do
      withBufferB edit
      pangoEvalAttrs
      
  in
    whnf bench sharedTok

insertOneChar :: BufferM ()
insertOneChar = insertB ' '

doNothing :: BufferM ()
doNothing = return ()

evalSyntax :: Eval [(Point, Attributes)]
evalSyntax = getAttrs

evalPango :: Eval [PangoAttribute]
evalPango = pangoAttrs

evalStrokes :: Eval [Stroke]
evalStrokes = getStrokes

evalFilteredStrokes :: Eval [Stroke]
evalFilteredStrokes = getFilteredStrokes