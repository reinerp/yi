{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

{- | Benchmarking framework for incremental parsing.

We define a monad, 'BenchM', which is a mockup of the 'YiM' monad but only
with the features we need.
-}
module Yi.Bench.Parser
  (
    BenchM,
    -- * Monadic actions
    withBufferB,
    withBufferBKey,
    mapRegions,
    getText,
    getAttrs,
    getStrokes,
    getFilteredStrokes,
    pangoAttrs,
    evaluate,
    updateSyntax,
    -- * Running the monad
    Token,
    newToken,
    run,
  )
  where

import Debug.Trace as T
import Control.DeepSeq
import qualified Prelude
import Control.Monad.State.Strict (State, runState, MonadState(..), modify)
import qualified Data.Map as M
import qualified Data.Rope as Rope
import Graphics.Rendering.Pango
import qualified Graphics.UI.Gtk as Gtk

import Prelude(zip, drop, uncurry, dropWhile, takeWhile)
import Yi
import Yi.Style
import Yi.Prelude
import Yi.Buffer.Misc
import qualified Yi.Window as W
import Yi.UI.Utils
import Yi.Syntax


type Regions = M.Map WindowRef Region

-- | The main \"mockup\" monad. See the 'MonadState' instance for access to the 'Regions'.
newtype BenchM a = BenchM { runBenchM :: State (FBuffer, Regions) a }
  deriving(Functor, Applicative, Monad)

--------------------------------------------------------------------------------
--                            The monadic actions                             --
--------------------------------------------------------------------------------
-- | Lift a buffer action, for actions which depend on the window (such as syntax awareness)
withBufferBKey :: WindowRef -> BufferM a -> BenchM a
withBufferBKey wkey m = do
    (fb, rgns) <- BenchM get
    let (v, _, fb') = runBufferFull (mockWindow wkey) fb m
    fb' `seq` (BenchM $ put (fb', rgns))
    return v

-- | Lift a buffer action, for window-independent actions
withBufferB :: BufferM a -> BenchM a
withBufferB = withBufferBKey mockKey

instance MonadState Regions BenchM where
  get = BenchM (snd <$> get)
  put v = v `seq` BenchM (modify (\(a,_) -> (a,v)))

-- | Convenience function
mapRegions :: (WindowRef -> Region -> BenchM ()) -> BenchM ()
mapRegions f = (mapM_ (uncurry f) . M.toList) =<< get

-- | Get the text for the given region (can be used in conjuction with 'deepseq' and 'mapRegions' to force the text)
getText :: WindowRef -> Region -> BenchM Rope
getText wkey rgn = withBufferBKey wkey $ (Rope.take (fromSize $ regionSize rgn)) <$> streamB Forward (regionStart rgn)

-- | Runs 'modeGetStrokes' on the syntax (does less work than 'getAttrs')
getStrokes :: WindowRef -> Region -> BenchM [Stroke]
getStrokes wkey rgn = withBufferBKey wkey $ do
  getter <- withSyntaxB modeGetStrokes
  pt <- pointB
  return (getter pt (regionStart rgn) (regionEnd rgn))

filterStrokes :: Region -> [Stroke] -> [Stroke]
filterStrokes rgn strokes =
  let
    i = regionStart rgn
    j = regionEnd rgn
    dropBefore = dropWhile (\s -> spanEnd s <= i)
    takeIn = takeWhile (\s -> spanBegin s <= j)
--    removeEmpty = filter (\(Span b _m a) -> b /= a)
  in
    takeIn . dropBefore $ strokes

getFilteredStrokes :: WindowRef -> Region -> BenchM [Stroke]
getFilteredStrokes wkey rgn = filterStrokes rgn <$> getStrokes wkey rgn

-- | Runs 'attributesPictureAndSelB' on the given region (forcing this will force syntax highlighting)
getAttrs :: WindowRef -> Region -> BenchM [(Point, Attributes)]
getAttrs wkey rgn = withBufferBKey wkey $ attributesPictureAndSelB mockSty Nothing rgn

-- | Produces the 'PangoAttribute's for the given region (this is all the non-graphical work that Pango has to do).
pangoAttrs :: WindowRef -> Region -> BenchM [PangoAttribute]
pangoAttrs wkey rgn = do
  picture <- getAttrs wkey rgn
  let tos = regionStart rgn
      bos = regionEnd rgn
      strokes = [(start',s,end') | ((start', s), end') <- zip picture (drop 1 (fmap fst picture) ++ [bos]),
                  s /= emptyAttributes]
      rel p = fromIntegral (p - tos)
      allAttrs = concat $ do
        (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
        return $ [ {- T.trace "hello" $ -} AttrForeground (rel p1) (rel p2) (mkCol True fg)
                 , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                 , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                 , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                 , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                 ]
  return allAttrs

updateSyntax :: BenchM ()
updateSyntax = do
  (fb, rgns) <- BenchM get
  let fb' = focusSyntax rgns $ clearSyntax fb
  BenchM $ put (fb', rgns)

-- | Evaluate the argument to WHNF and return
evaluate :: a -> BenchM ()
evaluate a = a `seq` return ()

--------------------------------------------------------------------------------
--                            Running the 'BenchM'                            --
--------------------------------------------------------------------------------
-- | Abstract state token, for use with criterion
newtype Token = Token (FBuffer, Regions)

-- | Create a new token
newToken :: Rope -> Token
newToken rope = Token (newB mockBufRef mockBufId rope, M.singleton (WindowRef 0) (mkRegion (Point 0) (Point 0)))

-- | Run it!
run :: BenchM a -> Token -> (Token, a)
run m (Token t) = 
  case runState (runBenchM m) t of
    (a, t') -> (Token t', a)

--------------------------------------------------------------------------------
--                                Mock objects                                --
--------------------------------------------------------------------------------
mockWindow key = (W.dummyWindow mockBufRef) { W.wkey = key }
mockKey = WindowRef (-1)
mockBufRef = BufferRef 0
mockBufId = Left "Test-Buffer-ID"
mockSty = extractValue . configTheme . configUI $ defaultConfig

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

