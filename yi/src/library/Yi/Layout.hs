{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, DeriveFunctor, TupleSections, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-} -- we might as well unbox our Ints.

-- | This module defines the layout manager interface (see 'LayoutManager'). To desgin a new layout manager, just make an instance of this class.
module Yi.Layout
  (
    -- * Concrete layouts
    Layout(..),
    Orientation(..),
    DividerPosition,
    DividerRef,
    RelativeSize,
    dividerPositionA,

    -- * Layout managers
    -- ** The interface
    LayoutManager(..),
    LayoutManagerMessage,
    AnyLayoutManager(..),
    layoutManagerSameType,
    -- ** Standard managers
    wide,
    tall,
    slidyTall,
    slidyWide,
    hPairNStack,
    IncreaseMainWindows(..),
    DecreaseMainWindows(..),
    vPairNStack,
    -- *** Manual layout manager
    manualLayout,
    SplitWindow(..),
    -- * Utility functions
    -- ** Layouts as rectangles
    Rectangle(..),
    layoutToRectangles,
    -- ** Transposing things
    Transposable(..),
    Transposed(..),
    -- ** 'DividerRef' combinators
    -- $divRefCombinators
    LayoutM,
    pair,
    singleWindow,
    stack,
    evenStack,
    runLayoutM,
  )
 where

import Prelude()
import Data.Accessor.Basic
import Yi.Prelude
import Data.Typeable
import Data.Maybe
import Data.List(length, splitAt)
import qualified Control.Monad.State.Strict as Monad

-------------------------------- Some design notes ----------------------
-- [Treatment of mini windows]

-- Mini windows are not subject to layout; instead, they are always
-- placed at the bottom of the screen. There are multiple reasons for
-- this, as discussed in
-- https://groups.google.com/d/topic/yi-devel/vhTObC25dpY/discussion, one
-- being that for many layouts, the bottom (or top) of the screen is the
-- only reasonable place for mini windows (for example, think about
-- side-by-side layouts).

-- [Design of the 'Layout' datatype]

-- The 'Layout' datatype is currently implemented in terms of
-- horizontal stacks and vertical stacks. An alternative approach,
-- which xmonad uses, is the following: a 'Layout a' could be a
-- function @a -> Rectangle@ which specifies in coordinates where a
-- window should be placed.
--
-- While this alternative is more flexible than the current approach
-- in allowing spiral layouts and the like, the vty UI doesn't support
-- this: only vertical and horizontal composition of images is
-- allowed.



----------------------------------- Concrete 'Layout's.
-- | UI-agnostic layout schema. The basic constructs are
-- (horizontal/vertical) stacks with fixed ratios between window
-- sizes; and (horizontal/vertical) pairs with a slider in between (if
-- available).
data Layout a
  = SingleWindow a
  | Stack {
      orientation :: !Orientation,              -- ^ Orientation
      wins        :: [(Layout a, RelativeSize)] -- ^ The layout stack, with the given weights
        -- TODO: fix strictness for stack (it's still lazy)
      }
  | Pair {
       orientation :: !Orientation,     -- ^ Orientation
       divPos      :: !DividerPosition, -- ^ Initial position of the divider
       divRef      :: !DividerRef,      -- ^ Index of the divider (for updating the divider position)
       pairFst     :: !(Layout a),      -- ^ Upper of of the pair
       pairSnd     :: !(Layout a)       -- ^ Lower of the pair
    }
  deriving(Typeable, Eq, Functor)

-- | Accessor for the 'DividerPosition' with given reference
dividerPositionA :: DividerRef -> Accessor (Layout a) DividerPosition
dividerPositionA ref = fromSetGet setter getter where
  setter pos = set'
    where
      set' s@(SingleWindow _) = s
      set' p@Pair{} | divRef p == ref = p{ divPos = pos }
                    | otherwise       = p{ pairFst = set' (pairFst p), pairSnd = set' (pairSnd p) }
      set' s@Stack{} = s{ wins = fmap (\(l, r) -> (set' l, r)) (wins s) }

  getter = fromMaybe invalidRef . get'

  get' (SingleWindow _) = Nothing
  get' p@Pair{} | divRef p == ref = Just (divPos p)
                | otherwise       = get' (pairFst p) <|> get' (pairSnd p)
  get' s@Stack{} = foldl' (<|>) Nothing (fmap (get' . fst) (wins s))

  invalidRef = error "Yi.Layout.dividerPositionA: invalid DividerRef"

instance Show a => Show (Layout a) where
  show (SingleWindow a) = show a
  show (Stack o s) = show o ++ " stack " ++ show s
  show p@(Pair{}) = show (orientation p) ++ " " ++ show (pairFst p, pairSnd p)

-- | The initial layout consists of a single window
instance Initializable a => Initializable (Layout a) where
  initial = SingleWindow initial

-- | Orientations for 'Stack' and 'Pair'
data Orientation
  = Horizontal
  | Vertical
  deriving(Eq, Show)

-- | Divider reference
type DividerRef = Int

-- | Divider position, in the range (0,1)
type DividerPosition = Double

-- | Relative sizes, for 'Stack'
type RelativeSize = Double

----------------------------------------------------- Layout managers
-- TODO: add Binary requirement when possible
-- | The type of layout managers. See the layout managers 'tall', 'hPairNStack' and 'slidyTall' for some example implementations.
class (Typeable m, Eq m) => LayoutManager m where
  -- | Given the old layout and the new list of windows, construct a
  -- layout for the new list of windows.
  --
  -- If the layout manager uses sliding dividers, then a user will expect that most
  -- of these dividers don't move when adding a new window. It is the layout
  -- manager's responsibility to ensure that this is the case, and this is the
  -- purpose of the @Layout a@ argument.
  --
  -- The old layout may come from a different layout manager, in which case the layout manager is free to ignore it.
  pureLayout :: m -> Layout a -> [a] -> Layout a
  -- | Describe the layout in a form suitable for the user.
  describeLayout :: m -> String
  -- | Sends a message to the layout manager, telling it to change its appearance.
  -- This message may take any ('Typeable') type, so that layout managers may define
  -- their own messages. Some examples
  --
  --  * 'hPairNStack' and 'vPairNStack' implement
  sendMessage :: LayoutManagerMessage s => s -> m -> m
  sendMessage = const id

-- | Messages to
class (Show a, Typeable a) => LayoutManagerMessage a

-- | Existential wrapper for 'Layout'
data AnyLayoutManager = forall m. LayoutManager m => AnyLayoutManager !m
  deriving(Typeable)

instance Eq AnyLayoutManager where
  (AnyLayoutManager l1) == (AnyLayoutManager l2) = maybe False (== l2) (cast l1)

instance LayoutManager (AnyLayoutManager) where
  pureLayout (AnyLayoutManager l) = pureLayout l
  describeLayout (AnyLayoutManager l) = describeLayout l
  sendMessage msg (AnyLayoutManager l) = AnyLayoutManager (sendMessage msg l)

-- | The default layout is 'tallLayout'
instance Initializable AnyLayoutManager where
  initial = hPairNStack 1

-- | True if the internal layout managers have the same type (but are not necessarily equal).
layoutManagerSameType :: AnyLayoutManager -> AnyLayoutManager -> Bool
layoutManagerSameType (AnyLayoutManager l1) (AnyLayoutManager l2) = typeOf l1 == typeOf l2

------------------------------ Standard layouts
-- | Tall windows (i.e. places windows side-by-side, equally spaced)
data Tall = Tall
  deriving(Eq, Typeable)

-- | Windows placed side-by-side, equally spaced.
tall :: AnyLayoutManager
tall = AnyLayoutManager Tall

instance LayoutManager Tall where
  pureLayout Tall _oldLayout ws = runLayoutM $ evenStack Horizontal (fmap singleWindow ws)
  describeLayout Tall = "Windows positioned side-by-side"

-- | Wide windows (windows placed on top of one another, equally spaced)
data Wide = Wide
  deriving(Eq, Typeable)

instance LayoutManager Wide where
  pureLayout Wide _oldLayout ws = runLayoutM $ evenStack Vertical (fmap singleWindow ws)
  describeLayout Wide = "Windows positioned above one another"

-- | Windows placed on top of one another, equally spaced
wide :: AnyLayoutManager
wide = AnyLayoutManager Wide

-- | Tall windows, with arranged in a balanced binary tree with sliders in between them
data SlidyTall = SlidyTall
  deriving(Eq, Typeable)

-- | Tall windows, arranged in a balanced binary tree with sliders in between them.
slidyTall :: AnyLayoutManager
slidyTall = AnyLayoutManager SlidyTall

instance LayoutManager SlidyTall where
  -- an error on input [] is easier to debug than an infinite loop.
  pureLayout SlidyTall _oldLayout [] = error "Yi.Layout: empty window list unexpected"
  pureLayout SlidyTall oldLayout xs = runLayoutM (go (Just oldLayout) xs) where
     go _layout [x] = singleWindow x
     go layout (splitList -> (lxs, rxs)) =
       case layout of
           -- if the old layout had a pair in the same point of the tree, use its divider position
           Just (Pair Horizontal pos _ l r) -> pair Horizontal pos (go (Just l) lxs) (go (Just r) rxs)
           -- otherwise, just use divider position 0.5
           _ -> pair Horizontal 0.5 (go Nothing lxs) (go Nothing rxs)

  describeLayout SlidyTall = "Slidy tall windows, with balanced-position sliders"

splitList :: [a] -> ([a], [a])
splitList xs = splitAt ((length xs + 1) `div` 2) xs

-- | Transposed version of 'SlidyTall'
newtype SlidyWide = SlidyWide (Transposed SlidyTall)
  deriving(Eq, Typeable)

-- | Transposed version of 'slidyTall'
slidyWide :: AnyLayoutManager
slidyWide = AnyLayoutManager (SlidyWide (Transposed (SlidyTall)))

instance LayoutManager SlidyWide where
    pureLayout (SlidyWide w) = pureLayout w
    describeLayout _ = "Slidy wide windows, with balanced-position sliders"

-- | Message for 'vPairNStack' and 'hPairNStack' telling them to increase the number of \"main\" windows by 1
data IncreaseMainWindows = IncreaseMainWindows
  deriving(Typeable, Show)
-- | Message for 'vPairNStack' and 'hPairNStack' telling them to decrease the number of \"main\" windows by 1
data DecreaseMainWindows = DecreaseMainWindows
  deriving(Typeable, Show)

instance LayoutManagerMessage IncreaseMainWindows
instance LayoutManagerMessage DecreaseMainWindows

-- | Fixed number of \"main\" windows on the left; stack of windows on the right
data HPairNStack = HPairNStack !Int
  deriving(Eq, Typeable)

-- | @n@ windows on the left; stack of windows on the right.
hPairNStack :: Int -> AnyLayoutManager
hPairNStack n | n < 1     = error "Yi.Layout.hPairNStackLayout: n must be at least 1"
                    | otherwise = AnyLayoutManager (HPairNStack n)

instance LayoutManager HPairNStack where
    pureLayout (HPairNStack n) oldLayout (fmap singleWindow -> xs)
          | length xs <= n = runLayoutM $ evenStack Vertical xs
          | otherwise = runLayoutM $ case splitAt n xs of
              (ls, rs) ->  pair Horizontal pos
                 (evenStack Vertical ls)
                 (evenStack Vertical rs)
       where
          pos = case oldLayout of
              Pair Horizontal pos' _ _ _ -> pos'
              _ -> 0.5

    describeLayout (HPairNStack n) = show n ++ " windows on the left; remaining windows on the right"
    sendMessage (cast -> Just IncreaseMainWindows) (HPairNStack n) = HPairNStack (n+1)
    sendMessage (cast -> Just DecreaseMainWindows) (HPairNStack n) = HPairNStack (max (n-1) 1)
    sendMessage _ lm = lm
    

newtype VPairNStack = VPairNStack (Transposed HPairNStack)
  deriving(Eq, Typeable)

-- | Transposed version of 'hPairNStack'.
vPairNStack :: Int -> AnyLayoutManager
vPairNStack n = AnyLayoutManager (VPairNStack (Transposed (HPairNStack n)))

instance LayoutManager VPairNStack where
    pureLayout (VPairNStack lm) = pureLayout lm
    sendMessage m (VPairNStack lm) = VPairNStack (sendMessage m lm)
    describeLayout (VPairNStack (Transposed (HPairNStack n))) = show n ++ " windows on top; remaining windows beneath"

-------------------------------- Manual layout manager
-- | This is really just WindowRef, but we don't have access to that here, and we're pretending that
-- 'LayoutManagers' are actually polymorphic in the window type...
type LayoutRef = Int
data ManualLayoutManager = ManualLayoutManager !(Layout LayoutRef)
  deriving(Eq, Typeable)

-- | Message for 'manualLayout', telling it to split the window named by 'LayoutRef' in the given orientation.
data SplitWindow = SplitWindow !Orientation !LayoutRef
  deriving(Show, Typeable)
instance LayoutManagerMessage SplitWindow

instance LayoutManager ManualLayoutManager where
    describeLayout (ManualLayoutManager lyt) = "Manual layout" -- draw it?
    pureLayout (ManualLayoutManager lyt) _old windows = undefined -- "zip" the windows with the layout. Also take care of divider positions
    sendMessage (cast -> Just (SplitWindow o ref)) lm = undefined -- split the given window
    sendMessage _ lm = lm

-- | A layout manager which provides no automation: the layout is driven entirely by 'SplitWindow' messages. However,
-- if windows have been created without also sending 'SplitWindow' messages, these windows will just be tacked onto the end of
-- the layout manager.
manualLayout :: AnyLayoutManager
manualLayout = AnyLayoutManager (ManualLayoutManager (SingleWindow 0))

----------------------- Utils

-- | A general bounding box
data Rectangle = Rectangle { rectX, rectY, rectWidth, rectHeight :: !Double }
  deriving(Eq, Show)

layoutToRectangles :: Rectangle -> Layout a -> [(a, Rectangle)]
layoutToRectangles bounds (SingleWindow a) = [(a, bounds)]
layoutToRectangles bounds (Stack o ts) = handleStack o bounds ts
layoutToRectangles bounds (Pair o p _ a b) = handleStack o bounds [(a,p), (b,1-p)]

handleStack :: Orientation -> Rectangle -> [(Layout a, RelativeSize)] -> [(a, Rectangle)]
handleStack o bounds tiles =
      let (totalSpace, startPos, mkBounds) = case o of
            Vertical -> (rectHeight bounds, rectY bounds, \pos size -> bounds{rectY = pos, rectHeight=size})
            Horizontal -> (rectWidth bounds, rectX bounds, \pos size -> bounds{rectX = pos, rectWidth=size})

          totalWeight' = sum (fmap snd tiles)
          totalWeight = if totalWeight' > 0 then totalWeight' else error "Yi.Layout: Stacks must have positive weights"
          spacePerWeight = totalSpace / totalWeight
          doTile pos (t, wt) = (pos + wt * spacePerWeight,
                                layoutToRectangles (mkBounds pos (wt * spacePerWeight)) t)
      in
       concat . snd . mapAccumL doTile startPos $ tiles

----------- Flipping things
-- | Things with orientations which can be flipped
class Transposable r where transpose :: r -> r
instance Transposable Orientation where { transpose Horizontal = Vertical; transpose Vertical = Horizontal }
instance Transposable (Layout a) where
    transpose (SingleWindow a) = SingleWindow a
    transpose (Stack o ws) = Stack (transpose o) (fmap (\(l,r) -> (transpose l,r)) ws)
    transpose (Pair o p r a b) = Pair (transpose o) p r (transpose a) (transpose b)

-- | Same as 'lm', but with all 'Orientation's 'transpose'd. See 'slidyWide' for an example of its use.
newtype Transposed lm = Transposed lm
  deriving(Eq, Typeable)

instance LayoutManager lm => LayoutManager (Transposed lm) where
    pureLayout (Transposed lm) l ws = transpose (pureLayout lm (transpose l) ws)
    describeLayout (Transposed lm) = "Transposed version of: " ++ describeLayout lm
    sendMessage msg (Transposed lm) = Transposed (sendMessage msg lm)

-------------------- 'DividerRef' combinators
-- $divRefCombinators
-- It is tedious and error-prone for 'LayoutManager's to assign 'DividerRef's themselves. Better is to use these monadic smart constructors for 'Layout'. For example, the layout
--
-- @'Pair' 'Horizontal' 0.5 0 ('Pair' 'Vertical' 0.5 1 ('SingleWindow' w1) ('SingleWindow' w2)) ('SingleWindow' w3)@
--
-- could be with the combinators below as
--
-- @'runLayoutM' $ 'pair' 'Horizontal' 0.5 ('pair' 'Vertical' 0.5 ('singleWindow' w1) ('singleWindow' w2)) ('singleWindow' w3)@
--
-- These combinators do will also ensure strictness of the 'wins' field of 'Stack'. They also tidy up and do some error checking: length-1 stacks are removed (they are unnecessary); length-0 stacks raise errors.

-- | A 'Layout a' wrapped in a state monad for tracking 'DividerRef's. This type is /not/ itself a monad, but should rather be thought of as a 'DividerRef'-free version of the 'Layout' type.
newtype LayoutM a = LayoutM (Monad.State DividerRef (Layout a))

singleWindow :: a -> LayoutM a
singleWindow a = LayoutM (pure (SingleWindow a))

pair :: Orientation -> DividerPosition -> LayoutM a -> LayoutM a -> LayoutM a
pair o p (LayoutM l1) (LayoutM l2) = LayoutM $ do
    ref <- Monad.get
    Monad.put (ref+1)
    Pair o p ref <$> l1 <*> l2

stack :: Orientation -> [(LayoutM a, RelativeSize)] -> LayoutM a
stack _ [] = error "Yi.Layout: Length-0 stack"
stack _ [l] = fst l
stack o ls = LayoutM (Stack o <$> mapM (\(LayoutM lm,rs) -> (,rs) <$> lm) ls)

-- | Special case of 'stack' with all 'RelativeSize's equal.
evenStack :: Orientation -> [LayoutM a] -> LayoutM a
evenStack o ls = stack o (fmap (\l -> (l,1)) ls)

runLayoutM :: LayoutM a -> Layout a
runLayoutM (LayoutM l) = Monad.evalState l 0