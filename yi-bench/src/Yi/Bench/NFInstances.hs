{-# LANGUAGE TemplateHaskell, TypeSynonymInstances #-}

-- | Exports instances of 'NFData'
module Yi.Bench.NFInstances() where

import Graphics.Rendering.Pango
import Control.DeepSeq
import Data.DeriveTH

import qualified Yi
import qualified Yi.Style as Yi
import qualified Yi.Syntax as Yi

rnfShow x = show x `deepseq` ()

instance NFData FontDescription where
  rnf = rnfShow

instance NFData Language where
  rnf = rnfShow

$(derives [makeNFData]
                     [''PangoAttribute,
                      ''FontStyle, 
                      ''Variant, 
                      ''PangoGravityHint, 
                      ''PangoGravity, 
                      ''PangoRectangle, 
                      ''Color,
                      ''Underline,
                      ''Stretch,
                      ''Weight
                      ]
                      )

instance NFData Yi.Point
instance NFData Yi.Attributes
instance NFData Yi.Stroke

--instance NFData PangoAttribute