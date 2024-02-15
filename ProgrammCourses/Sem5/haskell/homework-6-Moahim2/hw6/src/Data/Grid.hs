-- | This module defines 'Grid' datatype.
-- Feel free to define additional helper functions to work
-- with this datatype in this module.
module Data.Grid
  ( Grid (..)
  , gUp
  , gDown
  , gLeft
  , gRight
  , grToList
  ) where

import Control.Comonad (Comonad (..))

import Data.ListZipper (ListZipper (..), lLeft, lRight, lGenerator, lzToList)

-- |A 2D grid.
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

-- |Helper private function for constructing g-moves functions and instance Functor.
gM :: (ListZipper (ListZipper a1) -> ListZipper (ListZipper a2)) -> Grid a1 -> Grid a2
gM fun (Grid g) = Grid . fun $ g

instance Functor Grid where
  fmap = gM . fmap . fmap

-- Moving functions (by analogy with ListZipper but with new dimension).
gUp :: Grid a -> Grid a
gUp = gM lLeft

gDown :: Grid a -> Grid a
gDown = gM lRight

gLeft :: Grid a -> Grid a
gLeft = gM $ fmap lLeft

gRight :: Grid a -> Grid a
gRight = gM $ fmap lRight
--

instance Comonad Grid where
  extract = extract . extract . unGrid

  duplicate = Grid . fmap gHorizontal . gVertical
    where
      gHorizontal = lGenerator gLeft gRight
      gVertical = lGenerator gUp gDown

-- |Make list with summary size @n@.
grToList :: Int    -- ^ n
         -> Grid a -- ^ grid
         -> [[a]]  -- ^ result matrix.
grToList n (Grid g) = lzToList n $ fmap (lzToList n) g
