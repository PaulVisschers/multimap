module Data.MultiMap where

import qualified Data.Map as M
import qualified Data.Set as S

newtype MultiMap k a = Multi {unMulti :: M.Map k (S.Set a)} deriving (Eq, Ord, Show, Read)

empty :: MultiMap k a
empty = Multi M.empty

size :: MultiMap k a -> Int
size = M.foldr ((+) . S.size) 0 . unMulti

keyCount :: MultiMap k a -> Int
keyCount = M.size . unMulti

insert :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
insert k x (Multi m) = Multi $ M.insert k (S.insert x $ M.findWithDefault S.empty k m) m