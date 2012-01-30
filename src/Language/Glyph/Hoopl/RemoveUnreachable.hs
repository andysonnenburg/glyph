{-# LANGUAGE GADTs, ScopedTypeVariables #-}
module Language.Glyph.Hoopl.RemoveUnreachable
       ( removeUnreachable
       ) where

import Compiler.Hoopl

removeUnreachable :: forall n . NonLocal n => Graph n O C -> Graph n O C
removeUnreachable graph
  | length blocks < mapSize (toLabelMap graph) = toGraph (entry graph) blocks
  | otherwise = graph
  where
    blocks :: [Block n C C]
    blocks = postorder_dfs graph
    
    entry :: Graph n O C -> Block n O C
    entry (GMany (JustO x) _ _) = x
    
    toLabelMap :: Graph n O C -> LabelMap (Block n C C)
    toLabelMap (GMany _ x _) = x
    
    toGraph :: Block n O C -> [Block n C C] -> Graph n O C
    toGraph x xs = GMany entry' body exit
      where
        entry' = JustO x
        body = foldr addBlock emptyBody xs
        exit = NothingO
