{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Tree
import Data.Maybe (isJust, fromJust)

type Couple a = (a, Maybe a)
type FamilyTree a = Tree (Couple a)

addNode :: Couple a -> FamilyTree a
addNode a = Node a []

findByHusband :: Eq a => a -> FamilyTree a -> Maybe (Couple a)
findByHusband husband tree =
  foldTree (\(a,b) rest -> if a == husband then Just (a,b) else Nothing) tree

findByWife :: Eq a => a -> FamilyTree a -> Maybe (Couple a)
findByWife wife tree =
  foldTree (\(a,b) rest -> if isJust b && wife == fromJust b then Just (a,b) else Nothing) tree

addToNode :: Couple a  -> FamilyTree a -> FamilyTree a
addToNode a tree = undefined

main :: IO ()
main = putStrLn "Hello, Haskell!"
