{- ----------------------------------------------------------------------------------------
   what    : (im)predicativity, examples from Daan Leijen. HMF: Simple type inference for first-class polymorphism. International Conference on Functional Programming, 2008. 
   expected: 
---------------------------------------------------------------------------------------- -}

module Main where

poly (f :: forall a . a -> a) =  (f (1::Int), f True)

apply  f x = f x
revapp x f = f x
iapp :: (Int -> Int) -> Int -> Int
iapp   f x = f x
-- a1 = apply  poly
-- a2 = apply ~poly

single x = [x]

ids1 = single  id
ids2 = single ~id
ids3 = single  id :: [forall a . a->a]

idss1a = single  ids1
idss1b = single ~ids1

idss2a = single  ids2
idss2b = single ~ids2

ra1 = revapp id poly
ra2 = let f = revapp id in f poly
ra3 = let f = revapp id in (f poly, f iapp)

choose :: a -> a -> a
choose x y = x

ch1 = choose [] ids2
ch2 = choose ids2 []
ch3 = let f = choose [] in f ids2

foo1 = let foo x y = single y in foo ids2 id

main = return ()