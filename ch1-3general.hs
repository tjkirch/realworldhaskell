module General where

import Data.List
import Data.Function

-- why doesn't this work?
-- update: it does if you do both - :load general.hs types.hs
-- would probably be taken care of by ghc --make
--import MyTypes

add a b = a + b

mydrop n xs = if n <= 0 || null xs
                 then xs
              else
                 mydrop (n-1) (tail xs)

isOdd n = mod n 2 == 1

shortCircuitOr a b = if a then a else b

-- First attempt.
-- should return element before the last.  however, if the list is 0 or 1
-- elements long, what should it return?  needs to be generic, and there's
-- no equivalent to nil.
-- "last" from Prelude cheats - it throws an exception for empty list.
brokenLastButOne xs = if length xs <= 1
                   then (-42)  -- what should this be?  want nil
                else
                   head (drop ((length xs) - 2) xs)

-- Second attempt.
-- From docs:
-- "The Maybe type encapsulates an optional value. A value of type Maybe a
-- either contains a value of type a (represented as Just a), or it is empty
-- (represented as Nothing). Using Maybe is a good way to deal with errors or
-- exceptional cases without resorting to drastic measures such as error."
-- Not exactly sure how it's better yet, but it prevents the error.
lastButOne xs = if length xs <= 1
                   then Nothing
                else
                   Just (head (drop ((length xs) - 2) xs))

-- My favorite, learned from pieces of posts on the book site.
-- Uses standard exceptions from last/init as necessary.
givenLastButOne = last . init

-- copy of standard "not" showing pattern matching
myNot True = False
myNot False = True

-- copy of standard "sum" function, showing pattern matching
sumList (x:xs) = x + sumList xs
sumList []     = 0
-- Could also do this, the wild card matches empty lists too:
--sumList _ = 0

-- ridiculous pattern matching
complicated (True, 42, a, b, x:(y:(z:xs))) = (18, (y, xs))

-- Copied from types.hs for use below
-- (can't get modules to work yet)
data List a = Cons a (List a)
              | Nil
              deriving (Show)

-- Given in book
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Converse
-- This works, but I don't understand using tuple parenthesis syntax to crease
-- a list, which would normally use square brackets
-- Figured it out - it's not tuple syntax, the parentheses were just grouping;
-- removed it and it works fine.  ":" is the list cons operator.
--fromMyList (Cons x xs) = (x : fromMyList xs)
fromMyList (Cons x xs) = x : fromMyList xs
fromMyList Nil = []

-- the second underscore captures the empty list of a two-item list
tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

-- Ch3 exercises
-- 1: length of list
myLength xs
   | xs == []  = 0
   | otherwise = 1 + myLength (tail xs)

-- 2: type signature
-- This is what I wanted, but ghc can't handle xs == [] with it:
--myLength :: [a] -> Integer

-- This is what GHC auto-assigns for the signature:
--myLength :: (Eq a, Num t) => [a] -> t

-- Let's try to clean it up...
--myLength :: (Eq a, Int t) => [a] -> t
-- doesn't work.  Num works because it's a type class... not sure what that
-- means yet.  Int is a type constructor, can't use it there.

-- This works - Integral is a type class, more specific than Num
myLength :: (Eq a, Integral t) => [a] -> t

-- This does not work - "Class Integral used as a type"
--myLength :: [a] -> Integral

-- 3: mean of list
listSum xs
   | xs == []  = 0
   | otherwise = head xs + listSum (tail xs)

listMean xs = listSum xs / fromIntegral (length xs)

-- 4: turn list into palindrome: [1,2,3] -> [1,2,3,3,2,1]
-- Technically my function doesn't give that answer, but I like it better.
palindromeList xs
   | xs == []       = xs
   | length xs == 1 = xs
   | otherwise      = [head xs] ++ palindromeList (tail xs) ++ [head xs]

-- 5: Test whether list is a palindrome
-- Easy way:
isPalindrome xs = xs == reverse xs

-- 6: sort list of lists by length of sublist
-- Simple way, using hints from book comments
sortBySublistLength xs = sortBy listLength xs
   where listLength a b = compare (length a) (length b)

-- alternative using Data.Function.on combinator
sortBySublistLength2 xs = sortBy (compare `on` length) xs

-- 7: intersperse
-- NOTE: single characters 'a', strings "a".  I was trying to use "," as first
-- param, which fails without some other magic.
myIntersperse :: a -> [[a]] -> [a]
myIntersperse _ [] = []
-- note: below line fails if you write (x) - tries to construct an infinite type
myIntersperse _ [x] = x
myIntersperse sep (x:xs) = x ++ [sep] ++ myIntersperse sep xs

-- 8: height of tree
data Tree a = Empty | Tree {
   node       :: a,
   leftChild  :: Tree a,
   rightChild :: Tree a
} deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight t = 1 + max (treeHeight (leftChild t))
                       (treeHeight (rightChild t))

-- 9: direction type
data Direction = DLeft | DStraight | DRight
   deriving (Eq, Show)

-- 10: calculate direction change given 3 points
data Point = Point {
   x :: Int,
   y :: Int
} deriving (Eq, Show)

-- See "Graham scan" on Wikipedia for direction algorithm
turnDirection :: Point -> Point -> Point -> Direction
turnDirection a b c = case (compare grahamDirection 0) of
      LT -> DRight
      EQ -> DStraight
      GT -> DLeft
   where grahamDirection = (x b - x a) * (y c - y a)
                         - (y b - y a) * (x c - x a)

-- 11: given list of points, compute direction of each triple
-- (I'm sure there's a better way to handle the !!)
tripleDirections :: [Point] -> [Direction]
tripleDirections ps
   | length ps < 3 = []  -- no turns if less than 3
   | otherwise     = [turnDirection (ps !! 0) (ps !! 1) (ps !! 2)]
                     ++ tripleDirections (tail ps)

-- 12: full Graham scan algorithm to find convex hull
-- First step - find point with lowest y-coord, taking lowest x on a tie
-- (Can be improved - no need to sort twice)
minPoint :: [Point] -> Point
minPoint [] = error "Need at least one point"
minPoint ps = head (sortBy (compare `on` y)
                   (sortBy (compare `on` x) ps))

-- Next step - sort in order of angle the point and minPoint make from x axis
-- According to Wikipedia, can use cotangent
cot :: Point -> Point -> Float
cot p1 p2 = adj / opp
   where minp = minPoint [p1, p2]
         maxp = head ([p1, p2] \\ [minp])
         adj = fromIntegral $ x maxp - x minp
         opp = fromIntegral $ y maxp - y minp

-- Helper - drop points that would lead to right turns
-- (again - has to be a better way than !! thrice)
dropRightTurns :: [Point] -> [Point]
dropRightTurns ps
   | length ps < 3      = ps  -- done
   | nextTurn == DRight = (head ps) : dropRightTurns (drop 2 ps)
   | otherwise          = (head ps) : dropRightTurns (tail ps)
   where nextTurn = turnDirection (ps !! 0) (ps !! 1) (ps !! 2)

-- Pretty sure this is wrong
sortPoints :: [Point] -> [Point]
sortPoints [] = []
sortPoints ps = reverse $ sortBy (compare `on` cot minp) ps
   where minp = minPoint ps

-- Last step -- Generate list of points forming convex hull
convexHull :: [Point] -> [Point]
convexHull [] = []
convexHull ps = dropRightTurns $ sortPoints ps

-- Also writing an alternate version that does not keep >1 straight segments

-- Helper - drop points that would lead to non-left turns
-- (again - has to be a better way than !! thrice)
dropNonLeftTurns :: [Point] -> [Point]
dropNonLeftTurns ps
   | length ps < 3     = ps  -- done
   | nextTurn /= DLeft = (head ps) : dropNonLeftTurns (drop 2 ps)
   | otherwise         = (head ps) : dropNonLeftTurns (tail ps)
   where nextTurn = turnDirection (ps !! 0) (ps !! 1) (ps !! 2)

-- Last step -- Generate list of points forming convex hull
minimalConvexHull :: [Point] -> [Point]
minimalConvexHull [] = []
minimalConvexHull ps = dropNonLeftTurns sortedPoints
   where minp = minPoint ps
         sortedPoints = reverse $ sortBy (compare `on` cot minp) ps

-- Neither convex hull function is correct according to differing results:
-- http://marknelson.us/2007/08/22/convex/

-- Draw out results on console to see what's wrong
-- ... not even close.  result has all kinds of turns.
-- come back later.
