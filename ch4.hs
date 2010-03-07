-- Redefine standard functions for practice

mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mynull :: [a] -> Bool
mynull [] = True
mynull _  = False

myhead :: [a] -> a
myhead [] = error "empty list"
myhead (x:xs) = x

mytail :: [a] -> [a]
mytail [] = error "empty list"
mytail (x:xs) = xs

mylast :: [a] -> a
mylast [] = error "empty list"
mylast (x:[]) = x
mylast (x:xs) = mylast xs

myinit :: [a] -> [a]
myinit [] = error "empty list"
myinit (x:[]) = []
myinit (x:xs) = x : myinit xs

-- ++
myappend :: [a] -> [a] -> [a]
myappend [] y = y
myappend x  y = (head x) : myappend (tail x) y

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = (myreverse xs) ++ [x]

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs

myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs

myall :: (a -> Bool) -> [a] -> Bool
myall _ [] = True
myall f (x:xs) = f x && myall f xs

myany :: (a -> Bool) -> [a] -> Bool
myany _ [] = False
myany f (x:xs) = f x || myany f xs

mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake 0 _  = []
mytake n (x:xs) = x : (mytake (n - 1) xs)

mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop 0 x  = x
mydrop n (x:xs) = mydrop (n - 1) xs

mysplitAt :: Int -> [a] -> ([a], [a])
mysplitAt n xs = (take n xs, drop n xs)

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile f (x:xs) = if f x then x : mytakeWhile f xs
                              else []

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile _ [] = []
mydropWhile f (x:xs) = if f x then mydropWhile f xs
                              else (x:xs)

myspan :: (a -> Bool) -> [a] -> ([a], [a])
myspan f xs = (takeWhile f xs, dropWhile f xs)

mybreak :: (a -> Bool) -> [a] -> ([a], [a])
mybreak f xs = (dropWhile f xs, takeWhile f xs)

myelem :: (Eq a) => a -> [a] -> Bool
myelem _ [] = False
--myelem e (x:xs) = e == x || myelem e xs
myelem e xs = any ((==) e) xs

mynotElem :: (Eq a) => a -> [a] -> Bool
mynotElem _ [] = True
--mynotElem e (x:xs) = e /= x && mynotElem e xs
--mynotElem e xs = all ((/=) e) xs
mynotElem e xs = not (myelem e xs)

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter f (x:xs) = if f x then x : myfilter f xs
                           else     myfilter f xs

myisPrefixOf :: Eq a => [a] -> [a] -> Bool
myisPrefixOf [] _  = True
myisPrefixOf _  [] = False
myisPrefixOf xs ys = if head xs == head ys
                     then myisPrefixOf (tail xs) (tail ys)
                     else False

myisInfixOf :: Eq a => [a] -> [a] -> Bool
myisInfixOf [] _  = True
myisInfixOf _  [] = False
myisInfixOf xs ys = (head xs == head ys
                      && myisPrefixOf (tail xs) (tail ys)
                    ) || myisInfixOf        xs  (tail ys)

myisSuffixOf :: Eq a => [a] -> [a] -> Bool
myisSuffixOf [] _  = True
myisSuffixOf _  [] = False
myisSuffixOf xs ys = xs == ys || myisSuffixOf xs (tail ys)
