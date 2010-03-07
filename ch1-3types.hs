module MyTypes where

data Book2 = Book2 Int String
   deriving (Show)

sampleBook2 = Book2 2 "Hi there book2"

-- Don't need any data inside the data type
data Book0 = Book0
   deriving (Show)

-- These two are not equal
sampleBook0 = Book0
sampleBook0_2 = Book0

-- Simple synonym for an existing type
type MyInt = Int

-- These two ARE equal
sampleMyInt = 42 :: MyInt
sampleMyInt_2 = 42 :: MyInt

-- Above two are not equal to this
sampleMyInt_3 = 43 :: MyInt

-- Why doesn't the next line work?  says malformed type declaration.
--type newBook0 = (String, Int)

-- Also can't do this?
--type newBook0 = Book0

----  Answer: types need to start with a capital ----

type NewBook0 = Book0
type ComboType = (Book0, Book2)

-- Algebraic data types, i.e. "alternatives"
-- Bool is defined as:  data Bool = False | True
type Name = String
type Address = [String]
type AccountNumber = Int
type PayPalAccount = String
type CreditCardNumber = Int

-- You can create a BillingInfo object using any of the three methods
-- below, including Freebie with no arguments.

data BillingInfo = CreditCard CreditCardNumber Name Address
                 | PayPal PayPalAccount
                 | Freebie
     deriving (Show)

-- deconstruct a data type
-- Uses wild card "_" so it doesn't bind unnecessary variables
book2ID    (Book2 id _    ) = id
book2Title (Book2 _  title) = title

-- Call like this:
-- book2ID sampleBook2  (returns "Hi there book2")

-- record syntax, automatically gives accessors
data Book3 = Book3 {
   bookID    :: Int,
   bookTitle :: String
} deriving (Show)

-- What does GHC pick as type if there are two names?
type T1 = Int  -- picked Int unless we set the var using :: T1

data List a = Cons a (List a)
              | Nil
   deriving (Show)

data Tree a = Empty | Tree {
   node       :: a,
   leftChild  :: Tree a,
   rightChild :: Tree a
} deriving (Show)

-- Goal: ch3 ex2, develop tree type using Maybe so we only have one constructor
-- It works so far if we hardcode ints...
data MaybeTreeInt = MaybeTreeInt {
   intNode       :: Maybe Int,
   intLeftChild  :: Maybe MaybeTreeInt,
   intRightChild :: Maybe MaybeTreeInt
} deriving (Show)

-- This works with type variables, the difficulty was "just" in Justing the
-- children
data MaybeTree a = MaybeTree {
   maybeNode       :: a,
   maybeLeftChild  :: Maybe (MaybeTree a),
   maybeRightChild :: Maybe (MaybeTree a)
} deriving (Show)
