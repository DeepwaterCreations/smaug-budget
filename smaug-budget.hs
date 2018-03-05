module SmaugBudget
( Coinheap(..)
, addCoins
) where

-- What it should do:

-- -There are buckets of money. Call them "coinheaps". 
-- I want to be able to add money to heaps, and subtract money from heaps.
-- I want to be able to make records over time of these transactions.
-- There should also be a "cental coinheap" where unallocated money goes.
--
-- A single transaction has:
--  A target heap
--  An amount
--  A comment describing the purpose: What was bought, where did the income come from, etc.
--  A date!
--
data Transaction = Transaction { amount :: Double
                               , comment :: String } deriving (Show)
-- A coinheap has:
--  A name
--  A description/notes
--  An amount
--  An optional default transaction?
--

data Coinheap = Coinheap {name :: String
                         ,description :: String
                         ,transactions :: [Transaction]} deriving (Show)

--Get an empty coinheap
emptyHeap :: String -> String -> Coinheap
emptyHeap name desc = Coinheap name desc []

-- Add some amount of money to a coinheap
addCoins :: Coinheap -> Double -> String -> Coinheap
addCoins (Coinheap name desc transactions) addedAmount reason = Coinheap name desc (newTrans:transactions)
    where newTrans = Transaction addedAmount reason

-- Remove some amount of money from a coinheap
subtCoins :: Coinheap -> Double -> String -> Coinheap
subtCoins coinheap subtAmount reason = addCoins coinheap (-subtAmount) reason

-- Get the current balance of a coinheap
size :: Coinheap -> Double
size coinheap = foldl (\acc t -> acc + (amount t)) 0 transactionList
    where transactionList = transactions coinheap

--  ...but that's all very OOP, and maybe the wrong way to frame it?
--
--  A coinheap is mostly a set of transactions and a description.
--  Function: Takes a coinheap, an amount, and a comment, and adds that transaction to the heap. 
--            Ditto but subtracts.
--            Takes a coinheap, and returns a value
--
--  Basically, I'm adding tags to amounts of money. "Coinheap" is just a tag.
--  Functions:
--      Add money with a tag and comment, possibly negative value
--      Sum all transactions with a given tag/set of tags?/more complicated?
--      List comments of all transactions with a given tag
--      Ditto dateranges
--      Display all tags in use
--      Also edit/display comments on tags
--
--      Will my bank send emails or something with my balance, for automatic reconciliation?
--
--
--  Command line:
--      List heaps
--      Add a heap
--      Delete a heap
--      Print a name/desc
--      Edit a heap name/desc
--      Put money in a heap
--      Take money from a heap
--      Return present size of heap
--      Transfer money between two heaps
--      List transactions for a heap, or overall
--      Delete/edit previous transactions
--      Show projections into the future?
--      Dump database to a plaintext format or something?
--
--      Hey, what's this look like as a monad/monoid/all that jazz that I still haven't fully grokked?
--
--      TODO:
--      Don't use the ArchHaskell repository, it allegedly sucks
--      http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html
--      Get the haskell mongodb bindings: https://github.com/mongodb-haskell/mongodb
--      Implement away!
