{-# LANGUAGE DeriveFunctor #-}
module Language.CFG(parse, CFG(CFG), Parse(Lit, Prod)) where

import Data.Function
import Data.Map as M (Map, findWithDefault, fromList, alter, lookup, empty, insert) 
import Data.Maybe 
import Debug.Trace
import Data.Foldable 

import Prelude hiding (foldl,foldr, concatMap)

-- | A context free grammar in Chomsky normal form. Contains the list of literal 
--   mappings, a list of all the productions and the dedicated 'stop' symbol.
data CFG a b = CFG [(a,b)] [(a, (a, a))] a deriving (Show, Eq)

-- | A possible parse of a string 
data Parse a b = 
    -- | A literal node that includes the original symbol and position in the input.
    Lit Int b a 

    -- | A production
    | Prod a (Parse a b) (Parse a b)
    
    deriving (Show, Eq, Functor)

sym :: Parse a b -> a
sym (Lit _ b val)  = val
sym (Prod val _ _) = val

type Board a b = Map (Int, Int) [Parse a b]

cell :: (Int, Int) -> Board a b -> [Parse a b]
cell = findWithDefault [] 

makeEdge :: (Foldable f, Eq b) => [(a, b)] -> f b -> (Int, Board a b)
makeEdge rules = foldl go (0,empty)
    where
        go  (i, accum) x= (i+1, (insert (i,0) (matchLiteral rules (i,x)) accum))

-- | Given a context free grammer and a string returns all the valid parses using
--   the CYK algorithm.
parse ::  (Eq a, Eq b,  Foldable f) => CFG a b -> f b -> [Parse a b]
parse cfg@(CFG _ _ s) input = filter isTerminated . cell (0,n-1) $ board
    where
        (n, board)   = buildBoard cfg input
        isTerminated = (==s) . sym


buildBoard :: (Eq a, Eq b,  Foldable f) => CFG a b -> f b -> (Int, Board a b)
buildBoard (CFG lit comp stop) input 
    = (n, foldl (updateCell comp) edge [(i,j,k) | i <- [1 .. n-1], 
                                                  j <- [0 .. n-i], 
                                                  k <- [0 .. i-1]])
    where
        (n, edge) = makeEdge lit input

updateCell :: (Eq a) => [(a, (a, a))] -> Board a b -> (Int, Int, Int)  -> Board a b
updateCell rules board (i,j,k) = alter go (j, i) board
    where
        go (Just old) = Just $ old ++ productCell rules left right
        go Nothing    = Just $ productCell rules left right
        left  = cell l_idx board
        right = cell r_idx board
        l_idx = (j,k)
        r_idx = (j+k+1, i-k-1)


matchLiteral :: Eq b => [(a, b)] -> (Int, b) ->  [Parse a b]
matchLiteral rules (i,lit) =  map (Lit i lit . fst) . filter ((==lit) . snd) $ rules

productCell :: Eq a => [(a, (a, a))] -> [Parse a b] -> [Parse a b] -> [Parse a b]
productCell rules left right = concatMap (match rules) [(l,r)| l <- left, r <- right]
 
match :: Eq a => [(a, (a, a))] -> (Parse a b, Parse a b) -> [Parse a b]
match rules (p1, p2) =  catMaybes $ map f rules 
    where 
        f (v, (l, r)) | sym p1 == l && sym p2 == r = Just $ Prod v p1 p2
                      | otherwise                          = Nothing

