{-# LANGUAGE DeriveFunctor #-}
module CFG where

import Data.Function
import Data.Map as M (Map, findWithDefault, fromList, alter, lookup) 
import Data.Maybe 
import Debug.Trace
import Control.DeepSeq

data CFG a b = CFG [(a,b)] [(a, (a, a))] a deriving (Show, Eq)

data Parse a b = Lit Int b a 
               | Prod a (Parse a b) (Parse a b)
               deriving (Show, Eq, Functor)

instance (NFData a, NFData b) => NFData (Parse a b) where
    rnf = const () . fmap rnf

instance NFData TL where
    rnf v = v `seq` ()

headVal :: Parse a b -> a
headVal (Lit _ b val)  = val
headVal (Prod val _ _) = val

data TL = A | B | C | D | S deriving (Show, Eq, Bounded, Enum)

testGrammer :: CFG TL Char
testGrammer = CFG lit comp S
    
lit = [(A, 'a'), (B, 'b'), (A,'b')]
comp = [(C, (B,B)), (D, (A,A)), (S, (C,D)), (A, (A,A)), (S, (A,A))]

testString :: String 
testString = "bbaa"        

type Board a b = Map (Int, Int) [Parse a b]

cell :: (Int, Int) -> Board a b -> [Parse a b]
cell = findWithDefault [] 

makeEdge :: Eq b => [(a, b)] -> [b] -> Board a b
makeEdge rules = fromList . zip idx . map (matchLiteral rules) . zip [0..]
    where
        idx = [(i,0)| i <- [0..]]

parse ::  (Show a, Eq a, Eq b, Show b) => CFG a b -> [b] -> [Parse a b]
parse cfg@(CFG _ _ s) input = filter isTerminated . cell (0,n-1) $ buildBoard cfg input
    where
        isTerminated = (==s) . headVal
        n            = length input


buildBoard :: (Show a, Eq a, Eq b, Show b) => CFG a b -> [b] -> Board a b
buildBoard (CFG lit comp stop) input 
    = foldl (debug) edge [(i,j,k) | i <- [1 .. n-1], 
                                    j <- [0 .. n-i], 
                                    k <- [0 .. i-1]]
    where
        edge = makeEdge lit input
        n    = length input
        debug accum x  = (updateCell comp accum x) 

updateCell :: (Show a, Eq a, Show b)=> [(a, (a, a))] -> Board a b -> (Int, Int, Int)  -> Board a b
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

productCell :: (Show a, Eq a) => [(a, (a, a))] -> [Parse a b] -> [Parse a b] -> [Parse a b]
productCell rules left right = concatMap (match rules) [(l,r)| l <- left, r <- right]
 
match :: Eq a => [(a, (a, a))] -> (Parse a b, Parse a b) -> [Parse a b]
match rules (p1, p2) =  catMaybes $ map f rules 
    where 
        f (v, (l, r)) | headVal p1 == l && headVal p2 == r = Just $ Prod v p1 p2
                      | otherwise                          = Nothing

x = [(i,j,k) | i <- [0..5], 
               j <- reverse [0..2], 
               k <- [1..(i)]]
