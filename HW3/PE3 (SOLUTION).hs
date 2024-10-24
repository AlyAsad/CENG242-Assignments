{-# LANGUAGE FlexibleInstances #-}

module PE3 where

import Data.List (sort, sortBy)
import Text.Printf (printf)

data Term = Const Integer | Pw Integer Power | Trig Integer Power Trigonometric | Exp Integer Power Exponential

data Power = Power Integer
data Polynomial = Polynomial [(Integer, Power)]
data Exponential = Exponential Polynomial
data Trigonometric = Sin Polynomial | Cos Polynomial

class Evaluable a where
    function :: a -> (Integer -> Double)

class Differentiable a where
    derivative :: a -> [Term]

-- You can use this as is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- You don't have to follow the order the functions appear in the file
-- For example, you could first define all Show instances, then all Eq instances etc.
-- if that implementation order is more convenient for you.



--helpers
--{
------------------------------------------------

-- checks if "^" or "+" is in the string
inString :: [Char] -> Bool
inString [] = False
inString (curr:rest) = if ((curr == '^') || (curr == '+') || (curr == '-'))
                            then True
                            else (inString rest)

------------------------------------------------

--}

---------------------------------------------------------------------------------


-- INSTANCES FOR POWER
--{
instance Show Power where
    show (Power pow) = if (pow == 0)
                        then "1"
                        else if (pow == 1)
                        then "x"
                        else "x^" ++ (show pow)

instance Eq Power where
    (Power x) == (Power y) = (x == y)

instance Ord Power where
    (Power x) <= (Power y) = ( x <= y)


instance Evaluable Power where
    function (Power pow) = \x -> fromIntegral (x^pow)

instance Differentiable Power where
    derivative (Power pow) = if (pow == 0)
                            then [Const 0]
                            else if (pow == 1)
                            then [Const 1]
                            else [Pw pow (Power (pow-1))]
--}


---------------------------------------------------------------------------------


-- INSTANCES FOR POLYNOMIAL
--{

--helper
instance Eq Polynomial where
    (Polynomial x) == (Polynomial y) = ((sort x) == (sort y))


instance Show Polynomial where
    show (Polynomial []) = "0"
    show (Polynomial ((coeff, pow):rest)) = if (final == "0")
                                            then "0"
                                            else final
                                            
                                            where
                                            term = (show pow)
                                            curr | coeff == 0 = "0"
                                                 | coeff == 1 = term
                                                 | term == "1" = (show coeff)
                                                 | coeff == -1 = "-" ++ term
                                                 | otherwise = (show coeff) ++ term
                                            
                                            restEq = show (Polynomial rest)
                                            final | restEq == "0" = curr
                                                  | otherwise = (curr ++ " + " ++ restEq)


instance Evaluable Polynomial where
    function (Polynomial []) = \x -> 0.0
    function (Polynomial ((coeff, Power pow):rest)) = \x -> ((fromIntegral (coeff*(x^pow))) + (function (Polynomial rest) x))

instance Differentiable Polynomial where
    derivative (Polynomial []) = []
    derivative (Polynomial ((coeff, pow):rest)) = case curr of
                                                Const 0 -> derivative (Polynomial rest)
                                                Const 1 -> [Const coeff] ++ (derivative (Polynomial rest))
                                                Pw coeff2 pow -> [Pw (coeff*coeff2) pow] ++ (derivative (Polynomial rest))
                                                
                                                where
                                                [curr] = derivative pow

--}


---------------------------------------------------------------------------------


-- INSTANCES FOR EXPONENTIAL
--{

--helpers
polyToExp :: Polynomial -> [Term] -> [Term]

polyToExp _ [] = []
polyToExp poly ((Const 0):rest) = polyToExp poly rest
polyToExp poly ((Const coeff):rest) = [Exp coeff (Power 0) (Exponential poly)] ++ polyToExp poly rest
polyToExp poly ((Pw coeff pow):rest) = [Exp coeff pow (Exponential poly)] ++ polyToExp poly rest

------------------------------------------------

instance Show Exponential where
    show (Exponential poly) = final
                                where
                                pow = (show poly)
                                final
                                    | pow == "0" = "1"
                                    | pow == "1" = "e"
                                    | (inString pow) == False = "e^" ++ pow
                                    | otherwise = "e^(" ++ pow ++ ")"



instance Evaluable Exponential where
    function (Exponential poly) = \x -> getRounded (exp (function poly x))

instance Differentiable Exponential where
    derivative (Exponential poly) = polyToExp poly (derivative poly)
--}


---------------------------------------------------------------------------------


-- INSTANCES FOR TRIGONOMETRIC
--{

--helpers
polyToSin :: Polynomial -> [Term] -> [Term]

polyToSin _ [] = []
polyToSin poly ((Const 0):rest) = polyToSin poly rest
polyToSin poly ((Const coeff):rest) = [Trig (-coeff) (Power 0) (Sin poly)] ++ polyToSin poly rest
polyToSin poly ((Pw coeff pow):rest) = [Trig (-coeff) pow (Sin poly)] ++ polyToSin poly rest

polyToCos :: Polynomial -> [Term] -> [Term]
polyToCos _ [] = []
polyToCos poly ((Const 0):rest) = polyToCos poly rest
polyToCos poly ((Const coeff):rest) = [Trig coeff (Power 0) (Cos poly)] ++ polyToCos poly rest
polyToCos poly ((Pw coeff pow):rest) = [Trig coeff pow (Cos poly)] ++ polyToCos poly rest



instance Show Trigonometric where
    show (Sin poly) = "sin" ++ final
                        
                        where
                        eq = show poly
                        final | (inString eq) == False = eq
                              | otherwise = "(" ++ eq ++ ")"
                        
    
    show (Cos poly) = "cos" ++ final
                        
                        where
                        eq = show poly
                        final | (inString eq) == False = eq
                              | otherwise = "(" ++ eq ++ ")"


instance Evaluable Trigonometric where
    function (Sin poly) = \x -> getRounded (sin (function poly x))
    
    function (Cos poly) = \x -> getRounded (cos (function poly x))

instance Differentiable Trigonometric where
    derivative (Sin poly) = polyToCos poly (derivative poly)
    
    derivative (Cos poly) = polyToSin poly (derivative poly)
--}


---------------------------------------------------------------------------------


-- INSTANCES FOR TERM
--{

--helpers
makeFinal :: Integer -> Power -> [Term] -> [Term]
makeFinal _ _ [] = []
makeFinal coeff (Power pow) ((Trig coeff2 (Power pow2) trig):rest) = ([Trig (coeff*coeff2) (Power (pow+pow2)) trig]
                                                                        ++ (makeFinal coeff (Power pow) rest))


makeFinal2 :: Integer -> Power -> [Term] -> [Term]
makeFinal2 _ _ [] = []
makeFinal2 coeff (Power pow) ((Exp coeff2 (Power pow2) expTerm):rest) = ([Exp (coeff*coeff2) (Power (pow+pow2)) expTerm]
                                                                        ++ (makeFinal2 coeff (Power pow) rest))



---------------------------------------------



instance Show Term where
    show (Const coeff) = show coeff
    
    show (Pw coeff pow) = show (Polynomial [(coeff, pow)])
    
    show (Trig coeff pow trig) = if (left == "0")
                                 then "0"
                                 else  if (left == "1")
                                 then (show trig)
                                 else if (left == "-1")
                                 then ("-" ++ (show trig))
                                 else left ++ (show trig)
                                 
                                where left = (show (Polynomial [(coeff, pow)]))
    
    show (Exp coeff pow expTerm) = if (left == "0")
                                 then "0"
                                 else  if (left == "1")
                                 then (show expTerm)
                                 else if (left == "-1")
                                 then ("-" ++ (show expTerm))
                                 else left ++ (show expTerm)
                                 
                                where left = (show (Polynomial [(coeff, pow)]))

instance Evaluable Term where
    function (Const coeff) = \x -> fromInteger coeff
    
    function (Pw coeff pow) = \x -> (function (Polynomial [(coeff, pow)]) x)
    
    function (Trig coeff pow trig) = \x -> getRounded ((function (Polynomial [(coeff, pow)]) x) * (function trig x))
    
    function (Exp coeff pow expTerm) = \x -> getRounded ((function (Polynomial [(coeff, pow)]) x) * (function expTerm x))

instance Differentiable Term where
    derivative (Const coeff) = [Const 0]
    
    derivative (Pw coeff pow) = derivative (Polynomial [(coeff, pow)])
    
    derivative (Trig coeff pow trig) = if (coeff == 0)
                                        then [Const 0]
                                        else case powDiff of
                                        Const 0 -> restFinal
                                        Const 1 -> [Trig coeff (Power 0) trig] ++ restFinal
                                        (Pw coeff2 pow2) -> [Trig (coeff*coeff2) pow2 trig] ++ restFinal
                                        
                                        where
                                        [powDiff] = derivative pow
                                        restFinal = makeFinal coeff pow (derivative trig)


    derivative (Exp coeff pow expTerm) = if (coeff == 0)
                                        then [Const 0]
                                        else case powDiff of
                                        Const 0 -> restFinal
                                        Const 1 -> [Exp coeff (Power 0) expTerm] ++ restFinal
                                        (Pw coeff2 pow2) -> [Exp (coeff*coeff2) pow2 expTerm] ++ restFinal
                                        
                                        where
                                        [powDiff] = derivative pow
                                        restFinal = makeFinal2 coeff pow (derivative expTerm)
--}


---------------------------------------------------------------------------------


-- INSTANCES FOR [TERM]
--{
removeIndex _ _ [] = []
removeIndex currI indexes (curr:rest) = if (elem currI indexes)
                                    then (removeIndex (currI+1) indexes rest)
                                    else ([curr] ++ (removeIndex (currI+1) indexes rest))


-------------------------------------------------------------

--finds similar terms in list and gives their indexes
find :: Int -> Term -> [Term] -> [Int]
find _ _ [] = []
find currI (Const _) (curr:rest) = case curr of
                                        Const _ -> ([currI] ++ (find (currI+1) (Const 0) rest))
                                        _ -> (find (currI+1) (Const 0) rest)

find currI (Pw _ pow) (curr:rest) = case curr of 
                                                (Pw _ pow2) -> if (pow == pow2)
                                                    then ([currI] ++ (find (currI+1) (Pw 0 pow) rest))
                                                    else (find (currI+1) (Pw 0 pow) rest)
                                                _ -> (find (currI+1) (Pw 0 pow) rest)

find currI (Trig _ pow (Sin poly)) (curr:rest) = case curr of
                                        (Trig _ pow2 (Sin poly2)) -> if ((pow == pow2) && (poly == poly2))
                                                    then ([currI] ++ (find (currI+1) (Trig 0 pow (Sin poly)) rest))
                                                    else (find (currI+1) (Trig 0 pow (Sin poly)) rest)
                                        _ -> (find (currI+1) (Trig 0 pow (Sin poly)) rest)

find currI (Trig _ pow (Cos poly)) (curr:rest) = case curr of
                                        (Trig _ pow2 (Cos poly2)) -> if ((pow == pow2) && (poly == poly2))
                                                    then ([currI] ++ (find (currI+1) (Trig 0 pow (Cos poly)) rest))
                                                    else (find (currI+1) (Trig 0 pow (Cos poly)) rest)
                                        _ -> (find (currI+1) (Trig 0 pow (Cos poly)) rest)

find currI (Exp _ pow (Exponential poly)) (curr:rest) = case curr of
                                        (Exp _ pow2 (Exponential poly2)) -> if ((pow == pow2) && (poly == poly2))
                                                    then ([currI] ++ (find (currI+1) (Exp 0 pow (Exponential poly)) rest))
                                                    else (find (currI+1) (Exp 0 pow (Exponential poly)) rest)
                                        _ -> (find (currI+1) (Exp 0 pow (Exponential poly)) rest)


-------------------------------------------------------------

--adds up terms in specified indexes
addTerms [] = (Const 0, 0)

addTerms ((Const x):rest) = (Const result, result) where result = (x + (snd (addTerms rest)))

addTerms ((Pw coeff pow):rest) = (Pw result pow, result) where result = (coeff + (snd (addTerms rest)))

addTerms ((Trig coeff pow trig):rest) = (Trig result pow trig, result) where result = (coeff + (snd (addTerms rest)))

addTerms ((Exp coeff pow expTerm):rest) = (Exp result pow expTerm, result) where result = (coeff + (snd (addTerms rest)))


-------------------------------------------------------------

--finds similar terms and adds them up and removes 0 terms
shorten :: [Term] -> [Term]
shorten [] = []
shorten terms = if (coeff == 0)
                then (shorten newTerms)
                else ([final] ++ (shorten newTerms))
                where
                indexes = find 0 (terms !! 0) terms
                newTerms = removeIndex 0 indexes terms
                (final, coeff) = addTerms (map ((!!) terms) indexes)

-------------------------------------------------------------

instance Evaluable [Term] where
    function terms = \x -> getRounded $ sum [func x | func <- (map (function) terms)]

instance Differentiable [Term] where
    derivative terms = shorten (concat (map derivative terms))
    


--}



