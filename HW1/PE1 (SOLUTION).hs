module PE1 where

import Text.Printf


-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving Show

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x


--helper for #1
help1 name (Price a b c:rest) = if name == a then [b,c] else help1 name rest

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost (name, qty) prices = getRounded (qty*price/amount) where
                                                            x = help1 name prices
                                                            amount = head x
                                                            price = last x


-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
recipeCost (Recipe _ ingList) prices = getRounded $ sum $ map (flip getIngredientCost prices) ingList



-- helpers for #3
-- iterating over stocks list to find current quantity
help3 _ [] = 0
help3 ingName ((curr, qty):rest) = if ingName == curr then qty else help3 ingName rest

help3a [] _ final = final
help3a ((ingName, ingQty):rest) pantryList final = if pantryQty < ingQty 
                                                    then help3a rest pantryList (final ++ [(ingName, getRounded (ingQty - pantryQty))])
                                                    else help3a rest pantryList final
                                                    where pantryQty = (help3 ingName pantryList)

-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe
missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients (Recipe _ ingList) pantryList = help3a ingList pantryList []



--helper for #4
help4 [] _ final = final
help4 ((pantryName, pantryQty):rest) recipeList final = if finalQty == 0
                            then help4 rest recipeList final
                            else help4 rest recipeList (final ++ [(pantryName, finalQty)])
                            where finalQty = getRounded (pantryQty - help3 pantryName recipeList)
                            

help4a [] final = final
help4a ((a, b):rest) final = if num == 0
                                then help4a rest final
                                else help4a rest (final ++ [(a,num)])
                                where num = getRounded b

-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe pantryList (Recipe _ recipeList) = if missingIngredients (Recipe "" recipeList) pantryList == []
                                then help4 pantryList recipeList []
                                else help4a pantryList []


--helpers for #5

--iterate over prices one by one
help5 _ _ [] final = final
help5 pantryList listOfRecipes ((Price priceName priceQty priceCost):rest) final = 
            if reqQty > 0
                then help5 pantryList listOfRecipes rest 
                (final ++ [(priceName, reqQty, getIngredientCost (priceName, reqQty)
                [Price priceName priceQty priceCost])])
                
                else help5 pantryList listOfRecipes rest final
                
            where 
             reqQty = getRounded ((help5a priceName listOfRecipes 0) - (help3 priceName pantryList))

--iterate over recipes one by one to find total quantity of a specific ingredient
help5a _ [] final = final
help5a ingName ((Recipe _ recipeList):rest) final = help5a ingName rest (final + help3 ingName recipeList)


-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
makeShoppingList pantryList listOfRecipes listOfPrices = help5 pantryList listOfRecipes listOfPrices []

