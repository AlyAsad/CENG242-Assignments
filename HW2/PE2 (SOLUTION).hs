module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]


---------------------------------------------------------------------------------------------------------------

-- general helpers

-- takes hp, gold, and encounters list and returns final hp and gold
evalEnc :: Integer -> Integer -> [Encounter] -> (Integer, Integer)

--if no encounters left
evalEnc hp gold [] = (hp, gold)

--if fight encounter
evalEnc hp gold ((Fight (Enemy _ damage loot)):rest) = evalEnc (hp - damage) (gold + loot) rest

--if gold encounter
evalEnc hp gold ((Treasure (Gold loot)):rest) = evalEnc hp (gold + loot) rest

--if potion encounter
evalEnc hp gold ((Treasure (Potion loot)):rest) = evalEnc (hp + loot) gold rest


---------------------------------------------------------------------------------------------------------------
--{
--helpers for #1
help1 :: Integer -> Dungeon -> [Int] -> Integer -> (Integer, Integer)
--if tree empty
help1 hp EmptyTree _ gold = (hp, gold)

--if leaf
help1 hp (Leaf _ encounters) _ gold = evalEnc hp gold encounters

--if node and path ended
help1 hp (Node _ encounters _) [] gold = evalEnc hp gold encounters

--if node but remaining path
help1 hp (Node _ encounters children) (curr:rest) gold = help1 finalHP (children !! curr) rest finalGold
                            where temp = evalEnc hp gold encounters
                                  finalHP = fst temp
                                  finalGold = snd temp


-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp dungMap path = help1 hp dungMap path 0
--}
---------------------------------------------------------------------------------------------------------------
--{
--helpers for #2
help2 :: Integer -> Dungeon -> Integer -> [Integer] -> [Integer]

--if tree empty
help2 _ EmptyTree _ final = final

--if leaf
help2 hp (Leaf _ encounters) gold final = if (finalHP > 0) 
                                then (final ++ [finalGold])
                                else (final ++ [gold])
                                
                                where temp = evalEnc hp gold encounters
                                      finalHP = fst temp
                                      finalGold = snd temp

--if node
help2 hp (Node _ encounters children) gold final = if (finalHP > 0)
                                                    then (final ++ (help2a finalHP finalGold children))
                                                    else (final ++ [gold])
                                                    
                                                    where temp = evalEnc hp gold encounters
                                                          finalHP = fst temp
                                                          finalGold = snd temp


-- iterates over the children
help2a _ _ [] = []
help2a hp gold (curr:rest) = (help2 hp curr gold []) ++ (help2a hp gold rest)

-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated
findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp dungMap = maximum (help2 hp dungMap 0 [])
--}
---------------------------------------------------------------------------------------------------------------
--{
--helpers for #3

--iterates over children to see which are fine
help3 _ [] = []

help3 hp (curr:rest) = (help3a hp curr) ++ (help3 hp rest)

--if leaf
help3a hp (Leaf name encounters) = if (finalHP > 0)
                                then [Leaf name encounters]
                                else []
                                where finalHP = fst (evalEnc hp 0 encounters)

--if node
help3a hp (Node name encounters children) = if (finalHP <= 0)
                                            then []
                                            else if (newChildren == [])
                                                then [Leaf name encounters]
                                                else [Node name encounters newChildren]
                                            
                                            where finalHP = fst (evalEnc hp 0 encounters)
                                                  newChildren = help3 finalHP children


-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
--if root is empty
findViablePaths hp EmptyTree = EmptyTree

--if root is leaf
findViablePaths hp (Leaf name encounters) = if (finalHP > 0)
                                then (Leaf name encounters)
                                else EmptyTree
                                where finalHP = fst (evalEnc hp 0 encounters)

--if root is node
findViablePaths hp (Node name encounters children) = if (finalHP <= 0)
                                then EmptyTree
                                else if (newChildren == [])
                                    then (Leaf name encounters)
                                    else (Node name encounters newChildren)
                                    
                                where finalHP = fst (evalEnc hp 0 encounters)
                                      newChildren = help3 finalHP children
--}
---------------------------------------------------------------------------------------------------------------
--{
--helpers for #4
------------------------------------------------------
--main helper, its evaluating if the dungeon is empty or leaf or node, and also if the current node should become the root
main4 :: Dungeon -> Integer -> ((Integer, Dungeon), Integer)
--if no viable path
main4 EmptyTree _ = ((0, EmptyTree), 0)

--if leaf
main4 (Leaf name encounters) _ = ((0, Leaf name encounters), 0)

--if node with only one child
main4 (Node name encounters [child]) distFromRoot = if (snd temp == 1)
                                                    then temp
                                                    else (((fst (fst temp)) + 1, Node name encounters [snd (fst temp)]), 0)
                                                    where temp = main4 child (distFromRoot + 1)
                                                      

--if node with more than one child
main4 (Node name encounters children) distFromRoot = if (snd eval1Child == 1) then eval1Child
                                                     else if (snd eval2Child == 1) then eval2Child
                                                     else if (maxDist == firstFromRoot) then ((best1ChildDepth + 1, Node name encounters [snd (fst eval1Child)]), 0)
                                                     else if (maxDist == secFromRoot) then ((best2ChildDepth + 1, Node name encounters [snd (fst eval2Child)]), 0)
                                                     else ((distIfCurrBecomesRoot, Node name encounters [snd (fst eval1Child), snd (fst eval2Child)]), 1)
                                                     
                                                     where ((best1ChildDepth,best1Child), (best2ChildDepth, best2Child)) = getMaxDepth children
                                                           eval1Child = main4 best1Child (maximum [distFromRoot + 1, best2ChildDepth + 1])
                                                           eval2Child = main4 best2Child (maximum [distFromRoot + 1, best1ChildDepth + 1])
                                                           firstFromRoot = best1ChildDepth + distFromRoot + 1
                                                           secFromRoot = best2ChildDepth + distFromRoot + 1
                                                           distIfCurrBecomesRoot = best1ChildDepth + best2ChildDepth + 2
                                                           maxDist = maximum [distIfCurrBecomesRoot, firstFromRoot, secFromRoot]
------------------------------------------------------

--finds the max depth of a node
getDepth (Leaf _ _) = 0
getDepth (Node _ _ children) = 1 + (maximum (map getDepth children))

------------------------------------------------------
--takes children as input and finds the 2 max depth descendents of a node
getMaxDepth [] = ((-1, EmptyTree), (-1, EmptyTree))

getMaxDepth (curr:rest) = final
                where currDepth = getDepth curr
                      ((firstDepth, firstPath), (secDepth, secPath)) = getMaxDepth rest
                      mostMax = maximum [currDepth, firstDepth, secDepth]
                      final | (mostMax == currDepth && firstDepth > secDepth) || (mostMax == firstDepth && currDepth > secDepth) = ((currDepth, curr), (firstDepth, firstPath))
                            | (mostMax == currDepth) || (mostMax == secDepth && currDepth > firstDepth) = ((currDepth, curr), (secDepth, secPath))
                            | otherwise = ((firstDepth, firstPath), (secDepth, secPath))
                      

------------------------------------------------------
-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp dungMap = fst (main4 (findViablePaths hp dungMap) 0)


--}

---------------------------------------------------------------------------------------------------------------
--{

--helpers for #5

------------------------------------------------------
--finds the total hp cost and gold of entire tree and returns it (hp, gold)
findHPandGold :: Dungeon -> (Integer, Integer)
--if leaf
findHPandGold (Leaf name encounters) = evalEnc 0 0 encounters
--if node
findHPandGold (Node name encounters children) = (finalHP, finalGold)
                                        where finalHP = (fst currTuple) + (fst childrenTuple)
                                              finalGold = (snd currTuple) + (snd childrenTuple)
                                              currTuple = evalEnc 0 0 encounters
                                              childrenTuple = addUp 0 0 (evalChildren children)

------------------------------------------------------
--finds max efficiency from a tree
findMaxEff :: Dungeon -> Double
findMaxEff (Leaf name encounters) = findEff (evalEnc 0 0 encounters)

findMaxEff (Node name encounters children) = if (currEff > childrenMaxEff)
                                            then currEff
                                            else childrenMaxEff
                                            
                                            where currEff = findEff (findHPandGold (Node name encounters children))
                                                  childrenMaxEff = maximum (map findMaxEff children)

------------------------------------------------------
--converts (hp,gold) tuple into efficiency
findEff :: (Integer, Integer) -> Double
findEff (hp, gold) = if (hp < 0) 
                        then (fromIntegral gold)/(-(fromIntegral hp))
                        else (1/0)

------------------------------------------------------
--returns list of (hp, gold) of children in order
evalChildren [] = []
evalChildren (curr:rest) = [findHPandGold curr] ++ evalChildren rest

------------------------------------------------------
--adds up the list of (hp,gold) tuple
addUp hp gold [] = (hp, gold)
addUp hp gold ((newHP, newGold):rest) = addUp (hp + newHP) (gold + newGold) rest

------------------------------------------------------
--finds best efficiency index from list
bestEffIndex bestIndex _ _ [] = bestIndex
bestEffIndex bestIndex currBest currIndex (curr:rest) = if (curr > currBest)
                                                    then bestEffIndex currIndex curr (currIndex + 1) rest
                                                    else bestEffIndex bestIndex currBest  (currIndex + 1) rest
                                                    
------------------------------------------------------

-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree EmptyTree = EmptyTree

mostEfficientSubtree (Leaf name encounters) = (Leaf name encounters)

mostEfficientSubtree (Node name encounters children) = if (currEff > bestChildEff)
                                                    then (Node name encounters children)
                                                    else mostEfficientSubtree (children !! bestChild)
                                                    
                                                    where currEff = findEff (findHPandGold (Node name encounters children))
                                                          childrenEff = (map findMaxEff children)
                                                          bestChild = bestEffIndex 0 (childrenEff !! 0) 0 childrenEff
                                                          bestChildEff = (childrenEff !! bestChild)

--}

