module Day14 where

import Util (rawInput, split)

import qualified Data.Map as M

type Quantity = Int
type Substance = String
type Ingredient = (Quantity, Substance)
type Recipes = M.Map Ingredient [Ingredient]

filename :: String
filename = "input/Day14.txt"

input :: Recipes
input = M.fromList . map parseLine . lines $ rawInput filename where
  parseLine line = (\(reagents:prod:_) -> (processProduct prod, processReagents reagents)) $ split (== '=') line
  processProduct = processIngredient . drop 2
  processReagents = map processIngredient . split (== ',')
  processIngredient = (\(a:b:_) -> (read a, b)) . split (== ' ')

minimalQuantity :: Quantity -> Quantity -> Quantity
minimalQuantity target recipeSize = target `div` recipeSize + (if target `mod` recipeSize == 0 then 0 else 1)

addIngredientsLists :: [Ingredient] -> [Ingredient] -> [Ingredient]
addIngredientsLists i1 i2 = map flipTuple . M.toList $ M.unionWith (+) (makeMap i1) (makeMap i2) where
  makeMap = M.fromList . map flipTuple
  flipTuple = (\(a, b) -> (b, a))

applyLeftovers :: [Ingredient] -> [Ingredient] -> ([Ingredient], [Ingredient])
applyLeftovers reagents leftovers = (unMakeMap $ M.differenceWith (\a b -> if a > b then Just $ a - b else Nothing) (makeMap reagents) (makeMap leftovers), unMakeMap $ M.differenceWith (\a b -> if a > b then Just $ a - b else Nothing) (makeMap leftovers) (makeMap reagents)) where
  makeMap = M.fromList . map flipTuple
  unMakeMap = map flipTuple . M.toList
  flipTuple = (\(a, b) -> (b, a))

findOreRequirement :: Recipes -> Ingredient -> [Ingredient] -> (Quantity, [Ingredient])
findOreRequirement _ (x, "ORE") leftovers = (x, leftovers)
findOreRequirement recipes (quant, substance) leftovers =
  (\(o, l) -> (o, addIngredientsLists l [((quantNeeded * (fst $ fst recipe)) - quant , substance)]))
  . foldl (\(orecount, leftovers') i -> let (q, l) = findOreRequirement recipes i leftovers' in (q + orecount, l) ) (0, leftovers') $ reagents' where
  recipe = head . M.toList $ M.filterWithKey (\k _ -> snd k == substance) recipes
  quantNeeded = minimalQuantity quant $ (fst $ fst recipe)
  (reagents', leftovers') = applyLeftovers (map (\(q, s) -> (quantNeeded * q, s)). snd $ recipe) leftovers

part1 :: Quantity
part1 = fst $ findOreRequirement input (1, "FUEL") []

howManyFuelCamWeMake :: Recipes -> Quantity -> Quantity -> [Ingredient] -> Quantity
howManyFuelCamWeMake recipes fuelMade oreCount leftovers
  | oreCount < (fst $ findOreRequirement recipes (1, "FUEL") leftovers)
    = fuelMade
  | otherwise
    = let (oreUsed, leftovers') = findOreRequirement recipes (1, "FUEL") leftovers in
      howManyFuelCamWeMake recipes (fuelMade + 1) (oreCount - oreUsed) leftovers'

part2 :: Quantity
part2 = howManyFuelCamWeMake input 0 1000000000000 []
