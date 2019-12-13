{-# LANGUAGE RecordWildCards #-}

module Day12 where

import Data.Maybe
import Data.List

data CoOrd = CoOrd Int Int Int deriving (Show, Eq)

(<+>) :: CoOrd -> CoOrd -> CoOrd
(CoOrd x y z) <+> (CoOrd x' y' z') = CoOrd (x + x') (y + y') (z + z')

data Moon = Moon {position :: CoOrd,
                  velocity :: CoOrd} deriving (Show, Eq)

newMoon :: CoOrd -> Moon
newMoon p = Moon {position = p, velocity = CoOrd 0 0 0}

move :: Moon -> Moon
move m@Moon{..} = m {position = position <+> velocity}

energy :: Moon -> Int
energy Moon {position = (CoOrd x y z), velocity = (CoOrd x' y' z')} = (abs x + abs y + abs z) * (abs x' + abs y' + abs z')

applyGravity :: [Moon] -> Moon -> Moon
applyGravity moons m2 = m2 {velocity = (velocity m2) <+> delta} where
  normDeltaPositions (CoOrd x2 y2 z2) (CoOrd x1 y1 z1) = CoOrd (signum $ x1-x2) (signum $ y1-y2) (signum $ z1-z2)
  deltas = map (normDeltaPositions (position m2)) (map position moons)
  delta = foldr1 (<+>) deltas

input :: [Moon]
input = [newMoon (CoOrd 3 15 8),
         newMoon (CoOrd 5 (-1) (-2)),
         newMoon (CoOrd (-10) 8 2),
         newMoon (CoOrd 8 4 (-5))]

step :: [Moon] -> [Moon]
step moons = map (move . applyGravity moons) moons

part1 :: Int
part1 = sum . (map energy) $ iterate step input !! 1000

period :: [Moon] -> (Moon -> (Int, Int)) -> Int
period moons componentFunc = (+) 1 . fromJust . findIndex isRepeated . tail . map (map componentFunc) $ iterate step moons where
  isRepeated = (== (map componentFunc moons))

part2 :: Int
part2 = foldr1 lcm . map (period input)
  $ [(\(Moon (CoOrd x _ _) (CoOrd x' _ _)) -> (x,x')),
     (\(Moon (CoOrd _ y _) (CoOrd _ y' _)) -> (y,y')),
     (\(Moon (CoOrd _ _ z) (CoOrd _ _ z')) -> (z,z'))]
