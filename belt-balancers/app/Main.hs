module Main where

import qualified Data.HashMap.Strict as M
import Data.List (replicate)

main :: IO ()
main = do
  let bs = allBalancers 2 2 5
  print bs

type Balancer = M.HashMap ElementName Element

type ElementName = String

data Element
  = InputElement !Input
  | SplitterElement !Splitter
  deriving (Show)

data Input = Input
  { inputNextElem :: Maybe ElementName
  }
  deriving (Show)

data Splitter = Splitter
  { splitterLeftInput :: Maybe ElementName,
    splitterRightInput :: Maybe ElementName,
    splitterLeftOutput :: Maybe ElementName,
    splitterRightOutput :: Maybe ElementName
  }
  deriving (Show)

allBalancers :: Int -> Int -> Int -> [Balancer]
allBalancers numInputs numOutputs maxNumSplitters =
  filter doesBalancerHaveFinalShape $ allBalancersStartingFrom [initBalancer]
  where
    initBalancer =
      M.fromList $
        zipWith
          (\idx input -> ("input" ++ show idx, input))
          [0 ..]
          (replicate numInputs $ InputElement $ Input Nothing)

    -- Returns all balancers that can be created by upgrading the given initial set of balancers.
    -- It also includes initial set of balancers in the returned balancers.
    -- It doesn't check that they work correctly, or that they have correct final shape
    -- regarding number of outputs.
    allBalancersStartingFrom [] = []
    allBalancersStartingFrom balancers =
      let nextBalancers' = filter (not . isBalancerTooBig) $ concat $ nextBalancers <$> balancers
       in balancers ++ allBalancersStartingFrom nextBalancers'

    -- Returns all balancers that can be created by upgrading the given balancer via exactly
    -- one upgrade, where upgrade is adding new splitter or adding a loop.
    -- TODO:
    -- Vrati sve sljedece moguce balancere koji se mogu dobiti nadogradnjom na postojeci balancer.
    -- Kako se radi nadogradnja?
    -- Imamo dvije moguce radnje:
    -- 1. Dodajemo novi splitter. One se spaja na jedan ili dva postojeca outputa. (adding new splitter).
    -- 2. Na postojeci splitter koji ima free input spojimo jedan od postojecih free outputa. (adding a loop).
    -- Probamo svaki od ovih poteza napraviti, i dobijemo listu balancera.
    nextBalancers :: Balancer -> [Balancer]
    nextBalancers balancer | length (balancerSplitters balancer) == maxNumSplitters = []
    nextBalancers balancer = [] -- TODO: Vrati nesto smisleno!
    --
    isBalancerTooBig balancer = length (balancerSplitters balancer) > maxNumSplitters

    doesBalancerHaveFinalShape balancer =
      length (balancerOutputs balancer) == numOutputs
        && length (balancerInputs balancer) == numInputs

balancerInputs :: Balancer -> [(ElementName, Input)]
balancerInputs balancer = M.toList $ M.mapMaybe toInputElem balancer
  where
    toInputElem e = case e of
      InputElement input -> Just input
      _ -> Nothing

balancerSplitters :: Balancer -> [(ElementName, Splitter)]
balancerSplitters balancer = M.toList $ M.mapMaybe toSplitterElem balancer
  where
    toSplitterElem e = case e of
      SplitterElement splitter -> Just splitter
      _ -> Nothing

data Output = Output ElementName OutputType

data OutputType
  = LeftSplitterOutput
  | RightSplitterOutput
  | InputOutput

splitterOutputs :: (ElementName, Splitter) -> [Output]
splitterOutputs (name, splitter) = case splitter of
  (Splitter _ _ Nothing Nothing) -> [Output name LeftSplitterOutput, Output name RightSplitterOutput]
  (Splitter _ _ (Just _) Nothing) -> [Output name RightSplitterOutput]
  (Splitter _ _ Nothing (Just _)) -> [Output name LeftSplitterOutput]
  (Splitter _ _ (Just _) (Just _)) -> []

inputOutputs :: (ElementName, Input) -> [Output]
inputOutputs (name, input) = case input of
  Input Nothing -> [Output name InputOutput]
  Input (Just _) -> []

balancerOutputs :: Balancer -> [Output]
balancerOutputs balancer =
  let inputs = balancerInputs balancer
      splitters = balancerSplitters balancer
   in concatMap inputOutputs inputs ++ concatMap splitterOutputs splitters
