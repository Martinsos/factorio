module Main where

import qualified Data.HashMap.Strict as M
import Data.List (replicate)
import Data.Maybe (fromJust, fromMaybe)

-- TODO:
--   - Make basic code for generating balancers. [DONE]
--   - There seem to be too many solutions (doesn't stop printing) for 2 2 2 as input.
--     I must have done something wrong, figure out what.
--   - Make nice printing for balancers.
--   - Improve all code in this file to be nicer.
--   - Make logic for testing if balancer is balanced (simulation? analytical?)

main :: IO ()
main = do
  let bs = allBalancers 2 2 2
  print bs

type Balancer = M.HashMap ElementName Element

type ElementName = String

data Element
  = InputElement !Input
  | SplitterElement !Splitter
  deriving (Show)

fromInputElement :: Element -> Input
fromInputElement (InputElement input) = input
fromInputElement _ = error "Not an Input element!"

fromSplitterElement :: Element -> Splitter
fromSplitterElement (SplitterElement splitter) = splitter
fromSplitterElement _ = error "Not a Splitter element!"

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
    nextBalancers :: Balancer -> [Balancer]
    nextBalancers balancer | length (balancerSplitters balancer) == maxNumSplitters = []
    nextBalancers balancer =
      let outputs = balancerOutputs balancer
          splitters = balancerSplitters balancer
          possibleSplitterInputs =
            [ (out1, out2)
              | out1 <- Nothing : (Just <$> outputs),
                out2 <- filter ((/= out1) . Just) outputs
            ]
          possibleLoops =
            [ (out, fst splitterWithFreeInput)
              | out <- outputs,
                splitterWithFreeInput <- filter (doesSplitterHaveFreeInput . snd) splitters
            ]
       in map addSplitterWithInputs possibleSplitterInputs ++ map addLoop possibleLoops
      where
        addSplitterWithInputs :: (Maybe Output, Output) -> Balancer
        addSplitterWithInputs (maybeLeftInput, rightInput) =
          let maybeLeftInputElementName = outputName <$> maybeLeftInput
              rightInputElementName = outputName rightInput
              newSplitter = Splitter maybeLeftInputElementName (Just rightInputElementName) Nothing Nothing
              newSplitterName = fromMaybe "_" maybeLeftInputElementName ++ "+" ++ rightInputElementName
              balancer' = M.insert newSplitterName (SplitterElement newSplitter) balancer
              balancer'' = case maybeLeftInput of
                Nothing -> balancer'
                Just leftInput -> connectOutputToSplitter leftInput newSplitterName balancer'
              balancer''' =
                connectOutputToSplitter rightInput newSplitterName balancer''
           in balancer'''

        addLoop :: (Output, ElementName) -> Balancer
        addLoop (out, splitterName) =
          connectOutputToSplitter out splitterName balancer

        connectOutputToSplitter output splitterName balancer' =
          case output of
            Output inputName InputOutput ->
              let input = fromInputElement $ fromJust $ M.lookup inputName balancer'
                  newInput = input {inputNextElem = Just splitterName}
               in M.insert inputName (InputElement newInput) balancer'
            Output splitterName splitterOutputType ->
              let splitter = fromSplitterElement $ fromJust $ M.lookup splitterName balancer'
                  newSplitter = case splitterOutputType of
                    LeftSplitterOutput -> splitter {splitterLeftOutput = Just splitterName}
                    RightSplitterOutput -> splitter {splitterRightOutput = Just splitterName}
                    InputOutput -> error "impossible"
               in M.insert splitterName (SplitterElement splitter) balancer'

    isBalancerTooBig balancer = length (balancerSplitters balancer) > maxNumSplitters

    doesBalancerHaveFinalShape balancer =
      length (balancerOutputs balancer) == numOutputs
        && length (balancerInputs balancer) == numInputs

balancerInputs :: Balancer -> [(ElementName, Input)]
balancerInputs balancer =
  M.toList $
    M.mapMaybe
      toInputElem
      balancer
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
  deriving (Eq, Show)

outputName :: Output -> ElementName
outputName (Output name _) = name

data OutputType
  = LeftSplitterOutput
  | RightSplitterOutput
  | InputOutput
  deriving (Eq, Show)

splitterOutputs :: (ElementName, Splitter) -> [Output]
splitterOutputs (name, splitter) = case splitter of
  (Splitter _ _ Nothing Nothing) -> [Output name LeftSplitterOutput, Output name RightSplitterOutput]
  (Splitter _ _ (Just _) Nothing) -> [Output name RightSplitterOutput]
  (Splitter _ _ Nothing (Just _)) -> [Output name LeftSplitterOutput]
  (Splitter _ _ (Just _) (Just _)) -> []

doesSplitterHaveFreeInput :: Splitter -> Bool
doesSplitterHaveFreeInput splitter = case splitter of
  (Splitter Nothing _ _ _) -> True
  (Splitter _ Nothing _ _) -> True
  otherwise -> False

inputOutputs :: (ElementName, Input) -> [Output]
inputOutputs (name, input) = case input of
  Input Nothing -> [Output name InputOutput]
  Input (Just _) -> []

balancerOutputs :: Balancer -> [Output]
balancerOutputs balancer =
  let inputs = balancerInputs balancer
      splitters = balancerSplitters balancer
   in concatMap inputOutputs inputs ++ concatMap splitterOutputs splitters
