module Main where

import qualified Data.HashMap.Strict as M
import Data.List (nub, replicate)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

-- TODO:
--   - Make basic code for generating balancers. [DONE]
--   - Improve all code in this file to be nicer.
--   - Since I introduced loops, I am getting a lot more potential balancers. A bit too much it feels.
--     Can I somehow reduce that? Somehow skip the ones that are equivalent?
--   - Make logic for testing if balancer is balanced (simulation? analytical?)
--     I think we want to do analytical approach.
--     What we need to do is define a set of linear equations that we can then
--     solve to figure out how the balancer is working and if its outputs
--     are balanced.
--     I found this library, it is probably a good fit: https://hackage.haskell.org/package/mfsolve .
--     The basic idea:
--     - define each input as a constant: A, B, C, ... .
--     - for each splitter, assign a new variable to its output (both of them), and define question where we express
--       that variable via its inputs. So if inputs are A and B, and we just named splitter's outputs x,
--       we would have following equation: 2*x = A+B .
--     - Once we have all these equations, we want to calculate the value of outputs of the balancer.
--       So we should solve those! For example if outputs of balancer are really outputs of splitter
--       whose outputs we named with y, then we want to solve y (and for other outputs similarly).
--     - By solving the outputs, we will get them expressed via inputs, via A, B, C, ... .
--       Now, how can we figure out if they are balanced? Well, for each output, we can check if it
--       equals 1/num_outputs * (A + B + ...). If it does, that output is balanced! And if they are
--       all balanced, then our balancer is working!
--     - Fun: We could also try and look for balancers that are not perfectly balanced, but are balanced
--       pretty well. Maybe there are such balancers, that are significanly simpler then the perfect one,
--       which would make them interesting in practice. I am not sure how to define that yet, I guess
--       we could check that it is close to 1/num_outputs * (A + B + ...), and not exactly the same.
--   - Interesting question is how we can reduce search space. Maybe we can do genetic algorithm?
--     And we use our equation solving to say how close is the balancer to being balanced?

main :: IO ()
main = do
  let bs = allBalancers 2 2 2
  mapM_ (putStrLn . prettyShowBalancer) bs

type Balancer = M.HashMap ElementId Element

type ElementId = Int

data Element
  = InputElement !Input
  | SplitterElement !Splitter
  deriving (Show, Eq)

fromInputElement :: Element -> Input
fromInputElement (InputElement input) = input
fromInputElement _ = error "Not an Input element!"

fromSplitterElement :: Element -> Splitter
fromSplitterElement (SplitterElement splitter) = splitter
fromSplitterElement _ = error "Not a Splitter element!"

data Input = Input
  { inputNextElem :: Maybe ElementId
  }
  deriving (Show, Eq)

data Splitter = Splitter
  { splitterLeftInput :: Maybe ElementId,
    splitterRightInput :: Maybe ElementId,
    splitterLeftOutput :: Maybe ElementId,
    splitterRightOutput :: Maybe ElementId
  }
  deriving (Show, Eq)

allBalancers :: Int -> Int -> Int -> [Balancer]
allBalancers numInputs numOutputs maxNumSplitters =
  filter doesBalancerHaveFinalShape $ allBalancersStartingFrom [initBalancer]
  where
    initBalancer = M.fromList $ zip [0 ..] (replicate numInputs $ InputElement $ Input Nothing)

    -- Returns all balancers that can be created by upgrading the given initial set of balancers.
    -- It also includes initial set of balancers in the returned balancers.
    -- It doesn't check that they work correctly, or that they have correct final shape
    -- regarding number of outputs.
    allBalancersStartingFrom [] = []
    allBalancersStartingFrom balancers =
      -- TODO: I try to reduce repetition with `nub` here but it is probably not a good way to do it,
      --   I need smarter way of figuring out if two balancers are equivalent then just checking for equality.
      let nextBalancers' = nub $ filter (not . isBalancerTooBig) $ concat $ nextBalancers <$> balancers
          result = balancers ++ allBalancersStartingFrom nextBalancers'
       in -- in trace ("\nNum balancers: " ++ (show $ length balancers) ++ "\nNum nextBalancers: " ++ (show $ length nextBalancers') ++ "\n") result
          -- in trace ("\nBalancers:\n" ++ (unlines $ prettyShowBalancer <$> balancers) ++ "\n\nNextBalancers:\n" ++ (unlines $ prettyShowBalancer <$> nextBalancers')) result
          result

    -- Returns all balancers that can be created by upgrading the given balancer via exactly
    -- one upgrade, where upgrade is adding new splitter or adding a loop.
    nextBalancers :: Balancer -> [Balancer]
    nextBalancers balancer =
      let outputs = balancerOutputs balancer
          splitters = balancerSplitters balancer
          -- TODO: Some combinations don't make sense, for example connecting splitter to another splitter fully
          --   (both outputs to both inputs) so we could forbid those!
          possibleSplitterInputs =
            [ (out1, out2)
              | (i, out1) <- zip [0 ..] $ Nothing : (Just <$> outputs),
                out2 <- drop i outputs
            ]
          possibleLoops =
            [ (output, splitterId)
              | (splitterId, _) <- filter (doesSplitterHaveFreeInput . snd) splitters,
                output <- filter ((>= splitterId) . outputElementId) outputs
            ]
       in map addSplitterWithInputs possibleSplitterInputs ++ map addLoop possibleLoops
      where
        addSplitterWithInputs :: (Maybe Output, Output) -> Balancer
        addSplitterWithInputs (maybeLeftInput, rightInput) =
          let newSplitter = Splitter Nothing Nothing Nothing Nothing
              newSplitterId = M.size balancer
              balancer' = M.insert newSplitterId (SplitterElement newSplitter) balancer
              balancer'' = case maybeLeftInput of
                Nothing -> balancer'
                Just leftInput -> connectOutputToSplitter leftInput newSplitterId balancer'
              balancer''' =
                connectOutputToSplitter rightInput newSplitterId balancer''
           in balancer'''

        addLoop :: (Output, ElementId) -> Balancer
        addLoop (output, splitterId) = connectOutputToSplitter output splitterId balancer

        connectOutputToSplitter output splitterId balancer' =
          let balancer'' = case output of
                Output inputId InputOutput ->
                  let input = fromInputElement $ fromJust $ M.lookup inputId balancer'
                      newInput = input {inputNextElem = Just splitterId}
                   in M.insert inputId (InputElement newInput) balancer'
                Output outSplitterId outSplitterOutputType ->
                  let outSplitter = fromSplitterElement $ fromJust $ M.lookup outSplitterId balancer'
                      outSplitter' = case outSplitterOutputType of
                        LeftSplitterOutput -> outSplitter {splitterLeftOutput = Just splitterId}
                        RightSplitterOutput -> outSplitter {splitterRightOutput = Just splitterId}
                        InputOutput -> error "impossible"
                   in M.insert outSplitterId (SplitterElement outSplitter') balancer'
              balancer''' =
                let splitter = fromSplitterElement $ fromJust $ M.lookup splitterId balancer''
                    splitter' = case splitter of
                      Splitter Nothing _ _ _ -> splitter {splitterLeftInput = Just $ outputElementId output}
                      Splitter _ Nothing _ _ -> splitter {splitterRightInput = Just $ outputElementId output}
                      _ -> error "Tried to connect output to splitter that has no free inputs."
                 in M.insert splitterId (SplitterElement splitter') balancer''
           in balancer'''

    isBalancerTooBig balancer = length (balancerSplitters balancer) > maxNumSplitters

    doesBalancerHaveFinalShape balancer =
      length (balancerOutputs balancer) == numOutputs
        && length (balancerInputs balancer) == numInputs

balancerInputs :: Balancer -> [(ElementId, Input)]
balancerInputs balancer =
  M.toList $
    M.mapMaybe
      toInputElem
      balancer
  where
    toInputElem e = case e of
      InputElement input -> Just input
      _ -> Nothing

balancerSplitters :: Balancer -> [(ElementId, Splitter)]
balancerSplitters balancer = M.toList $ M.mapMaybe toSplitterElem balancer
  where
    toSplitterElem e = case e of
      SplitterElement splitter -> Just splitter
      _ -> Nothing

data Output = Output ElementId OutputType
  deriving (Eq, Show)

outputElementId :: Output -> ElementId
outputElementId (Output elementId _) = elementId

data OutputType
  = LeftSplitterOutput
  | RightSplitterOutput
  | InputOutput
  deriving (Eq, Show)

splitterOutputs :: (ElementId, Splitter) -> [Output]
splitterOutputs (splitterId, splitter) = case splitter of
  (Splitter _ _ Nothing Nothing) -> [Output splitterId LeftSplitterOutput, Output splitterId RightSplitterOutput]
  (Splitter _ _ (Just _) Nothing) -> [Output splitterId RightSplitterOutput]
  (Splitter _ _ Nothing (Just _)) -> [Output splitterId LeftSplitterOutput]
  (Splitter _ _ (Just _) (Just _)) -> []

doesSplitterHaveFreeInput :: Splitter -> Bool
doesSplitterHaveFreeInput splitter = case splitter of
  (Splitter Nothing _ _ _) -> True
  (Splitter _ Nothing _ _) -> True
  otherwise -> False

inputOutputs :: (ElementId, Input) -> [Output]
inputOutputs (inputId, input) = case input of
  Input Nothing -> [Output inputId InputOutput]
  Input (Just _) -> []

balancerOutputs :: Balancer -> [Output]
balancerOutputs balancer =
  let inputs = balancerInputs balancer
      splitters = balancerSplitters balancer
   in concatMap inputOutputs inputs ++ concatMap splitterOutputs splitters

prettyShowBalancer :: Balancer -> String
prettyShowBalancer balancer =
  unlines $ (\(eid, e) -> " - " ++ show eid ++ ": " ++ prettyShowElement e) <$> M.toList balancer

prettyShowElement :: Element -> String
prettyShowElement e = show e
