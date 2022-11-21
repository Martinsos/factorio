module Main where

import qualified Data.HashMap.Strict as M
import Data.List (nub, replicate)
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)

-- TODO:
--   - Make basic code for generating balancers. [DONE]
--   - Add possibility to have loops in balancers. Check comment in the code.
--   - Improve all code in this file to be nicer.
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

main :: IO ()
main = do
  let bs = allBalancers 2 2 2
  mapM_ (putStrLn . prettyShowBalancer) bs

type Balancer = M.HashMap ElementName Element

type ElementName = String

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
  { inputNextElem :: Maybe ElementName
  }
  deriving (Show, Eq)

data Splitter = Splitter
  { splitterLeftInput :: Maybe ElementName,
    splitterRightInput :: Maybe ElementName,
    splitterLeftOutput :: Maybe ElementName,
    splitterRightOutput :: Maybe ElementName
  }
  deriving (Show, Eq)

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
          possibleSplitterInputs =
            [ (out1, out2)
              | (i, out1) <- zip [0 ..] $ Nothing : (Just <$> outputs),
                out2 <- drop i outputs
            ]
          -- TODO: This is not the best right now, as it also can add just normal, non loop connections,
          --   by using output from older splitter for input for new splitter.
          --   So what does loop mean? Loops means connecting an existing output to an
          --   input of existing splitter, where that output didn't exist at the moment of creating
          --   that splitter (so it is either output of that same splitter, or of some other splitter
          --   created after it).
          --   It would be best if we had numbers attached to each element, and they grow in order,
          --   then we can easily tell who is before whom, we just check their number/id.
          --   So maybe instead of names in the hash map, we use numbers, as ids?
          --   When adding a new element to hash map, we can check its size and just give it an id of total size + 1.
          possibleLoops = []
       in -- [ (out, fst splitterWithFreeInput)
          --   | out <- outputs,
          --     splitterWithFreeInput <- filter (doesSplitterHaveFreeInput . snd) splitters
          -- ]
          map addSplitterWithInputs possibleSplitterInputs ++ map addLoop possibleLoops
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
        addLoop (output@(Output outputName _), splitterName) =
          let balancer' = connectOutputToSplitter output splitterName balancer
              splitter = fromSplitterElement $ fromJust $ M.lookup splitterName balancer
              splitter' = case splitter of
                Splitter Nothing _ _ _ -> splitter {splitterLeftInput = Just splitterName}
                Splitter _ Nothing _ _ -> splitter {splitterRightInput = Just splitterName}
                _ -> error "Tried to construct loop with splitter that has no free inputs."
              balancer'' = M.insert splitterName (SplitterElement splitter) balancer'
           in balancer''

        connectOutputToSplitter output splitterName balancer' =
          case output of
            Output inputName InputOutput ->
              let input = fromInputElement $ fromJust $ M.lookup inputName balancer'
                  newInput = input {inputNextElem = Just splitterName}
               in M.insert inputName (InputElement newInput) balancer'
            Output outSplitterName outSplitterOutputType ->
              let outSplitter = fromSplitterElement $ fromJust $ M.lookup outSplitterName balancer'
                  outSplitter' = case outSplitterOutputType of
                    LeftSplitterOutput -> outSplitter {splitterLeftOutput = Just splitterName}
                    RightSplitterOutput -> outSplitter {splitterRightOutput = Just splitterName}
                    InputOutput -> error "impossible"
               in M.insert outSplitterName (SplitterElement outSplitter') balancer'

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

prettyShowBalancer :: Balancer -> String
prettyShowBalancer balancer =
  unlines $ (\(name, e) -> " - " ++ name ++ ": " ++ prettyShowElement e) <$> M.toList balancer

prettyShowElement :: Element -> String
prettyShowElement e = show e
