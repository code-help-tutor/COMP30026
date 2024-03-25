WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module RunDPDA
where
import Data.Char
import Data.Maybe (catMaybes)
import DPDA

----------------------------------------------------------------------------
--  A DPDA simulator written in Haskell
----------------------------------------------------------------------------

--  Usage: 'use p' where p is a DPDA. The function will ask for
--  input to p, then print Accept or Reject, as appropriate.
--  Or, to run p on a single input, use 'runDPDA p input'

data Decision
  = Accept | Reject
    deriving (Eq, Show)

--  There is an assumption that the DPDA is well-formed and deterministic.
--  Each transition step consumes an input symbol. Symbols are character,
--  but for technical reasons, the input alphabet is assumed to contain
--  only digits and lower case letters. We follow the convention of using
--  'eps' in stack operations to mean 'nothing'. That is, 'pop x' is
--  captured as 'replace x by eps', 'push x' as 'replace eps by x',
--  and 'leave the stack untouched' by 'replace eps by eps'.

--  A configuration is a triple (state, stack, remaining input).

type Configuration = (State, Stack, [Symbol])

--  The function `runDPDA' does the step-by-step simulation.

runDPDA :: DPDA -> Input -> Decision

runDPDA (_, _, _, delta, startState, acceptStates) input
  = if compliant input
    then
      if endState `elem` acceptStates then Accept else Reject
    else
      error "DPDA input should be in [a-z0-9]*: lower case letters/digits"
    where
      (endState, _, _)
        = until (\x -> x == step delta x) (step delta) (startState, [], input)

-- We introduce a special 'dead state'/'reject state', to be used
-- when the given transition function hasn't been completed.
-- Since a well-formed DPDA's states must be non-negative integers,
-- using -1 will do the trick.

deadState :: State
deadState = -1

step :: [Transn] -> Configuration -> Configuration
step delta initialConfig@(state, stack, input)
  = case target of
      [] -> if input == "" then initialConfig else (deadState, stack, input)
      [config] -> config
      configs -> error $ "PDA is not deterministic: target configurations: " ++ (show configs) 
  where
    target = catMaybes [c1, c2, c3, c4]
    c1 =
      case lookup (state, eps, eps) delta of
        Just (newState, newTop) -> Just (newState, push newTop stack, input)
        Nothing -> Nothing
    c2 = case stack of
      top:stackTail ->
        case lookup (state, eps, top) delta of
          Just (newState, newTop) -> Just (newState, push newTop stackTail, input)
          Nothing -> Nothing
      [] -> Nothing
    c3 = case input of
      next:inputTail ->
        case lookup (state, next, eps) delta of
          Just (newState, newTop) -> Just (newState, push newTop stack, inputTail)
          Nothing -> Nothing
      [] -> Nothing
    c4 = case (input, stack) of
      (next:inputTail, top:stackTail) ->
        case lookup (state, next, top) delta of
          Just (newState, newTop) -> Just (newState, push newTop stackTail, inputTail)
          Nothing -> Nothing
      (_, _) -> Nothing
    push sym stack' = if sym == eps then stack' else sym:stack'

--  The function `use' takes a PDA p and enters a dialogue with the
--  user, asking for input to p, and printing p's decision.

use :: DPDA -> IO ()

use p
  = do
      putStr "Input (Q to quit): "
      input <- getLine
      if compliant input
        then do {putStrLn (show (runDPDA p input)); use p}
        else if (head input) == 'Q'
          then return ()
          else do
            putStrLn "Input must be digits and/or lower case letters"
            use p

--  The function `compliant' checks that input consists entirely of digits
--  and lower case letters.

compliant :: Input -> Bool
compliant
  = all comply
    where
      comply c = isLower c || isDigit c