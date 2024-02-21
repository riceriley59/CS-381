module HW5sol where 

import HW5types
import Data.Maybe

-- This function handles the correct semantics of this program and language 
-- making sure that the stack works as expected, and throwing a nothing in return 
-- in some cases, hence the Maybe Stack.
semCmd :: Cmd -> Stack -> Maybe Stack
semCmd (LDI i) s = Just((I i):s) -- load an integer onto the stack
semCmd (LDB b) s = Just((B b):s) -- load a boolean onto the stack

-- Add the top values on the stack and push result onto the stack
-- If there aren't two integers return Nothing
semCmd ADD ((I i):(I i'):s) = Just (I(i + i'):s) 
semCmd ADD _ = Nothing 

-- Similiar to the ADD command we get the two top values of the stack 
-- multiply them and push result onto stack. If there aren't two integers 
-- return Nothing
semCmd MULT ((I i):(I i'):s) = Just (I(i * i'):s)
semCmd MULT _ = Nothing

-- If top value is less than next push True onto stack
-- otherwise push false
semCmd LEQ ((I i):(I i'):s) = Just ((B(i <= i')):s)
semCmd LEQ _ = Nothing

-- If top of stack is True execute first program
-- If False execute second program
semCmd (IFELSE [] _ ) ((B True):s) = Just s 
semCmd (IFELSE p1 _ ) ((B True):s) = sem p1 s
semCmd (IFELSE _ [] ) ((B False):s) = Just s 
semCmd (IFELSE _ p2 ) ((B False):s) = sem p2 s 

-- complete duplicate DUP to duplicate the top value on the
-- stack both integers and boolean values can be duplicated
-- if the stack is empty return Nothing
semCmd DUP (x:s) = Just (x:x:s) 
semCmd DUP _ = Nothing

-- This handles the DEC command which takes the top element
-- off of the stack and then decrements that value and pushes
-- it back onto the top of the stack
semCmd DEC ((I x):s) = Just (I (x - 1):s)
semCmd DEC _ = Nothing

-- This handles the SWAP command which takes the two top elements off of
-- the stack. It then puts the two values back into the stack in reverse order 
-- effectively swapping the elements.
semCmd SWAP (i:i':s) = Just (i':i:s)
semCmd SWAP _ = Nothing

-- This handles the POP command and removes k elements off of the stack
semCmd (POP k) s = if length s >= k then Just (drop k s) else Nothing   

-- Catch any undefined commands or errors
semCmd _ _ = Nothing

-- sem applies all the commands in the program to the stack
sem :: Prog -> Stack -> Maybe Stack
sem [] [] = Nothing    -- empty stack and no program
sem []  s = Just s     -- no program
sem (c:cs) s = case semCmd c s of
                Just s' -> sem cs s'
                Nothing -> Nothing   -- stop execution on Nothing		

rankC :: Cmd -> CmdRank
rankC ADD  = (2, 1)
rankC MULT = (2, 1)
rankC LEQ  = (2, 1)

rankC SWAP = (2, 2)
rankC DEC  = (1, 1)

rankC (LDI _) = (0, 1)
rankC (LDB _) = (0, 1)

rankC DUP   = (1, 2)
rankC (POP k) = (k, 0)

rankC (IFELSE _ _) = (0, 0)

rankP :: Prog -> Rank -> Maybe Rank
rankP [] r = Just r
rankP (IFELSE p1 p2 : xs) r = case (rankP p1 (r - 1), rankP p2 (r - 1)) of 
  (Just r1, Just r2) -> rankP xs (min r1 r2)
  _ -> Nothing
rankP (x:xs) r = case rankC x of 
  (n, m) -> if r >= n then rankP xs (r - n + m) 
            else Nothing


-- to run call sem and replace nothing with Error and
-- remove the Just from the stack to return S stack
run :: Prog -> Stack -> Result
run p s = case rankP p (length s) of 
  Nothing -> RankError
  Just r -> case sem p s of 
    Just s' -> A s'
    Nothing -> TypeError
