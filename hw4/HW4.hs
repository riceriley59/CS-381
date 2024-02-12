-- Homework 4
-- Name:  Riley Rice
-- Date: 2-12-2024
-- CS 381 - Programming Language Funadmentals

-- Declare Homework4 module and import Maybe class
module Homework4 where
import Data.Maybe

-- Define program to be list of commands
type Prog   = [Cmd]

-- Define different commands for stack program
data Cmd    = LDI Int | LDB Bool | LEQ | ADD | MULT | DUP | IFELSE Prog Prog 
  deriving Show

-- Sample programs
p1 = [LDI 5, ADD, MULT]
p2 = [LEQ, IFELSE [LDI 7, ADD] [MULT]]
p3 = [LDB False, DUP]
--	 

-- Define the two values that can be used in a command 
-- In this instance it's a integer or Boolean
data Val = I Int | B Bool
  deriving Show

-- Define a stack as a list of values
type Stack = [Val]

-- Sample stacks
s1 = [I 1, I 2, I 3, I 5]
s2 = [B False, I 5, I 7]
s3 = [I 10, I 4, B True]
--

-- Define the result of a program as either a Stack or an Error
data Result = S Stack | Error
  deriving Show

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
semCmd DUP (v:s) = Just (v:v:s) 
semCmd DUP _ = Nothing

-- Catch any undefined commands or errors
semCmd _ _ = Nothing

-- sem applies all the commands in the program to the stack
sem :: Prog -> Stack -> Maybe Stack
sem [] [] = Nothing    -- empty stack and no program
sem []  s = Just s     -- no program
sem (c:cs) s = case semCmd c s of
                Just s' -> sem cs s'
                Nothing -> Nothing   -- stop execution on Nothing		

-- to run call sem and replace nothing with Error and
-- remove the Just from the stack to return S stack
run :: Prog -> Stack -> Result
run p s = if isNothing((sem p s))then Error
          else S (fromJust(sem p s)) 
