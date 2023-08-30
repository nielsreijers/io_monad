{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE InstanceSigs #-}

module MyIO (
    MyIO,
    myPrintChar,
    myReadChar,
    myRunProgramme
) where

import Control.Applicative () -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

-- The MyIO monad is in essence nothing more than a 'programme' which maps input to
-- a tuple containing a value, the remaining input, and optionally some output.
--
--                     input  -> (value, remaining_input, output)
newtype MyIO a = MyIO ([Char] -> (a,     [Char],          [Char]))
-- By composing MyIO monadic functions, we end up with a programme that,
-- given some input, produces a MyIO () value.
-- During its evaluation, it may consume some of its input, and produce some output.

instance Functor MyIO where
    -- To lift a function that transforms a into b, we need to produce a new function
    -- that transforms MyIO a (a programme producing an a) into MyIO b (a programme producing a b).
    fmap :: (a -> b) -> MyIO a -> MyIO b
    -- The result has to be a new function [Char] -> (a, [Char], [Char])
    -- This function, given some input
    --  1) first runs the original programme on the input,
    --     which may consume some input an produce some output,
    --  2) then applies f to the value,
    --  3) returns (the value of type b, remaining input, and output produced by the input programme)
    fmap f (MyIO programme) = MyIO (\input ->
        let (x, remaining_input, output) = programme input in
            (f x, remaining_input, output))

instance Applicative MyIO where
  pure = return
  (<*>) = ap

instance Monad MyIO where
    -- Return simply produces a programme that produces the value
    -- and doesn't touch its input, or produce any output.
    return x = MyIO (\input -> (x, input, []))

    -- Bind takes as input a programme producing a value of type a,
    --                     and a function mapping a value of type a to a programme of type b.
    (>>=) :: MyIO a -> (a -> MyIO b) -> MyIO b
    -- We have two programmes: the original input MyIO a,
    --                         and the programme produced by the function acting on the value of type a.
    -- We need to produce a new programme that combines both.
    -- Given some input, this new programme, given some input:
    MyIO programme_x >>= f = MyIO (\input ->
        --  1) Run the first MyIO (programme_x) on the input,
        --     this MUST produce a value of type b, and MAY consume some input or produce some output
        let (x, remaining_input_x, output_x) = programme_x input
        --  2) Next, run the function on the value of type a.
        --     This does NOT yet produce a value of b, but a new programme_y of type b.
            MyIO programme_y = f x
        --  3) Run this new programme_y on the REMAINING input, left over after running programme_x,
        --     this produces a value of type b,
        --     and again may consume some more input and produce some output.
            (y, remaining_input_y, output_y) = programme_y remaining_input_x
        --  4) The result of these two programmes is then a tuple containing
        --       - the value of type b,
        --       - the input remaining after running both programmes,
        --       - and the concatenation of the output produced by both programmes.
        in (y, remaining_input_y, output_x ++ output_y))

-- The monadic primitives:
-- This simple MyIO monad only reads and writes characters,
-- but it should be clear that any, more complex, IO could be expressed using a stream of characters.

-- Printing a character doesn't touch the input, and simply produces an output containing just that character.
myPrintChar :: Char -> MyIO ()
myPrintChar c = MyIO (\input -> ((), input, [c]))

-- Reading a character produces a MyIO Char programme, which
-- consumes the first character in the input, and produces this value along with empty output.
myReadChar :: MyIO Char
myReadChar = MyIO (\case
    [] -> error "empty input"
    (x : xs) -> (x, xs, []))

-- Running a programme means applying a MyIO () monad to some input string.
-- Here, we simply evaluate the function,
-- and print the resulting output, along with the remaining input just for clarification.
myRunProgramme :: MyIO () -> [Char] -> IO ()
myRunProgramme (MyIO programme) input =
    let (_, remaining_input, output) = programme input
    in do putStrLn ""
          putStrLn "Programme output"
          putStrLn output
          putStrLn "Remaining input"
          putStrLn remaining_input
