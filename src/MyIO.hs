{-# LANGUAGE LambdaCase #-}
module MyIO (
    MyIO,
    myPrintChar,
    myReadChar,
    myRunProgramme
) where

import Control.Applicative () -- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

newtype MyIO a = MyIO ([Char] -> (a, [Char], [Char]))

instance Functor MyIO where
    fmap f (MyIO programme) = MyIO (\input ->
        let (x, remaining_input, output) = programme input in
            (f x, remaining_input, output))

instance Applicative MyIO where
  pure = return
  (<*>) = ap

instance Monad MyIO where
    return x = MyIO (\input -> (x, input, []))

    MyIO programme_x >>= f = MyIO (\input ->
        let (x, remaining_input_x, output_x) = programme_x input
            MyIO programme_y = f x
            (y, remaining_input_y, output_y) = programme_y remaining_input_x
        in (y, remaining_input_y, output_x ++ output_y))

myPrintChar :: Char -> MyIO ()
myPrintChar c = MyIO (\input -> ((), input, [c]))

myReadChar :: MyIO Char
myReadChar = MyIO (\case
    [] -> error "empty input"
    (x : xs) -> (x, xs, []))

myRunProgramme :: MyIO () -> [Char] -> IO ()
myRunProgramme (MyIO programme) input =
    let (_, remaining_input, output) = programme input
    in do putStrLn ""
          putStrLn "Programme output"
          putStrLn output
          putStrLn "Remaining input"
          putStrLn remaining_input
