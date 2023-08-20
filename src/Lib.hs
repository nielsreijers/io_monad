module Lib
    ( someFunc
    ) where

import MyIO
import Data.Char (toUpper)

makeSillyProgramme :: MyIO()
makeSillyProgramme = do
    c1 <- myReadChar
    c2 <- myReadChar
    myPrintChar c1
    myPrintChar (toUpper c2)

someFunc :: IO ()
someFunc = myRunProgramme (makeSillyProgramme) ['a', 'b', 'c', 'd']

