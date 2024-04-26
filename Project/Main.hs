module Main where

import UU.Parsing
import Scanner(scanner)
import Parser


main :: IO ()
main = do input <- readFile "Slide.p5"
          let token = scanner input
          putStrLn(show token)
          tree <- parseIO pSlides token
          putStrLn (show tree)
