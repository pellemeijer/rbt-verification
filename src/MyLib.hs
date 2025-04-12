{-# OPTIONS_GHC -fplugin=LiquidHaskell #-}

module MyLib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
