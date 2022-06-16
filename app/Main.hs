{-# LANGUAGE UnicodeSyntax #-}

module Main where

import Control.Monad.IO.Class

import MatrixBot.App (runApp)

main ∷ IO ()
main = liftIO runApp
