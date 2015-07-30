module Main (main) where

import Control.FRPNow.Gloss

import Graphics.Gloss

import Game

displayMode :: Display
displayMode = InWindow "Dumb Test" (600,600) (0,0)


main :: IO ()
main = runNowGlossPure displayMode blue 60 ((fmap.fmap.fmap.fmap) drawGame game)
