{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}

module Main where

import Data.String (IsString)
import Data.Coerce (coerce, Coercible)
import System.Random (randomRIO)
import Options.Applicative
import Settings

data Command =
    ProteinVeggieCarb (Maybe Protein) (Maybe Veggie) (Maybe Carb)
  | Recipe
  | Anything

main :: IO ()
main = withSettings \settings -> customExecParser ps parser >>= program settings where
  program :: Settings -> Command -> IO ()
  program settings c = do
    let is = dinnerIdeas settings c 
    i <- randomRIO (0, length is)
    putStrLn . unDinnerIdea $ is !! i
  ps :: ParserPrefs
  ps = prefs . mconcat $
    [ disambiguate
    , showHelpOnError
    ]
    
dinnerIdeas :: Settings -> Command -> [DinnerIdea]
dinnerIdeas settings = \case
  ProteinVeggieCarb mp mv mc -> carbProteinVeggie <$> orListing mc <*> orListing mp <*> orListing mv
  Recipe -> recipes settings
  Anything -> listing settings
  where
    orListing = maybe (listing settings) pure
    orListing :: SettingsLists c => Maybe c -> [c]

parser :: ParserInfo Command
parser = flip info mods . hsubparser . mconcat $
  [ command "pvc" (info pvc (progDesc "Outputs a dinner idea with a random protein, veggie, and carb"))
  , command "recipe" (info recipe (progDesc "Outputs a more specific recipe idea"))
  , command "any" (info any (progDesc "Outputs a completely random dinner idea"))
  ]
  where
    o :: forall x. (SettingsLists x, Coercible String x, Coercible x String) => String -> Char -> Parser (Maybe x)
    o p p' = option ((Just @x . coerce @String @x) <$> str) (long p <> short p' <> value (Nothing @x) <> completeWith (map coerce $ listing @x (withSettings id)))
    pvc = ProteinVeggieCarb <$> o @Protein "protein" 'p' <*> o @Veggie "veggie" 'v' <*> o @Carb "carb" 'c'
    recipe = pure Recipe
    any = pure Anything
    mods = header "Generate Dinner Ideas" <> footer "Copyright 2021 (c) Samuel Schlesinger" <> progDesc "Outputs dinner ideas from a list of recipes as well as lists of proteins, veggies, and carbs"
