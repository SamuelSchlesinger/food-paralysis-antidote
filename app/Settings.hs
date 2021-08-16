{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Settings where

import Data.String (IsString)

mySettings :: Settings
mySettings = Settings
  { proteins = ["steak", "pork", "chicken", "tofu"]
  , carbs = ["rice", "rice noodles", "homemade noodles", "quinoa", "cous cous"]
  , veggies = ["broccoli", "brussel sprouts", "asparagus", "onions and mushrooms"]
  , recipes = ["ravioli and peas", "stir fry"]
  }

newtype Protein = Protein { unProtein :: String }
  deriving newtype (Read, Show, IsString)

newtype Veggie = Veggie { unVeggie :: String }
  deriving newtype (Read, Show, IsString)

newtype Carb = Carb { unCarb :: String }
  deriving newtype (Read, Show, IsString)
 
newtype DinnerIdea = DinnerIdea { unDinnerIdea :: String }
  deriving newtype (Read, Show, IsString)

data Settings = Settings
  { recipes :: [DinnerIdea]
  , carbs :: [Carb]
  , proteins :: [Protein]
  , veggies :: [Veggie]
  }

class SettingsLists t where
  listing :: Settings -> [t]

instance SettingsLists Carb where
  listing = carbs

instance SettingsLists Protein where
  listing = proteins

instance SettingsLists Veggie where
  listing = veggies

instance SettingsLists DinnerIdea where
  listing context = mappend (recipes context) $ carbProteinVeggie <$> listing context <*> listing context <*> listing context
    
withSettings :: (Settings -> a) -> a
withSettings f = f mySettings

carbProteinVeggie :: Carb -> Protein -> Veggie -> DinnerIdea
carbProteinVeggie (Carb c) (Protein p) (Veggie v) = DinnerIdea $ "{ carb: " <> c <> ", protein: " <> p <> ", veggie: " <> v <> "}" 
