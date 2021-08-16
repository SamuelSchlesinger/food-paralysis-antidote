{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Settings where

import Data.String (IsString)

mySettings :: Settings
mySettings = Settings
  { proteins = ["steak", "pork", "chicken", "tofu", "scallops", "salmon", "tuna", "ground beef", "sausage", "egg"]
  , carbs = ["rice", "rice noodles", "homemade noodles", "quinoa", "cous cous", "roasted potatoes", "tater tots", "mashed potatoes", "baked potatoes"]
  , veggies = ["broccoli", "brussel sprouts", "asparagus", "onions and mushrooms", "side salad", "zucchini", "squash"]
  , recipes =
    [ "ravioli and peas", "stir fry", "pizza", "trader joe's fried rice", "trader joe's orange chicken", "salad", "cream of mushroom soup", "quesadillas", "black bean burger", "veggie burger", "beef burger", "portabella burger"
    , "fajitas", "sushi", "tortilla soup", "butternut squash soup" ]
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
carbProteinVeggie (Carb c) (Protein p) (Veggie v) = DinnerIdea $ p <> " and " <> v <> " over " <> c
