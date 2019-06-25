{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Evaluate where

import GHC.Generics (Generic)
import Control.Lens ((&), (^.), (^?), (.~), (^..), (^?!), preview)
import qualified Control.Exception as E
import Network.Wreq
import Data.List (intercalate)
import Data.Map (Map, (!))
import Data.Text (Text, unpack)
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Lens (_String, _Array, _Value, _Object, key, values, nth, _Integer)
import Data.Maybe

--import Api

-- default header

apiKey = "6a5f13ed66854b848d9ffb7817feb6da"
evaluateUrl = "https://api.labs.cognitive.microsoft.com/academic/v1.0/evaluate?"

-- evaluate stuff

defaultAttributes = "Id,RId,Ti,E"

{- For documentation of api

data InvertedAbstractBody = InvertedAbstractBody {
  indexLength :: Integer,
  inveretedIndex :: Map String [Integer]
  } deriving (Show, Generic)

instance FromJSON InvertedAbstractBody

data ExtendedBody = ExtendedBody {
  dn :: Text, -- DN
  ia :: InvertedAbstractBody -- IA
  } deriving (Show, Generic)

instance FromJSON ExtendedBody

data EntityBody = EntityBody {
  logprob :: Double,
  prob :: Double,
  id :: Integer,
  rId :: [Integer],
  e :: ExtendedBody
  } deriving (Show, Generic)

instance FromJSON EntityBody

data GetEvaluateBody = GetEvaluateBody {
  expr :: Text,
  entities :: [EntityBody]
  } deriving (Show, Generic)

instance FromJSON GetEvaluateBody
-}

data Evaluate = Evaluate {
  exprStr :: String,
  count :: Integer,
  attributes :: String
}

data Paper = Paper {
  id :: Integer,
  normalTitle :: String,
  title :: String,
  abstract :: String
  } deriving (Show)

instance Show Evaluate where
  show (Evaluate expr count attributes)
    = intercalate "&" ["expr=" ++ expr,
                       "count=" ++ show count,
                       "attributes=" ++ attributes]

{-
reqInterpretInput :: String -> IO ()
reqInterpretInput input = do
    let opts = defaults & header "Ocp-Apim-Subscription-Key" .~ [apiKey]
    r <- asValue =<< getWith opts (show (interpret input))
    putStrLn $ show $ r ^. responseBody . key "interpretations"
                                        . _Array
                                        . traverse
                                        . key "rules"
                                        ._Array
                                        ^.. traverse
                                        . key "output"
                                        . key "value"
                                        . _String
-}


reqGetPaperById :: Integer -> IO ()
reqGetPaperById id = do
  let opts = defaults & header "Ocp-Apim-Subscription-Key" .~ [apiKey]
  r <- asValue =<< getWith opts evaluate
  let stuff =  fromJust (r ^. responseBody ^? key "entities" . nth 0)
  let paper = getPaperFromExtended stuff
  putStrLn (title paper)
  where evaluate = evaluateUrl ++ show ( Evaluate ("Id=" ++ show id) 1 defaultAttributes)


getPaperFromExtended :: Value -> Paper
getPaperFromExtended value = let id = fromJust $ value ^? key "Id" . _Integer
                                 normalTitle = unpack $ fromJust $ value ^? key "Ti" . _String
                                 extended = fromJust $ value ^? key "E" . _Value
                                 (title, abstract) = parseExtended extended
                          in Paper id normalTitle title abstract
   where parseExtended :: Value -> (String, String) -- display name
         parseExtended value = let title = fromJust (value ^? key "DN" . _String)
                                   ia = value ^? key "IA" . _Object
                               in (unpack title, show ia)
