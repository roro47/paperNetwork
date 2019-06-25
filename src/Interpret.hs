{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Interpret (reqInterpretCall) where


import GHC.Generics (Generic)
import Control.Lens ((&), (^.), (^?), (.~), (^..), preview)
import qualified Control.Exception as E
import Network.Wreq
import Data.Map (Map, (!))
import Data.Text (Text)
import Data.Aeson (FromJSON, Value)
import Data.Aeson.Lens (_String, _Array, _Value, _Object, key, values)

import Api

apiKey = "6a5f13ed66854b848d9ffb7817feb6da"
-- Interpret Stuff

data RuleBody = RuleBody {
  name :: Text,
  output :: Map Text Text
  } deriving (Show, Generic)

instance FromJSON RuleBody

data InterpretBody =
 InterpretBody {
  logprob :: Double,
  parse :: Text,
  rules :: [RuleBody]
  } deriving (Show, Generic)

instance FromJSON InterpretBody

data GetInterpretBody = GetInterpretBody {
  query :: Text,
  interpretations :: [InterpretBody]
  } deriving (Show, Generic)

instance FromJSON GetInterpretBody


-- default header
interpretUrl = "https://api.labs.cognitive.microsoft.com/academic/v1.0/interpret?"


-- use default value for other parameters
data Interpret = Interpret {
  queryInput :: String
 }

instance Show Interpret where
  show (Interpret query) = interpretUrl ++ "query=" ++ query

interpret :: String -> Interpret
interpret query = Interpret query

reqInterpretCall :: String -> IO ()
reqInterpretCall input = do
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

