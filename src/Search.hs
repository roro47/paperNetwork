{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Search where

import Foundation
import Api
import Yesod.Core

getSearchR :: String -> Handler Value
getSearchR input = return $ object ["result" .= input]
