{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Reference where

import Foundation
import Yesod.Core

getReferenceR :: Integer -> Handler Value
getReferenceR x = return $ object ["paperId" .= x]
