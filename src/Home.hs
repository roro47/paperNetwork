{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Home where

import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)

import Foundation
import Yesod.Core
import Yesod.Form
--import Yesod.Form.Jquery
--import Yesod.Form.Types
--import Yesod.Form.Functions

data Search = Search
  { searchInput :: Text } deriving Show


searchAForm :: AForm Handler Search
searchAForm = Search
       <$> areq textField "Input" Nothing


searchForm :: Html -> MForm Handler (FormResult Search , Widget)
searchForm = renderTable searchAForm

getHomeR :: Handler Html
getHomeR = do
      ((result, widget), enctype) <- runFormGet searchForm
      defaultLayout
         [whamlet|
              <p>Result #{show result}
              <form enctype=#{enctype}>
                ^{widget}
         |]
