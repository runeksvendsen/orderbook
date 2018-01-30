{-# LANGUAGE TypeOperators #-}
module Lib.Fetch where

import MyPrelude
import qualified Servant.Common.BaseUrl as S
import qualified Servant.Client        as SC
import           Servant.API
import qualified Data.Aeson            as Json
import qualified Network.HTTP.Client   as HTTP


class Json.FromJSON dataType => DataSource dataType where
   dataSrc :: DataSrc dataType

data DataSrc dataType = DataSrc
   { dsUrl     :: S.BaseUrl
   , dsClientM :: SC.ClientM dataType
   }

fetch :: forall dataType.
         DataSource dataType
      => HTTP.Manager
      -> IO (Either SC.ServantError dataType)
fetch man = SC.runClientM clientM env
   where env = SC.ClientEnv man (dsUrl (dataSrc :: DataSrc dataType))
         clientM = dsClientM (dataSrc :: DataSrc dataType)


