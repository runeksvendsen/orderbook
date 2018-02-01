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

srcFetch
   :: forall dataType.
      HTTP.Manager
   -> DataSrc dataType
   -> IO (Either SC.ServantError dataType)
srcFetch man ds = SC.runClientM clientM env
   where env = SC.ClientEnv man (dsUrl ds)
         clientM = dsClientM ds

fetch :: forall dataType.
         DataSource dataType
      => HTTP.Manager
      -> IO (Either SC.ServantError dataType)
fetch man = srcFetch man (dataSrc :: DataSrc dataType)


