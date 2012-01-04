{-# LANGUAGE DeriveDataTypeable, 
             TemplateHaskell, 
             TypeFamilies, 
             TypeSynonymInstances, 
             OverloadedStrings  #-}

module Database.HXournal.IDMap.Type where

import Control.Applicative 
import Control.Monad.Reader
import Control.Monad.State
import Data.Typeable
import Data.Data
import Data.SafeCopy
import qualified Data.Map as M

import Data.Acid 
import Data.UUID
import Data.Aeson
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B

data Hxournal-idmapInfo = Hxournal-idmapInfo { 
  hxournal-idmap_uuid :: UUID, 
  hxournal-idmap_name :: String
} deriving (Show,Typeable,Data)


instance FromJSON UUID where
  parseJSON x = do r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
                   case r of 
                     Nothing -> fail ("UUID parsing failed " ++ show x )
                     Just uuid -> return uuid 

instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON Hxournal-idmapInfo where
  parseJSON (Object v) = Hxournal-idmapInfo <$>  v .: "uuid" <*> v .: "name"

instance ToJSON Hxournal-idmapInfo where
  toJSON (Hxournal-idmapInfo uuid name) = object [ "uuid" .= uuid , "name" .= name ] 


instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

$(deriveSafeCopy 0 'base ''Hxournal-idmapInfo)

type Hxournal-idmapInfoRepository = M.Map UUID Hxournal-idmapInfo 

addHxournal-idmap :: Hxournal-idmapInfo -> Update Hxournal-idmapInfoRepository Hxournal-idmapInfo 
addHxournal-idmap minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (hxournal-idmap_uuid minfo) minfo m
  put m'
  return minfo
 
queryHxournal-idmap :: UUID -> Query Hxournal-idmapInfoRepository (Maybe Hxournal-idmapInfo) 
queryHxournal-idmap uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query Hxournal-idmapInfoRepository [Hxournal-idmapInfo]
queryAll = do m <- ask   
              return (M.elems m)


updateHxournal-idmap :: Hxournal-idmapInfo -> Update Hxournal-idmapInfoRepository (Maybe Hxournal-idmapInfo)
updateHxournal-idmap minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (hxournal-idmap_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

deleteHxournal-idmap :: UUID -> Update Hxournal-idmapInfoRepository (Maybe Hxournal-idmapInfo)
deleteHxournal-idmap uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''Hxournal-idmapInfoRepository [ 'addHxournal-idmap, 'queryHxournal-idmap, 'queryAll, 'updateHxournal-idmap, 'deleteHxournal-idmap] )
