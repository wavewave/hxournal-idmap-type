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
import Data.UUID.Instances 
import Data.Aeson
import Data.Text.Encoding as E
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as B
import Data.Time.Clock

data HXournalIDMapInfo = HXournalIDMapInfo { 
  hxournal_idmap_uuid :: UUID, 
  hxournal_idmap_name :: String, 
  hxournal_idmap_creationtime :: UTCTime 
} deriving (Show,Typeable,Data)



instance FromJSON HXournalIDMapInfo where
  parseJSON (Object v) = HXournalIDMapInfo <$>  v .: "uuid" <*> v .: "name" <*> v .: "creationtime"

instance ToJSON HXournalIDMapInfo where
  toJSON (HXournalIDMapInfo uuid name ctime) = object [ "uuid" .= uuid
                                                      , "name" .= name
                                                      , "creationtime" .= ctime ] 


$(deriveSafeCopy 0 'base ''HXournalIDMapInfo)

type HXournalIDMapInfoRepository = M.Map UUID HXournalIDMapInfo 

addHXournalIDMap :: HXournalIDMapInfo -> Update HXournalIDMapInfoRepository HXournalIDMapInfo 
addHXournalIDMap minfo = do 
  m <- get 
  let (r,m') = M.insertLookupWithKey (\_k _o n -> n) (hxournal_idmap_uuid minfo) minfo m
  put m'
  return minfo
 
queryHXournalIDMap :: UUID -> Query HXournalIDMapInfoRepository (Maybe HXournalIDMapInfo) 
queryHXournalIDMap uuid = do 
  m <- ask 
  return (M.lookup uuid m)

queryAll :: Query HXournalIDMapInfoRepository [HXournalIDMapInfo]
queryAll = do m <- ask   
              return (M.elems m)


updateHXournalIDMap :: HXournalIDMapInfo -> Update HXournalIDMapInfoRepository (Maybe HXournalIDMapInfo)
updateHXournalIDMap minfo = do 
  m <- get 
  let (r,m') = M.updateLookupWithKey (\_ _ -> Just minfo) (hxournal_idmap_uuid minfo) m
  put m'
  maybe (return Nothing) (const (return (Just minfo))) r 

deleteHXournalIDMap :: UUID -> Update HXournalIDMapInfoRepository (Maybe HXournalIDMapInfo)
deleteHXournalIDMap uuid = do 
  m <- get
  let r = M.lookup uuid m  
  case r of 
    Just _ -> do  
      let m' = M.delete uuid m  
      put m' 
      return r
    Nothing -> return Nothing


$(makeAcidic ''HXournalIDMapInfoRepository [ 'addHXournalIDMap, 'queryHXournalIDMap, 'queryAll, 'updateHXournalIDMap, 'deleteHXournalIDMap] )
