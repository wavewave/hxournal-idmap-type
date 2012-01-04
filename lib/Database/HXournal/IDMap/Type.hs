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

data HXournalIDMapInfo = HXournalIDMapInfo { 
  hxournal_idmap_uuid :: UUID, 
  hxournal_idmap_name :: String
} deriving (Show,Typeable,Data)


instance FromJSON UUID where
  parseJSON x = do r <- return . fromString . C.unpack . E.encodeUtf8 =<< parseJSON x
                   case r of 
                     Nothing -> fail ("UUID parsing failed " ++ show x )
                     Just uuid -> return uuid 

instance ToJSON UUID where
  toJSON = toJSON . E.decodeUtf8 . C.pack . toString 

instance FromJSON HXournalIDMapInfo where
  parseJSON (Object v) = HXournalIDMapInfo <$>  v .: "uuid" <*> v .: "name"

instance ToJSON HXournalIDMapInfo where
  toJSON (HXournalIDMapInfo uuid name) = object [ "uuid" .= uuid , "name" .= name ] 


instance SafeCopy UUID where 
  putCopy uuid = contain $ safePut (toByteString uuid) 
  getCopy = contain 
            $ maybe (fail "cannot parse UUID") return . fromByteString 
              =<< safeGet

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
