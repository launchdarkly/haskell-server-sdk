{-# LANGUAGE CPP #-}
module LaunchDarkly.AesonCompat where

#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson        (Key)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Functor.Identity      (Identity(..), runIdentity)
#else
import qualified Data.HashMap.Strict as HM
#endif
import qualified Data.Text                  as T

#if MIN_VERSION_aeson(2,0,0)
type KeyMap = KeyMap.KeyMap

deleteKey :: Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
deleteKey = KeyMap.delete

objectKeys :: KeyMap.KeyMap v -> [T.Text]
objectKeys = map Key.toText . KeyMap.keys

keyToText :: Key -> T.Text
keyToText = Key.toText

insertKey :: T.Text -> v -> KeyMap.KeyMap v -> KeyMap.KeyMap v
insertKey key = KeyMap.insert (Key.fromText key)

filterKeys :: (Key -> Bool) -> KeyMap.KeyMap a -> KeyMap.KeyMap a
filterKeys p = KeyMap.filterWithKey (\key _ -> p key)

adjustKey :: (v -> v) -> Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
adjustKey f k = runIdentity . KeyMap.alterF (Identity . fmap f) k

keyMapUnion :: KeyMap.KeyMap v -> KeyMap.KeyMap v -> KeyMap.KeyMap v
keyMapUnion = KeyMap.union 
#else
type KeyMap = HM.HashMap T.Text

deleteKey :: T.Text -> HM.HashMap T.Text v -> HM.HashMap T.Text v
deleteKey = HM.delete

objectKeys :: HM.HashMap T.Text v -> [T.Text]
objectKeys = HM.keys

keyToText :: T.Text -> T.Text
keyToText = id

insertKey :: T.Text -> v -> HM.HashMap T.Text v -> HM.HashMap T.Text v
insertKey = HM.insert

filterKeys :: (T.Text -> Bool) -> HM.HashMap T.Text a -> HM.HashMap T.Text a
filterKeys p = HM.filterWithKey (\key _ -> p key)

adjustKey :: (v -> v) -> T.Text -> HM.HashMap T.Text v -> HM.HashMap T.Text v
adjustKey = HM.adjust

keyMapUnion :: HM.HashMap T.Text v -> HM.HashMap T.Text v -> HM.HashMap T.Text v
keyMapUnion = HM.union 
#endif