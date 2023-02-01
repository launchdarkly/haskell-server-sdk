{-# LANGUAGE CPP #-}

module LaunchDarkly.AesonCompat where

#if MIN_VERSION_aeson(2,0,0)
import           Data.Aeson        (Key)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KeyMap
import           Data.Functor.Identity      (Identity(..), runIdentity)
import qualified Data.Map.Strict as M
#else
import qualified Data.HashMap.Strict as HM
#endif
import qualified Data.Text as T

#if MIN_VERSION_aeson(2,0,0)
type KeyMap = KeyMap.KeyMap

null :: KeyMap v -> Bool
null = KeyMap.null

emptyObject :: KeyMap v
emptyObject = KeyMap.empty

singleton :: T.Text -> v -> KeyMap v
singleton key = KeyMap.singleton (Key.fromText key)

fromList :: [(T.Text, v)] -> KeyMap v
fromList list = KeyMap.fromList (map (\(k, v) -> ((Key.fromText k), v)) list)

toList :: KeyMap v -> [(T.Text, v)]
toList m = map (\(k, v) -> ((Key.toText k), v)) (KeyMap.toList m)

deleteKey :: T.Text -> KeyMap.KeyMap v -> KeyMap.KeyMap v
deleteKey key = KeyMap.delete (Key.fromText key)

lookupKey :: T.Text -> KeyMap.KeyMap v -> Maybe v
lookupKey key = KeyMap.lookup (Key.fromText key)

objectKeys :: KeyMap.KeyMap v -> [T.Text]
objectKeys = map Key.toText . KeyMap.keys

objectValues :: KeyMap.KeyMap v -> [v]
objectValues m = map snd $ KeyMap.toList m

keyToText :: Key -> T.Text
keyToText = Key.toText

insertKey :: T.Text -> v -> KeyMap.KeyMap v -> KeyMap.KeyMap v
insertKey key = KeyMap.insert (Key.fromText key)

filterKeys :: (Key -> Bool) -> KeyMap.KeyMap a -> KeyMap.KeyMap a
filterKeys p = KeyMap.filterWithKey (\key _ -> p key)

filterObject :: (v -> Bool) -> KeyMap.KeyMap v -> KeyMap.KeyMap v
filterObject = KeyMap.filter

adjustKey :: (v -> v) -> Key -> KeyMap.KeyMap v -> KeyMap.KeyMap v
adjustKey f k = runIdentity . KeyMap.alterF (Identity . fmap f) k

mapValues :: (v1 -> v2) -> KeyMap.KeyMap v1 -> KeyMap.KeyMap v2
mapValues = KeyMap.map

mapWithKey :: (T.Text -> v1 -> v2) -> KeyMap.KeyMap v1 -> KeyMap.KeyMap v2
mapWithKey f m = KeyMap.fromMap (M.mapWithKey (\k v -> f (keyToText k) v) (KeyMap.toMap m))

mapMaybeValues :: (v1 -> Maybe v2) -> KeyMap.KeyMap v1 -> KeyMap.KeyMap v2
mapMaybeValues = KeyMap.mapMaybe

keyMapUnion :: KeyMap.KeyMap v -> KeyMap.KeyMap v -> KeyMap.KeyMap v
keyMapUnion = KeyMap.union

foldrWithKey :: (T.Text -> v -> a -> a) -> a -> KeyMap.KeyMap v -> a
foldrWithKey f accum initial = KeyMap.foldrWithKey (\k v a -> f (keyToText k) v a) accum initial
#else
type KeyMap = HM.HashMap T.Text

null :: KeyMap v -> Bool
null = HM.null

emptyObject :: KeyMap v
emptyObject = HM.empty

singleton :: T.Text -> v -> HM.HashMap T.Text v
singleton = HM.singleton

fromList :: [(T.Text, v)] -> KeyMap v
fromList = HM.fromList

toList :: HM.HashMap T.Text v -> [(T.Text, v)]
toList = HM.toList

deleteKey :: T.Text -> HM.HashMap T.Text v -> HM.HashMap T.Text v
deleteKey = HM.delete

lookupKey :: T.Text -> HM.HashMap T.Text v -> Maybe v
lookupKey = HM.lookup

objectKeys :: HM.HashMap T.Text v -> [T.Text]
objectKeys = HM.keys

objectValues :: HM.HashMap T.Text v -> [v]
objectValues = HM.elems

keyToText :: T.Text -> T.Text
keyToText = id

insertKey :: T.Text -> v -> HM.HashMap T.Text v -> HM.HashMap T.Text v
insertKey = HM.insert

filterKeys :: (T.Text -> Bool) -> HM.HashMap T.Text a -> HM.HashMap T.Text a
filterKeys p = HM.filterWithKey (\key _ -> p key)

filterObject :: (v -> Bool) -> HM.HashMap T.Text v -> HM.HashMap T.Text v
filterObject = HM.filter

adjustKey :: (v -> v) -> T.Text -> HM.HashMap T.Text v -> HM.HashMap T.Text v
adjustKey = HM.adjust

mapValues :: (v1 -> v2) -> HM.HashMap T.Text v1 -> HM.HashMap T.Text v2
mapValues = HM.map

mapWithKey :: (T.Text -> v1 -> v2) -> HM.HashMap T.Text v1 -> HM.HashMap T.Text v2
mapWithKey = HM.mapWithKey

mapMaybeValues :: (v1 -> Maybe v2) -> HM.HashMap T.Text v1 -> HM.HashMap T.Text v2
mapMaybeValues = HM.mapMaybe

keyMapUnion :: HM.HashMap T.Text v -> HM.HashMap T.Text v -> HM.HashMap T.Text v
keyMapUnion = HM.union

foldrWithKey :: (T.Text -> v -> a -> a) -> a -> HM.HashMap T.Text v -> a
foldrWithKey = HM.foldrWithKey
#endif
