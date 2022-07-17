module Stargaze.Aggregate
  ( aggregate,
    MergeMap (..),
    ProjectAgg (..),
  )
where

import qualified Data.Aeson.KeyMap as HasMap
import Data.Foldable (foldl')
import Data.Function (on)
import Data.HashMap (Map)
import qualified Data.HashMap as HashMap
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Stargaze.Types (Author (authorId), Project (projectLanguage, projectOwner, projectTopics))

newtype MergeMap k v = MergeMap {getMergeMap :: Map k v} deriving (Show)

instance (Semigroup v, Eq k, Hashable k, Ord k) => Semigroup (MergeMap k v) where
  MergeMap x <> MergeMap y = MergeMap $ HashMap.unionWith (<>) x y

instance (Semigroup v, Eq k, Hashable k, Ord k) => Monoid (MergeMap k v) where
  mempty = MergeMap mempty
  mappend = (<>)

data ProjectAgg = ProjectAgg
  { aggByTag :: MergeMap String [Project],
    aggByLang :: MergeMap String [Project],
    aggByOwner :: MergeMap Int64 [Project],
    aggAuthor :: Map Int64 Author
  }
  deriving (Show)

instance Semigroup ProjectAgg where
  x <> y =
    ProjectAgg
      { aggByTag = on (<>) aggByTag x y,
        aggByLang = on (<>) aggByLang x y,
        aggByOwner = on (<>) aggByOwner x y,
        aggAuthor = on (<>) aggAuthor x y
      }

instance Monoid ProjectAgg where
  mappend = (<>)
  mempty = ProjectAgg mempty mempty mempty mempty

singletonAggregate :: Project -> ProjectAgg
singletonAggregate project =
  ProjectAgg
    { aggByTag = foldl' (<>) mempty _singletonTags,
      aggByLang = MergeMap $ HashMap.singleton _language [project],
      aggByOwner = MergeMap $ HashMap.singleton _ownerId [project],
      aggAuthor = HashMap.singleton _ownerId _owner
    }
  where
    _owner = projectOwner project
    _ownerId = authorId _owner
    _tags = projectTopics project
    _language = fromMaybe "Unknown" $ projectLanguage project
    _singletonTags = map (\tag -> MergeMap $ HashMap.singleton tag [project]) _tags

aggregate :: [Project] -> ProjectAgg
aggregate = foldl' (\acc project -> acc <> singletonAggregate project) mempty
