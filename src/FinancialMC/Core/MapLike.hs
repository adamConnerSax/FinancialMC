{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
module FinancialMC.Core.MapLike 
       (
         IsMap(..)
       , IndexedList(..)
       ) where
       
import qualified Data.Map as M
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Foldable as F
import qualified Data.List as L
import Data.Ord (Down(Down),comparing)

import Control.DeepSeq (NFData(rnf))

import qualified Data.Sequence as S
import Data.Sequence ((|>))


class IsMap k v m | m->k, m->v where
  mEmpty::m
  mLookup::k->m->Maybe v
  mInsert::k->v->m->m
  mInsertWith::(v->v->v)->k->v->m->m
  mKeys::m->[k]
  mElems::m->[v]
  mToList::m->[(k,v)]
  mFromList::[(k,v)]->m
  mShow::(Show k, Show v)=>m->String
  
instance Ord k=>IsMap k v (M.Map k v) where
  mEmpty = M.empty
  mLookup = M.lookup
  mInsert = M.insert
  mInsertWith = M.insertWith
  mKeys = M.keys
  mElems = M.elems
  mToList = M.toList
  mFromList = M.fromList
  mShow = M.foldrWithKey (\k v s-> s ++ "\n" ++ show k ++ ": " ++ show v) ""

instance (Eq k,Hashable k)=>IsMap k v (HM.HashMap k v) where
  mEmpty = HM.empty
  mLookup = HM.lookup
  mInsert = HM.insert
  mInsertWith = HM.insertWith
  mKeys = HM.keys
  mElems = HM.elems
  mToList = HM.toList
  mFromList = HM.fromList
  mShow = HM.foldrWithKey (\k v s-> s ++ "\n" ++ show k ++ ": " ++ show v) ""

  
data IndexedList k v = IndexedList {_ilIndex::(M.Map k Int), _ilData::(S.Seq v)}

instance Ord k=>IsMap k v (IndexedList k v) where
  mEmpty = IndexedList M.empty S.empty
  mLookup k (IndexedList im vs) = M.lookup k im >>= (\x-> Just $ S.index vs x) 
  mInsert k v (IndexedList im vs) = 
    let mIndex = M.lookup k im 
    in case mIndex of 
      Nothing->IndexedList (M.insert k (length vs) im) (vs |> v)
      Just index -> IndexedList im (S.update index v vs) 
  
  mInsertWith f k v (IndexedList im vs) = 
    let mIndex = M.lookup k im
    in case mIndex of 
      Nothing -> IndexedList (M.insert k (length vs) im) (vs |> v)
      Just index -> IndexedList im (S.adjust (f v) index vs) 
  
  mKeys (IndexedList im _) = M.keys im
  mElems (IndexedList _ vs) = F.foldl (\lvs v->v:lvs) [] vs
  mToList il@(IndexedList im _) = 
    zip (fst.unzip $ L.sortBy (comparing (\(x,y)->Down y)) (M.toList im)) (mElems il)
  
  mFromList l = foldr (\(k,v) il->mInsert k v il) mEmpty l
  mShow il = foldr (\(k,v) s->s ++ "\n" ++ show k ++ ": " ++ show v) "" (mToList il)
  
instance Functor (IndexedList k) where 
  fmap f (IndexedList im vs) = IndexedList im (fmap f vs) 
  
instance Foldable (IndexedList k) where
  foldr f z (IndexedList im vs) = foldr f z vs 
  
instance Traversable (IndexedList k) where
  traverse g (IndexedList im vs) = fmap (IndexedList im) $ traverse g vs where


instance (NFData k, NFData v)=>NFData (IndexedList k v) where
  rnf (IndexedList index sList) = rnf index `seq` rnf sList `seq` rnf ()

newtype FStore k v = FStore (M.Map k v)

emptyF::FStore k v
emptyF = FStore M.empty

lookupF::Ord k=>k->FStore k v->Maybe v
lookupF key fs = M.lookup key m where
  FStore m = fs

insertF::Ord k=>k->v->FStore k v->FStore k v
insertF key value fs = FStore $ M.insert key value m where
  FStore m = fs

insertWithF::Ord k=>(v->v->v)->k->v->FStore k v->FStore k v
insertWithF f key value fs = FStore $ M.insertWith f key value m where
  FStore m = fs 

toListF::FStore k v->[(k,v)]
toListF fs = M.toList m where
  FStore m = fs 

fromListF::Ord k=>[(k,v)]->FStore k v
fromListF l = FStore $ M.fromList l

instance Functor (FStore k) where
  fmap f fs = FStore $ M.map f m where
    FStore m = fs

instance Foldable (FStore k) where
  foldr f z fs = M.foldr f z m where 
    FStore m = fs
  
instance Traversable (FStore k) where
  traverse g fs = fmap FStore $ traverse g m where 
    FStore m = fs
    
    
  
data MapLike k v = MapLike {
  mlEmpty::MapLike k v,
  mlLookup::Ord k=>k->Maybe v,
  mlInsert::Ord k=>k->v->MapLike k v,
  mlInsertWith::Ord k=>(v->v->v)->k->v->MapLike k v,
  mlKeys::[k],
  mlElems::[v],
  mlToList::[(k,v)],
  mlFromList::Ord k=>[(k,v)]->MapLike k v,
  mlFmap::forall w.(v->w)->MapLike k w,
  mlFoldr::forall w.(v->w->w)->w->w,
  mlTraverse::forall f.Applicative f=>forall w.(v->f w)->f (MapLike k w)
  }

instance Functor (MapLike k) where
  fmap f ml = mlFmap ml f
  
instance Foldable (MapLike k) where
  foldr f z ml = mlFoldr ml f z 
  
instance Traversable (MapLike k) where  
  traverse g ml = mlTraverse ml g


mapToMapLike::M.Map k v->MapLike k v
mapToMapLike m = MapLike 
                 (mapToMapLike $ M.empty) 
                 (\k->M.lookup k m)
                 (\k v->mapToMapLike $ M.insert k v m)
                 (\f k v->mapToMapLike $ M.insertWith f k v m)
                 (M.keys m)
                 (M.elems m)
                 (M.toList m)
                 (\l->mapToMapLike $ M.fromList l)
                 (\f->mapToMapLike $ fmap f m)
                 (\f z->M.foldr f z m)
                 (\g->fmap mapToMapLike $ traverse g m)

