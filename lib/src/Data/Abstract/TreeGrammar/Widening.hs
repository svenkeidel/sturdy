{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Data.Abstract.TreeGrammar.Widening where

-- import           Control.Monad.State
-- import           Control.Monad.Reader
-- import           Control.Monad.Writer

-- import           Data.HashMap.Lazy (HashMap)
-- import qualified Data.HashMap.Lazy as M
-- import           Data.HashSet (HashSet)
-- import qualified Data.HashSet as H

-- import           Data.Abstract.TreeGrammar (Grammar,IsGrammar,ProdMap)
-- import qualified Data.Abstract.TreeGrammar as G
-- import qualified Data.Abstract.TreeGrammar.Terminal as T
-- import           Data.Abstract.TreeGrammar.OrdMap (OrdMap)
-- import qualified Data.Abstract.TreeGrammar.OrdMap as O
-- import           Data.Abstract.Widening (Widening,Stable(..))
-- import           Data.Order
-- import           Data.Identifiable
-- import           Data.Semigroup
-- import           Data.Maybe
-- import           Data.Foldable
-- import           Data.Ord (comparing)

-- import qualified Data.List as L

-- widening :: forall n t. (IsGrammar n t,Eq (t n))
--          => (Grammar n t -> Grammar n t -> Grammar n t)
--          -> Widening (Grammar n t)
-- widening w g1 g2
--   | g2 ⊑ g1   = (Stable,g1)
--   | g1 ⊑ g2   = (Instable,w g1 g2)
--   | otherwise = (Instable,w g1 (G.minimize (g1 `G.union` g2)))

-- widening' :: IsGrammar n2 t => Grammar n2 t -> Grammar n2 t
-- widening' = _
--   where
--     go = _

-- data WidenState n1 n2 t = WidenState
--   { ordN1N2  :: OrdMap [n1] [n2]
--   , ordN2N2  :: OrdMap [n2] [n2]
--   , result   :: Grammar n2 t
--   }

-- widening' :: forall n1 n2 t. (IsGrammar n1 t, IsGrammar n2 t)
--           => Grammar n1 t -> Grammar n2 t -> Grammar n2 t
-- widening' g1 g2 =
--   let Just s = execStateT (runReaderT (go H.empty (G.start g1)) []) (WidenState O.empty O.empty g2)
--   in result s
--   where
--     go :: (MonadReader [HashSet n2] m, MonadState (WidenState n1 n2 t) m, MonadPlus m) => HashSet n1 -> n1 -> m ()
--     go seen n1
--       | H.member n1 seen = return ()
--       | otherwise = do
--           ord <- gets ordN1N2
--           let n2s = flatten $ O.upper [n1] ord
--           if depth n1 d1 == 0
--           then mapM_ removeClash (H.filter (\n2 -> depth n2 d2 > 0) n2s)
--           else local (n2s:) $ mapM_ (go (H.insert n1 seen)) (T.nonTerminals (G.lookup n1 g1))

--     d1 = calcDepth g1
--     d2 = calcDepth g2

--     removeClash :: (MonadReader [HashSet n2] m, MonadState (WidenState n1 n2 t) m, MonadPlus m) => n2 -> m ()
--     removeClash n = do
--       ancestors <- ask
--       let ancestors' = L.sortBy (comparing (\x -> depth x d2))
--                      $ filter (\x -> depth x d2 > depth n d2)
--                      $ H.toList (mconcat ancestors)
--       replaceArc n ancestors' `mplus` unionNodes n ancestors'

--     replaceArc :: (MonadReader [HashSet n2] m, MonadState (WidenState n1 n2 t) m, MonadPlus m) => n2 -> [n2] -> m ()
--     replaceArc n ancestors = do
--       ancestor <- findGreaterAncestor n ancestors
--       modifyResult $ M.insert n (G.Rhs mempty [ancestor])

--     findGreaterAncestor :: (MonadState (WidenState n1 n2 t) m, MonadPlus m) => n2 -> [n2] -> m n2
--     findGreaterAncestor _ []     = mzero
--     findGreaterAncestor n (x:xs) = do
--       subset <- modifyN2N2 (g2 {G.start = n } `G.subsetOf'` g2 {G.start = x})
--       if subset
--         then return x
--         else findGreaterAncestor n xs

--     unionNodes :: (MonadReader [HashSet n2] m, MonadState (WidenState n1 n2 t) m, MonadPlus m) => n2 -> [n2] -> m ()
--     unionNodes n ancestors = do
--       ancestor <- findGoodAncestor n ancestors
--       modifyResult $ \ps -> M.insertWith (<>) ancestor (fold (M.lookup n ps))
--                           $ M.insert n (G.Rhs mempty [ancestor]) ps

--     findGoodAncestor :: (MonadState (WidenState n1 n2 t) m) => n2 -> [n2] -> m n2
--     findGoodAncestor _  []  = error "findGoodAncestor n2 []"
--     findGoodAncestor n2 xs  = do
--       g <- gets result
--       return $ minimumBy (comparing (\x -> T.difference (G.lookup n2 g) (G.lookup x g))) xs

-- modifyN1N2 :: MonadState (WidenState n1 n2 t) m => (OrdMap [n1] [n2] -> OrdMap [n1] [n2]) -> m ()
-- modifyN1N2 f = modify $ \s -> s { ordN1N2 = f (ordN1N2 s)}

-- modifyN2N2 :: MonadState (WidenState n1 n2 t) m => (OrdMap [n2] [n2] -> (a,OrdMap [n2] [n2])) -> m a
-- modifyN2N2 f = do
--   s <- get
--   let (a,o') = f (ordN2N2 s)
--   put $ s { ordN2N2 = o' }
--   return a


-- modifyResult :: MonadState (WidenState n1 n2 t) m => (ProdMap n2 t -> ProdMap n2 t) -> m ()
-- modifyResult f = modify $ \s ->
--   let g = (result s)
--   in s { result = g { G.productions = f (G.productions g) }}

-- flatten :: Identifiable x => [HashSet [x]] -> HashSet x
-- flatten x1 = H.fromList [ x | x2 <- x1, x3 <- H.toList x2, x <- x3 ]
       
-- depth :: forall n. Identifiable n => n -> HashMap n (Max Int) -> Max Int
-- depth n = depth' ([n] :: [n])

-- depth' :: (Foldable f, Identifiable n) => f n -> HashMap n (Max Int) -> Max Int
-- depth'  xs d = foldMap (\n -> fromMaybe 0 (M.lookup n d)) xs

-- -- deepest :: (Foldable f, Identifiable n) => f n -> HashMap n (Max Int) -> n
-- -- deepest xs d = case foldl' (\m n -> m <> fmap (fmap (`Arg` n)) (M.lookup n d)) Nothing xs of
-- --   Just (Max (Arg _ n)) -> n
-- --   _ -> error "called function deepest on empty list"

-- calcDepth :: forall n t. IsGrammar n t => Grammar n t -> HashMap n (Max Int)
-- calcDepth g = execState (go H.empty (G.start g)) M.empty
--   where
--     go :: MonadState (HashMap n (Max Int)) m => HashSet n -> n -> m (Max Int)
--     go seen n
--       | H.member n seen = return 0
--       | otherwise       = do
--         d <- execWriterT $ T.traverse (\n' -> tell =<< go (H.insert n seen) n')
--                          $ G.lookup n g
--         let d' = d + 1
--         modify $ M.insert n d'
--         return d'

--     -- widthWidening :: (MonadState (WidenState n1 n2 t) m, MonadPlus m) => n1 -> m ()
--     -- widthWidening n1 = do
--     --   ord <- gets ordN1N2
--     --   let n2s = flatten $ O.upper [n1] ord
--     --   cs <- widenConstr _ union (G.lookup n1 g1) (G.lookup' n2s g2)
--     --   n2 <- union n2s
--     --   modifyResult $ M.insert n2 (G.Rhs cs mempty)


--     -- union :: (MonadState (WidenState n1 n2 t) m, MonadPlus m) => HashSet n2 -> m n2
--     -- union [x] = return x
--     -- union xs = do
--     --   m <- get
--     --   case M.lookup xs (unionMap m) of
--     --     Just n2 -> return n2
--     --     Nothing -> do
--     --       n2 <- fresh
--     --       modifyResult $ M.insert n2 (G.Rhs mempty xs)
--     --       return n2
          
