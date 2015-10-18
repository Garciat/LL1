{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module LL1 where

import Prelude hiding ((*>))

import Control.Monad
import Control.Monad.ST
import Control.Monad.Writer.Strict

import Data.IORef
import Data.STRef
import Data.List
import Data.Maybe

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb mf = do
  b <- mb
  when b mf

---

data Rule t a where
  RTag :: t -> Rule t a -> Rule t a
  
  RSeq :: Rule t a -> Rule t a -> Rule t a
  RAlt :: Rule t a -> Rule t a -> Rule t a
  
  RTer :: a -> Rule t a
  
  REps :: Rule t a
  REof :: Rule t a
  
  RPretty :: Rule t a -> Rule t a
  deriving (Ord)

instance (Eq t, Eq a) => Eq (Rule t a) where
  (RTag t _) == (RTag t' _)  = t == t'
  (RSeq a b) == (RSeq a' b') = a == a' && b == b'
  (RAlt a b) == (RAlt a' b') = a == a' && b == b'
  (RTer x)   == (RTer x')    = x == x'
  (REps)     == (REps)       = True
  (REof)     == (REof)       = True
  _          == _            = False

showRule :: (Show t, Show a) => Rule t a -> String
showRule rule =
  case rule of
    RTag t a  -> show t ++ " -> " ++ go a
    otherwise -> go rule
  where
    go (RPretty (RTer a)) = show a
    go (RPretty a) = go a
    
    go (RTag t a) = "{" ++ show t ++ "}"
    go (RSeq a b)  = go a ++ " "   ++ go b
    go (RAlt a b)  = go a ++ " | " ++ go b
    go (RTer a)    = "'" ++ show a ++ "'"
    go (REps)      = "ε"
    go (REof)      = "<<EOF>>"

instance (Show t, Show a) => Show (Rule t a) where
  show = showRule

---

class IsRule a r | a -> r where
  toRule :: a -> r

instance IsRule (Rule t a) (Rule t a) where
  toRule = id

---

infixr 6 *>
infixl 4 <|>

eof     = REof
eps     = REps
rule    = RTag 
a  *> b = RSeq (toRule a) (toRule b)
a <|> b = RAlt (toRule a) (toRule b)

---

data Grammar t a = Grammar { start :: Rule t a }

---

traversal :: (Ord t) => (s -> Rule t a -> s) -> s -> Rule t a -> s
traversal f s rule = runST $ do
  skipsR <- newSTRef Set.empty
  queueR <- newSTRef (Seq.singleton rule)
  sR <- newSTRef s
  
  let observe r = do
      modifySTRef sR (flip f r)
  
  let putQ r = do
      modifySTRef queueR (Seq.|> r)
  
  let addSkip t = do
      modifySTRef skipsR (Set.insert t)
  
  let hasSkip t = do
      Set.member t <$> readSTRef skipsR
  
  let putSkip r@(RTag t a) ifNew = do
      whenM (not <$> hasSkip t) $ do
        addSkip t
        putQ a
        ifNew
  
  let popQ = do
      top <- flip Seq.index 0 <$> readSTRef queueR
      modifySTRef queueR (Seq.drop 1)
      return top
  
  let isEmptyQ = do
      Seq.null <$> readSTRef queueR
  
  let withPopQ f = do
      whenM (not <$> isEmptyQ) $ do
        popQ >>= f
  
  let go = do
      withPopQ $ \r -> do
        case r of
          RSeq a b  -> observe r >> putQ a >> putQ b
          RAlt a b  -> observe r >> putQ a >> putQ b
          RTag t a  -> putSkip r (observe r)
          r         -> observe r
        go
  
  go >> readSTRef sR

nonterms :: (Ord t, Eq a) => Rule t a -> [Rule t a]
nonterms = nub . traversal f []
  where
    f ts r@RTag{} = r : ts
    f ts _        = ts

terminals :: (Ord t, Eq a) => Rule t a -> [Rule t a]
terminals = nub . traversal f []
  where
    f ts r@RTer{} = r : ts
    f ts _        = ts

tagAlts :: Rule t a -> [Rule t a]
tagAlts (RTag t a) = go a
  where
    go (RAlt a b) = go a ++ go b
    go r          = [r]

isEps REps = True
isEps _    = False

hasEps = or . map isEps

isTag RTag{} = True
isTag _      = False

tag (RTag t _) = t

first :: (Ord t, Eq a) => Rule t a -> [Rule t a]
first = go Set.empty
  where
    go ks r@REps{} = [r]
    go ks r@RTer{} = [r]
    
    go ks r@(RTag t _) =
      if Set.member t ks then
        []
      else
        concatMap (go (Set.insert t ks)) (tagAlts r)
    
    go ks (RSeq a b) =
      let y  = go ks a
          y' = y \\ [eps] in
        if hasEps y then
          y' ++ go ks b
        else
          y'

follow :: (Ord t, Eq a) => Grammar t a -> Rule t a -> [Rule t a]
follow gram = go (nonterms $ start gram)
  where
    go nts x@RTag{} = nub $ execWriter $ do
      mapM_ f nts
      when (x == start gram) $
        tell [eof]
      where
        f nt = mapM_ g (tagAlts nt)
          where
            g (RSeq a b) = do
              g b
              when (a == x) $ do
                let y  = first b
                let y' = y \\ [eps]
                tell y'
                when (hasEps y) $
                  tell $ go (nts \\ [x]) nt
            
            g r =
              when (r == x) $
                tell $ go (nts \\ [x]) nt

---

printParseTable :: (Ord t, Ord a, Show t, Show a) => Grammar t a -> IO ()
printParseTable gram = do
  let ts  = [eof] ++ terminals (start gram)
  let nts = sortOn tag $ nonterms (start gram)
  
  forM_ nts $ \nt -> do
    let alts = tagAlts nt
    row <- newIORef Map.empty
    forM_ alts $ \alt -> do
      let fts = first alt
      forM_ fts $ \ft -> do
        whenM (Map.member ft <$> readIORef row) $
          error ("not LL1: " ++ show (nt, ft))
        modifyIORef row (Map.insert ft alt)
      when (hasEps fts) $ do
        let fws = follow gram nt
        forM_ fws $ \fw -> do
          whenM (Map.member fw <$> readIORef row) $
            error ("not LL1: " ++ show (nt, fw))
          modifyIORef row (Map.insert fw alt)
    
    putStrLn (show nt)
    forM_ ts $ \t -> do
      mrule <- Map.lookup t <$> readIORef row
      case mrule of
        Just rule -> putStr ("\t" ++ show (RPretty t) ++ "\t: " ++ show (RPretty rule) ++ "\n")
        otherwise -> return ()
    putStr "\n"
