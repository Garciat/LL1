{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ulsa where

import LL1

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

---

data Literal
  = IntLit Integer
  deriving (Show, Eq, Ord)

data Token
  = Lam
  | RArr
  | Let
  | In
  | LParen
  | RParen
  | Eq
  | Plus
  | Slash
  | Star
  | Minus
  | Lit Literal
  | Var String
  | Endl
  deriving (Eq, Ord)

data Label
  = Program
  | DeclList
  | Decl
  | Expr
  | Term
  | TermSuf
  | Fact
  | FactSuf
  | App
  | AppSuf
  | Val
  deriving (Show, Eq, Ord)

---

newtype LLToken = LLToken Token

deriving instance Ord LLToken
deriving instance Eq  LLToken

instance Show LLToken where
  show (LLToken t) =
    case t of
      Lam    -> "λ"
      RArr   -> "→"
      Let    -> "LET"
      In     -> "IN"
      LParen -> "("
      RParen -> ")"
      Eq     -> "="
      Plus   -> "+"
      Slash  -> "/"
      Star   -> "*"
      Minus  -> "-"
      Lit _  -> "LIT"
      Var _  -> "VAR"
      Endl   -> "ENDL"

instance IsRule Token (Rule Label LLToken) where
  toRule = RTer . LLToken

instance IsRule (Literal -> Token) (Rule Label LLToken) where
  toRule f = toRule $ f (IntLit 0)

instance IsRule (String -> Token) (Rule Label LLToken) where
  toRule f = toRule $ f ""

---

program   = rule Program  $ declList

declList  = rule DeclList $ decl *> declList
                        <|> eps

decl      = rule Decl     $ Var *> Eq *> expr *> Endl

expr      = rule Expr     $ Lam *> Var *> RArr *> expr
                        <|> Let *> Var *> expr *> In *> LParen *> expr *> RParen
                        <|> term

term      = rule Term     $ fact *> termSuf

termSuf   = rule TermSuf  $ Plus *> fact
                        <|> Minus *> fact
                        <|> eps

fact      = rule Fact     $ app *> factSuf

factSuf   = rule FactSuf  $ Star *> app
                        <|> Slash *> app
                        <|> eps

app       = rule App      $ val *> appSuf

appSuf    = rule AppSuf   $ app
                        <|> eps

val       = rule Val      $ Lit
                        <|> Var
                        <|> LParen *> expr *> RParen

ulsa = Grammar program
