module Data.Memory where

data Memory = MemNum Int | MemStr String
type Stack = [Memory]

instance Show Memory where
  show (MemNum a) = show a
  show (MemStr a) = a

