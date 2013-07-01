{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}
module KLambda.Vector where

import           Control.Monad       (liftM)
import qualified Data.Text           as T
import qualified Data.Vector.Mutable as MV

newtype Vector a = Vector (MV.IOVector a)

data VectorException
    = OutOfBounds Int
    deriving Show

read :: Vector a -> Int -> IO (Either VectorException a)
read (Vector vec) idx
  | idx >= MV.length vec = return (Left (OutOfBounds idx))
  | otherwise = liftM Right (MV.read vec idx)

write :: Vector a -> Int -> a -> IO (Either VectorException ())
write (Vector vec) idx val
  | idx >= MV.length vec = return (Left (OutOfBounds idx))
  | otherwise = liftM Right (MV.write vec idx val)

newVector :: Int -> a -> IO (Vector a)
newVector i v = liftM Vector (MV.replicate i v)

vectorToList :: Vector a -> IO [a]
vectorToList (Vector vec) = iter vec 0
  where
    iter :: MV.IOVector a -> Int -> IO [a]
    iter vec n
      | n < MV.length vec = do
        e <- MV.read vec n
        rest <- iter vec (n+1)
        return (e:rest)
      | otherwise = return []

listToVector :: [a] -> IO (Vector a)
listToVector vals = do
    vec <- MV.new (length vals)
    iter vals vec 0
    return (Vector vec)
  where
    iter :: [a] -> MV.IOVector a -> Int -> IO ()
    iter [] _ _ = return ()
    iter (v:vs) vec idx = do
      MV.write vec idx v
      iter vs vec (idx+1)

vectorToText :: Show a => Vector a -> IO T.Text
vectorToText vec = do
    lst <- vectorToList vec
    return $ "[" `T.append` T.intercalate " " (map (T.pack . show) lst) `T.append` "]"
