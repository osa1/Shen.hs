{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}
module KLambda.Vector where

import           Control.Monad       (liftM)
import           Data.List           (intercalate)
import qualified Data.Vector.Mutable as MV

newtype Vector a = Vector (MV.IOVector (Maybe a))

data VectorException
    = OutOfBounds Int
    | NotInitialized Int
    deriving Show

read :: Vector a -> Int -> IO (Either VectorException (Maybe a))
read (Vector vec) idx
  | idx >= MV.length vec = return (Left (OutOfBounds idx))
  | otherwise = liftM Right (MV.read vec idx)

write :: Vector a -> Int -> a -> IO (Either VectorException ())
write (Vector vec) idx val
  | idx >= MV.length vec = return (Left (OutOfBounds idx))
  | otherwise = liftM Right (MV.write vec idx (Just val))

newVector :: Int -> IO (Vector a)
newVector i = liftM Vector (MV.replicate i Nothing)

vectorToList :: Vector a -> IO [Maybe a]
vectorToList (Vector vec) = iter vec 0
  where
    iter :: MV.IOVector (Maybe a) -> Int -> IO [Maybe a]
    iter vec n
      | n < MV.length vec = do
        e <- MV.read vec n
        rest <- iter vec (n+1)
        return (e:rest)
      | otherwise = return []

listToVector :: [Maybe a] -> IO (Vector a)
listToVector vals = do
    vec <- MV.new (length vals)
    iter vals vec 0
    return (Vector vec)
  where
    iter :: [Maybe a] -> MV.IOVector (Maybe a) -> Int -> IO ()
    iter [] _ _ = return ()
    iter (Nothing:vs) vec idx = do
      MV.write vec idx Nothing
      iter vs vec (idx+1)
    iter (Just v:vs) vec idx = do
      MV.write vec idx (Just v)
      iter vs vec (idx+1)

vectorToString :: Show a => Vector a -> IO String
vectorToString vec = do
    lst <- vectorToList vec
    return $ "[" ++ intercalate " " (map elemstr lst) ++ "]"
  where
    elemstr :: Show a => Maybe a -> String
    elemstr Nothing  = "_"
    elemstr (Just a) = show a
