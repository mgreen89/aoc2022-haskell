module AoC.Challenge.Day20 (
  day20a,
  day20b,
)
where

import AoC.Solution
import Control.Monad (foldM, forM_, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Foldable (traverse_)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as MV
import Text.Read (readEither)

findIndex :: (PrimMonad m, MV.Unbox a) => (a -> Bool) -> MVector (PrimState m) a -> m Int
findIndex check v = do
  x <- runExceptT $ foldM go () [0 .. MV.length v - 1]
  case x of
    Left i -> pure i
    Right _ -> error "didn't find index"
 where
  go _ i = do
    c <- MV.read v i
    when (check c) $ throwE i

mix :: (PrimMonad m) => MVector (PrimState m) (Int, Int) -> m ()
mix v =
  forM_
    [0 .. MV.length v - 1]
    $ \w -> do
      i <- findIndex ((== w) . fst) v
      a@(_, x) <- MV.read v i
      let j = (i + x) `mod` (MV.length v - 1)
      case compare i j of
        LT -> MV.move (MV.slice i (j - i) v) (MV.slice (i + 1) (j - i) v)
        GT -> MV.move (MV.slice (j + 1) (i - j) v) (MV.slice j (i - j) v)
        EQ -> pure ()
      MV.write v j a

coords :: (PrimMonad m) => MVector (PrimState m) (Int, Int) -> m Int
coords v = do
  start <- findIndex ((== 0) . snd) v
  cs <-
    traverse
      (\i -> snd <$> MV.read v ((start + i) `mod` MV.length v))
      [1000, 2000, 3000]
  pure $ sum cs

solveA :: [Int] -> Int
solveA inp =
  runST $ do
    v <- V.thaw . V.fromList . zip [0 ..] $ inp
    mix v
    coords v

day20a :: Solution [Int] Int
day20a =
  Solution
    { sParse = traverse readEither . lines
    , sShow = show
    , sSolve = Right . solveA
    }

solveB :: Int -> [Int] -> Int
solveB key inp =
  runST $ do
    v <- V.thaw . V.fromList . zip [0 ..] . fmap (* key) $ inp
    traverse_ (const $ mix v) ([1 .. 10] :: [Int])
    coords v

day20b :: Solution [Int] Int
day20b =
  Solution
    { sParse = traverse readEither . lines
    , sShow = show
    , sSolve = Right . solveB 811589153
    }
