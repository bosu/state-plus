{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
import Control.Monad.Trans.State.Plus
import Test.QuickCheck (Arbitrary, arbitrary, CoArbitrary, stdArgs
                            , Args(maxSuccess), quickCheckWithResult)
import Test.QuickCheck.Function
import Control.Monad.Identity
import Control.Monad.State
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.QuickCheck.Test (isSuccess)
import Test.QuickCheck.Property (property)
import Data.List (foldl')
import System.Exit (exitFailure)

newtype P = P { unP :: Bool } deriving (Eq, Arbitrary, Show, CoArbitrary)
instance Function P where
    function = functionMap unP P

newtype SI = SI { unSI :: [Int] } deriving (Eq, Arbitrary, Show, CoArbitrary)
instance Function SI where
    function = functionMap unSI SI

instance Show (StatePlusT SI Identity P) where
    show m = "StatePlus: " ++ show (runIdentity $ runStatePlusT m (SI [0]))

arbSP :: SI -> P -> Bool -> StatePlusT SI Identity P
arbSP si p b = do
    put si
    when b $ mzero
    return p

instance Arbitrary (StatePlusT SI Identity P) where
    arbitrary = do
        s <- arbitrary
        p <- arbitrary
        b <- arbitrary
        return $ arbSP s p b

extract :: StatePlusT SI Identity P -> Maybe (P, SI)
extract m = case fst res of
    Nothing -> Nothing
    Just v -> Just (v, snd res)
    where res = runIdentity $ runStatePlusT m (SI [0])

instance EqProp (StatePlusT SI Identity P) where
    a =-= b = property $ (extract a) == (extract b)

-- shamelessly copied from Checkers
checkB :: TestBatch -> IO ()
checkB (name,tests) =
  do putStrLn $ "\n" ++ name ++ ":"
     mapM_ pr tests
 where
   pr (s,p) = do putStr (padTo (width + 4) ("  "++s ++ ":"))
                 r <- quickCheckWithResult stdArgs { maxSuccess = 500 } p
                 when (not $ isSuccess r) $ exitFailure
   width    = foldl' max 0 (map (length.fst) tests)
   padTo n = take n . (++ repeat ' ')

main :: IO ()
main = do
    checkB (monad (undefined :: StatePlusT SI Identity (P, P, P)))
    checkB (monadOr (undefined :: StatePlusT SI Identity (P, P)))

