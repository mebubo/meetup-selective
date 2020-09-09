{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Selective (Over(..), ifS, Selective)
import Data.Maybe (fromJust)
import Data.Foldable (Foldable(fold))
import Data.Traversable (sequenceA)
import Data.List (intercalate)
import Data.Functor.Identity (runIdentity, Identity(..))

newtype Task k v = Task {
    run :: forall f. Selective f => (k -> f v) -> f v
}

type Script k v = k -> Maybe (Task k v)

script :: Script FilePath String
script "release.tar" = Just $ Task $ \fetch -> tar [fetch "LICENSE", fetch "exe"]
script "exe" = Just $ Task $ \fetch ->
    let src = fetch "src.ml"
        cfg = fetch "config"
        libc = fetch "lib.c"
        libml = fetch "lib.ml"
    in compile [src, ifS (parse cfg) libc libml]
script _ = Nothing

compile :: Applicative f => [f String] -> f String
compile files = (("compiling: " ++) . intercalate " ") <$> sequenceA files

parse :: Functor f => f String -> f Bool
parse = fmap (== "true")

tar :: Applicative f => [f String] -> f String
tar files = (("tar: " ++) . intercalate " ") <$> sequenceA files

dependenciesOver :: Task k v -> [k]
dependenciesOver task = getOver $ run task (\k -> Over [k])

fileSystem :: String -> Identity String
fileSystem "config" = Identity "false"
fileSystem x = pure x

execute :: Task String String -> String
execute task = runIdentity $ run task fileSystem

main :: IO ()
main = do
    print $ dependenciesOver $ fromJust $ script "release.tar"
    print $ dependenciesOver $ fromJust $ script "exe"
    print $ execute $ fromJust $ script "exe"
    print $ execute $ fromJust $ script "release.tar"
