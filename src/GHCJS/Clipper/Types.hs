{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Clipper.Types (
    JSValWrapper(..), ClipperBase(..), ClipType(..), EndType(..), PolyFillType(..),
    PolyType(..), IntPoint(..), intPoint, IntRect(..), intRect, JoinType(..),
    Path(..), toPath, fromPath, Paths(..), toPaths, fromPaths
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as M

class JSValWrapper v where
    toJSVal :: v -> JSVal
    fromJSVal :: JSVal -> v

newtype ClipperBase = ClipperBase {
    unClipperBase :: JSVal
    }

instance JSValWrapper ClipperBase where
    toJSVal = unClipperBase
    fromJSVal = ClipperBase

-- clip Types
data ClipType = CTIntersection
              | CTUnion
              | CTDifference
              | CTXor
              deriving (Eq, Enum)

data EndType = ETOpenSquare
             | ETOpenRound
             | ETOpenButt
             | ETClosedLine
             | ETClosedPolygon
             deriving (Eq, Enum)

data PolyFillType = PFTEvenOdd
                  | PFTNoneZero
                  | PFTPositive
                  | PFTNegative
                  deriving (Eq, Enum)

data PolyType = PTSubject
              | PTClip
              deriving (Eq, Enum)


newtype IntPoint = IntPoint {
    unIntPoint :: ClipperBase
    } deriving JSValWrapper

foreign import javascript unsafe "new window['ClipperLib']['IntPoint']($1, $2)"
    js_mkIntPoint :: Int -> Int -> JSVal

intPoint :: Int -> Int -> IntPoint
intPoint x y = fromJSVal $ js_mkIntPoint x y

newtype IntRect = IntRect {
    unIntRect :: ClipperBase
    } deriving JSValWrapper

foreign import javascript unsafe "new window['ClipperLib']['IntRect']($1, $2, $3, $4)"
    js_mkIntRect :: Int -> Int -> Int -> Int -> JSVal

intRect :: Int -> Int -> Int -> Int -> IntRect
intRect left top right bottom = fromJSVal $ js_mkIntRect left top right bottom


data JoinType = JTSquare
              | JTRound
              | JTMiter
              deriving (Eq, Enum)


newtype Path = Path {
    unPath :: ClipperBase
    } deriving (JSValWrapper)

toPath :: [IntPoint] -> IO Path
toPath ps = fromJSVal <$> M.toJSVal (toJSVal <$> ps)

fromPath :: Path -> IO [IntPoint]
fromPath p = (fmap fromJSVal . concat) <$> M.fromJSVal (toJSVal p)

newtype Paths = Paths {
    unPaths :: ClipperBase
    } deriving JSValWrapper

toPaths :: [Path] -> IO Paths
toPaths ps = fromJSVal <$> M.toJSVal (toJSVal <$> ps)

fromPaths :: Paths -> IO [Path]
fromPaths ps = (fmap fromJSVal . concat) <$> M.fromJSVal (toJSVal ps)
