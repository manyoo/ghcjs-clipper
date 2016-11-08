{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Clipper.Clipper (
    Clipper(..), mkClipper, area, addPath, addPaths, clear,
    cleanPolygon, cleanPolygons, execute, getBounds, orientation,
    PointPolygonRelation(..), pointInPolygon, simplifyPolygon, simplifyPolygons
    ) where

import GHCJS.Types

import GHCJS.Clipper.Types

newtype Clipper = Clipper {
    unClipper :: ClipperBase
} deriving (JSValWrapper)

foreign import javascript unsafe "new window['ClipperLib']['Clipper']()"
    js_mkClipper :: IO JSVal

mkClipper :: IO Clipper
mkClipper = fromJSVal <$> js_mkClipper

foreign import javascript unsafe "window['ClipperLib']['Clipper']['Area']($1)"
    js_area :: JSVal -> Double

area :: Path -> Double
area = js_area . toJSVal

foreign import javascript unsafe "($4)['AddPath']($1, $2, $3)"
    js_addPath :: JSVal -> Int -> Bool -> JSVal -> IO ()

addPath :: Path -> PolyType -> Bool -> Clipper -> IO ()
addPath p pt closed clp = js_addPath (toJSVal p) (fromEnum pt) closed (toJSVal clp)

foreign import javascript unsafe "($4)['AddPaths']($1, $2, $3)"
    js_addPaths :: JSVal -> Int -> Bool -> JSVal -> IO ()

addPaths :: Paths -> PolyType -> Bool -> Clipper -> IO ()
addPaths ps pt closed clp = js_addPaths (toJSVal ps) (fromEnum pt) closed (toJSVal clp)

foreign import javascript unsafe "($1)['Clear']()"
    js_clear :: JSVal -> IO ()

clear :: Clipper -> IO ()
clear = js_clear . toJSVal

foreign import javascript unsafe "window['ClipperLib']['Clipper']['CleanPolygon']($1, $2)"
    js_cleanPolygon :: JSVal -> Double -> JSVal

cleanPolygon :: Path -> Double -> Path
cleanPolygon p d = fromJSVal $ js_cleanPolygon (toJSVal p) d

foreign import javascript unsafe "window['ClipperLib']['Clipper']['CleanPolygons']($1, $2)"
    js_cleanPolygons :: JSVal -> Double -> JSVal

cleanPolygons :: Paths -> Double -> Paths
cleanPolygons ps d = fromJSVal $ js_cleanPolygons (toJSVal ps) d

foreign import javascript unsafe "($5)['Execute']($1, $2, $3, $4)"
    js_execute :: Int -> JSVal -> Int -> Int -> JSVal -> IO Bool

execute :: ClipType -> Paths -> PolyFillType -> PolyFillType -> Clipper -> IO Bool
execute ct solution subjFt clpFt clp = js_execute (fromEnum ct) (toJSVal solution) (fromEnum subjFt) (fromEnum clpFt) (toJSVal clp)

foreign import javascript unsafe "window['ClipperLib']['Clipper']['GetBounds']($1)"
    js_getBounds :: JSVal -> JSVal

getBounds :: Paths -> IntRect
getBounds = fromJSVal . js_getBounds . toJSVal

foreign import javascript unsafe "window['ClipperLib']['Clipper']['Orientation']($1)"
    js_orientation :: JSVal -> Bool

orientation :: Path -> Bool
orientation = js_orientation . toJSVal


data PointPolygonRelation = PPRInPolygon
                          | PPROnPolygon
                          | PPROutPolygon
                          deriving Eq

toPointPolygonRelation :: Int -> PointPolygonRelation
toPointPolygonRelation i | i == 0    = PPROutPolygon
                         | i < 0     = PPROnPolygon
                         | otherwise = PPRInPolygon

foreign import javascript unsafe "window['ClipperLib']['Clipper']['PointInPolygon']($1, $2)"
    js_pointInPolygon :: JSVal -> JSVal -> Int

pointInPolygon :: IntPoint -> Path -> PointPolygonRelation
pointInPolygon ip p = toPointPolygonRelation $ js_pointInPolygon (toJSVal ip) (toJSVal p)

foreign import javascript unsafe "window['ClipperLib']['Clipper']['SimplifyPolygon']($1, $2)"
    js_simplifyPolygon :: JSVal -> Int -> JSVal

simplifyPolygon :: Path -> PolyFillType -> Paths
simplifyPolygon p pft = fromJSVal $ js_simplifyPolygon (toJSVal p) (fromEnum pft)

foreign import javascript unsafe "window['ClipperLib']['Clipper']['SimplifyPolygons']($1, $2)"
    js_simplifyPolygons :: JSVal -> Int -> JSVal

simplifyPolygons :: Paths -> PolyFillType -> Paths
simplifyPolygons ps pft = fromJSVal $ js_simplifyPolygons (toJSVal ps) (fromEnum pft)
