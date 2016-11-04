{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Clipper.ClipperOffset (
    ClipperOffset(..), mkClipperOffset, coAddPath, coAddPaths, coClear, coExecute
    ) where

import GHCJS.Types
import GHCJS.Clipper.Types

newtype ClipperOffset = ClipperOffset {
    unClipperOffset :: ClipperBase
    } deriving JSValWrapper

foreign import javascript unsafe "new window['ClipperLib']['ClipperOffset']($1, $2)"
    js_mkClipperOffset :: Double -> Double -> IO JSVal

mkClipperOffset :: Double -> Double -> IO ClipperOffset
mkClipperOffset miterLimit roundPrecision = fromJSVal <$> js_mkClipperOffset miterLimit roundPrecision

foreign import javascript unsafe "$(4)['AddPath']($1, $2, $3)"
    js_addPath :: JSVal -> Int -> Int -> JSVal -> IO ()

coAddPath :: Path -> JoinType -> EndType -> ClipperOffset -> IO ()
coAddPath p jt et co = js_addPath (toJSVal p) (fromEnum jt) (fromEnum et) (toJSVal co)

foreign import javascript unsafe "($4)['AddPaths']($1, $2, $3)"
    js_addPaths :: JSVal -> Int -> Int -> JSVal -> IO ()

coAddPaths :: Paths -> JoinType -> EndType -> ClipperOffset -> IO ()
coAddPaths ps jt et co = js_addPaths (toJSVal ps) (fromEnum jt) (fromEnum et) (toJSVal co)

foreign import javascript unsafe "($1)['Clear']()"
    js_clear :: JSVal -> IO ()

coClear :: ClipperOffset -> IO ()
coClear = js_clear . toJSVal

foreign import javascript unsafe "($3)['Execute']($1, $2)"
    js_execute :: JSVal -> Double -> JSVal -> IO ()

coExecute :: Paths -> Double -> ClipperOffset -> IO ()
coExecute ps delta co = js_execute (toJSVal ps) delta (toJSVal co)
