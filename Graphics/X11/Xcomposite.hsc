


module Graphics.X11.Xcomposite (
    CompositeRedirectMode,
    compositeRedirectAutomatic,
    compositeRedirectManual,
    xcompositeRedirectWindow,
    xcompositeRedirectSubwindows,
    xcompositeUnredirectWindow,
    xcompositeUnredirectSubwindows,
    xcompositeCreateRegionFromBorderClip,
    xcompositeNameWindowPixmap,
    xcompositeGetOverlayWindow,
    xcompositeReleaseOverlayWindow,
    xcompositeQueryExtension,
    xcompositeQueryVersion
    ) where

import Foreign
import Foreign.C.Types
import Graphics.X11.Xlib

#include <X11/Xlib.h>

#include <X11/extensions/Xcomposite.h>

type CompositeRedirectMode = CInt

compositeRedirectAutomatic :: CompositeRedirectMode
compositeRedirectAutomatic = 0

compositeRedirectManual :: CompositeRedirectMode
compositeRedirectManual = 1


foreign import ccall unsafe "XCompositeRedirectWindow"
    xcompositeRedirectWindow :: Display -> Window -> CompositeRedirectMode -> IO ()

foreign import ccall unsafe "XCompositeRedirectSubwindows"
    xcompositeRedirectSubwindows :: Display -> Window -> CompositeRedirectMode -> IO ()

foreign import ccall unsafe "XCompositeUnredirectWindow"
    xcompositeUnredirectWindow :: Display -> Window -> CompositeRedirectMode -> IO ()

foreign import ccall unsafe "XCompositeUnredirectSubwindows"
    xcompositeUnredirectSubwindows :: Display -> Window -> CompositeRedirectMode -> IO ()

xcompositeCreateRegionFromBorderClip :: Display -> Window -> IO Region
xcompositeCreateRegionFromBorderClip dpy win = do
    rp <- cXcompositeCreateRegionFromBorderClip dpy win
    makeRegion rp
foreign import ccall unsafe "XCompositeCreateRegionFromBorderClip"
    cXcompositeCreateRegionFromBorderClip :: Display -> Window -> IO (Ptr Region)

foreign import ccall unsafe "XCompositeNameWindowPixmap"
    xcompositeNameWindowPixmap :: Display -> Window -> IO Pixmap

foreign import ccall unsafe "XCompositeGetOverlayWindow"
    xcompositeGetOverlayWindow :: Display -> Window -> IO Window

foreign import ccall unsafe "XCompositeReleaseOverlayWindow"
    xcompositeReleaseOverlayWindow :: Display -> Window -> IO ()


xcompositeQueryExtension :: Display -> IO (Maybe (CInt, CInt))
xcompositeQueryExtension dpy = wrapPtr2 (cXcompositeQueryExtension dpy) go
    where go False _ _                = Nothing
          go True eventbase errorbase = Just (fromIntegral eventbase, fromIntegral errorbase)

xcompositeQueryVersion :: Display -> IO (Maybe (CInt, CInt))
xcompositeQueryVersion dpy = wrapPtr2 (cXcompositeQueryVersion dpy) go
    where go False _ _        = Nothing
          go True major minor = Just (fromIntegral major, fromIntegral minor)

foreign import ccall unsafe "XCompositeQueryExtension"
    cXcompositeQueryExtension :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

foreign import ccall unsafe "XCompositeQueryVersion"
    cXcompositeQueryVersion :: Display -> Ptr CInt -> Ptr CInt -> IO Bool

-- Borrowed from the Xdamage bindings
wrapPtr2 :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO c) -> (c -> a -> b -> d) -> IO d
wrapPtr2 cfun f =
  withPool $ \pool -> do aptr <- pooledMalloc pool
                         bptr <- pooledMalloc pool
                         ret <- cfun aptr bptr
                         a <- peek aptr
                         b <- peek bptr
                         return (f ret a b)
