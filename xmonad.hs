{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Monad
import           Data.Bool
import qualified Data.Map                     as M
import           Graphics.X11.ExtraTypes.XF86
import           XMonad                       hiding (Position)
import qualified XMonad.Prompt                as P
import qualified XMonad.Prompt.Shell          as P
import qualified XMonad.Prompt.Pass           as P
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.StatusBar
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W

main :: IO ()
main = do
  xmonad
    . ewmhFullscreen
    . ewmh
    . withEasySB (statusBarProp "xmobar" (pure def {ppLayout = bool "" "F" . ("Full"==)})) toggleStrutsKey
    $ def
    { modMask            = mod4Mask
    , terminal           = "st -f \"DejaVu Sans Mono:size=11:bold\""
    , keys               = myKeys
    , focusFollowsMouse  = False
    , workspaces         = map show [1..4 :: Int]
    , normalBorderColor  = "#002222"
    , focusedBorderColor = "#00BBBB"
    , borderWidth        = 3
    , manageHook         = manageDocks <+> manageHook def
    , layoutHook         = smartBorders . avoidStruts $ layoutHook def
    }
    where
      toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
      toggleStrutsKey XConfig{ modMask = m } = (m, xK_f)


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig{..} = let m = modMask in M.fromList $
  [ ((0, xF86XK_AudioMute)         , spawn "amixer -q set Master toggle")
  , ((0, xF86XK_AudioLowerVolume)  , spawn "amixer -q set Master 2%-")
  , ((0, xF86XK_AudioRaiseVolume)  , spawn "amixer -q set Master 2%+")
  , ((0, xF86XK_MonBrightnessDown) , spawn "light -U 1")
  , ((0, xF86XK_MonBrightnessUp)   , spawn "light -A 1")

  , ((m .|. shiftMask, xK_c) , kill)
  , ((m .|. shiftMask, xK_e) , windows W.swapDown) -- swap next
  , ((m .|. shiftMask, xK_k) , windows W.swapUp)   -- swap prev
  , ((m .|. shiftMask, xK_i) , sendMessage Shrink) -- shrink
  , ((m .|. shiftMask, xK_a) , sendMessage Expand) -- expand
  , ((m .|. shiftMask, xK_p) , P.passPrompt def)

  , ((m, xK_h)      , spawn terminal)
  , ((m, xK_e)      , spawn "emacsclient -c")
  , ((m, xK_p)      , P.shellPrompt (def{ P.bgColor="#000"
                                        , P.fgColor="grey"
                                        , P.position=P.Top
                                        , P.promptBorderWidth=0
                                        , P.font="xft:DejaVu Sans Mono:size=11:bold"}))
  , ((m, xK_space)  , sendMessage NextLayout)         -- rot algo
  , ((m, xK_i)      , windows W.focusUp)              -- focus prev
  , ((m, xK_a)      , windows W.focusDown)            -- focus next
  , ((m, xK_m)      , windows W.focusMaster)          -- focus mastr
  , ((m, xK_Return) , windows W.swapMaster)           -- swap foc
  , ((m, xK_j)      , withFocused $ windows . W.sink) -- tiling

  ] ++ -- switch and move to workspace
  [ ((n .|. m, k), windows $ f i)
  | (i, k) <- zip workspaces [xK_t, xK_r, xK_n, xK_s]
  , (f, n) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++ -- switch to screen
  [ ((0 .|. m, k), screenWorkspace i >>= flip whenJust (windows . W.view))
  | (i, k) <- zip [0..] [xK_m, xK_w]
  ]
