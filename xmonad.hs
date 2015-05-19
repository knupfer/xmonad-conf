module Main where

import qualified Data.Map                     as M
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.Run              (spawnPipe)

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  spawn "xss-lock slock"
  spawn "emacs --daemon"
  spawn $ unwords [ "pkill"             , "trayer;"
                  , "trayer"
                  , "--edge"            , "top"
                  , "--align"           , "center"
                  , "--SetDockType"     , "true"
                  , "--SetPartialStrut" , "true"
                  , "--expand"          , "true"
                  , "--width"           , "10"
                  , "--height"          , "17"
                  , "--transparent"     , "true"
                  , "--tint"            , "0x000000"]
  xmonad defaultConfig
    { modMask            = mod4Mask
    , terminal           = "xterm -rv -b 0 -w 0 -fa 8"
    , keys               = myKeys
    , focusFollowsMouse  = False
    , mouseBindings      = myMouseBindings
    , workspaces         = myWorkspaces
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#008888"
    , borderWidth        = 1
    , manageHook         = manageDocks <+> manageHook defaultConfig
    , layoutHook         = smartBorders $ avoidStruts $ layoutHook defaultConfig
    , logHook            = dynamicLogWithPP $ xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle  = const ""
                    , ppLayout = \x -> if x == "Full" then " : " ++ x else ""
                    , ppSep    = ""
                    }
    }

myWorkspaces :: [String] -- number and name of workspaces
myWorkspaces = fmap show ([1..4] :: [Int])

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ()) -- key bindings
myKeys conf@(XConfig {modMask = m}) = M.fromList $
  [ ((m .|. shiftMask, xK_c)       , kill)               -- kill win
  , ((m .|. shiftMask, xK_e)       , windows W.swapDown) -- swap next
  , ((m .|. shiftMask, xK_k)       , windows W.swapUp)   -- swap prev
  , ((m .|. shiftMask, xK_i)       , sendMessage Shrink) -- shrink
  , ((m .|. shiftMask, xK_a)       , sendMessage Expand) -- expand
  , ((0, xF86XK_MonBrightnessUp)   , spawn "xbacklight +5")
  , ((0, xF86XK_MonBrightnessDown) , spawn "xbacklight -5")
  , ((0, xF86XK_AudioLowerVolume)  , spawn "amixer set Master 2%-")
  , ((0, xF86XK_AudioRaiseVolume)  , spawn "amixer set Master 2%+")
  , ((0, xF86XK_AudioMute)         , spawn "amixer set Master toggle")
  , ((m, xK_h)      , spawn $ XMonad.terminal conf) -- term
  , ((m, xK_q)      , spawn "xmonad --recompile; xmonad --restart")
  , ((m, xK_e)      , spawn "emacs")
  , ((m, xK_p)      , spawn "dmenu_run")              -- dmenu
  , ((m, xK_space)  , sendMessage NextLayout)         -- rot algo
  , ((m, xK_i)      , windows W.focusUp)              -- focus prev
  , ((m, xK_a)      , windows W.focusDown)            -- focus next
  , ((m, xK_m)      , windows W.focusMaster)          -- focus mastr
  , ((m, xK_Return) , windows W.swapMaster)           -- swap foc
  , ((m, xK_j)      , withFocused $ windows . W.sink) -- tiling
  , ((m, xK_comma)  , sendMessage (IncMasterN 1))     -- more wins
  , ((m, xK_period) , sendMessage (IncMasterN (-1)))  -- less wins
  , ((m, xK_f)      , sendMessage ToggleStruts)
  ] ++ -- switch and move to workspace
  [ ((n .|. m, k), windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) [xK_t, xK_r, xK_n, xK_s]
  , (f, n) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++ -- switch and move to screen
  [ ((n .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..]
    , (f, n)    <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {modMask = m}) = M.fromList
  [((m, button1) , \w -> focus w >> mouseMoveWindow w
  >> windows W.shiftMaster) -- dragging
  , ((m, button2) , \w -> focus w >> windows W.shiftMaster) -- raise
  , ((m, button3) , \w -> focus w >> mouseResizeWindow w    -- resize
  >> windows W.shiftMaster)]
