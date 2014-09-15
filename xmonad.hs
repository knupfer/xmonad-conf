import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

main :: IO ()
main = xmonad defaults

myTerminal :: String
myTerminal = "xterm"

myClickJustFocuses :: Bool -- click on window also passes the click
myClickJustFocuses = False

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myBorderWidth :: (Num a) => a
myBorderWidth = 1

myNormalBorderColor :: String
myNormalBorderColor = "#000000"

myFocusedBorderColor :: String
myFocusedBorderColor = "#005555"

myModMask :: KeyMask -- modifier
myModMask = mod4Mask -- use Super_L which is with my xmodmap AltGr

myWorkspaces :: [String] -- number and name of workspaces
myWorkspaces = fmap show ([1..4] :: [Int])

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ()) -- key bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
 [ ((modm .|. shiftMask, xK_Return) , spawn $ XMonad.terminal conf) -- term
 , ((modm .|. shiftMask, xK_p)      , spawn "gmrun")            -- gmrun
 , ((modm .|. shiftMask, xK_space) , setLayout $ XMonad.layoutHook conf)
 , ((modm .|. shiftMask, xK_c)      , kill)             -- kill foc win
 , ((modm .|. shiftMask, xK_j)    , windows W.swapDown) -- swap foc next
 , ((modm .|. shiftMask, xK_k)    , windows W.swapUp)   -- swap foc prev
 , ((modm .|. shiftMask, xK_q)      , io exitSuccess)   -- quit
 , ((modm .|. shiftMask, xK_i)    , sendMessage Shrink) -- shrink master
 , ((modm .|. shiftMask, xK_a)    , sendMessage Expand) -- expand master
 , ((modm, xK_q)      , spawn "xmonad --recompile; xmonad --restart")
 , ((modm, xK_g)      , refresh)                -- refresh windows
 , ((modm, xK_p)      , spawn "dmenu_run")      -- dmenu
 , ((modm, xK_space)  , sendMessage NextLayout) -- rotate algos
 , ((modm, xK_i)      , windows W.focusUp)      -- focus prev
 , ((modm, xK_a)      , windows W.focusDown)    -- focus next
 , ((modm, xK_m)      , windows W.focusMaster)  -- focus mastr
 , ((modm, xK_Return) , windows W.swapMaster)   -- swap foc mast
 , ((modm, xK_j)      , withFocused $ windows . W.sink) -- tiling
 , ((modm, xK_comma)  , sendMessage (IncMasterN 1))     -- more wins
 , ((modm, xK_period) , sendMessage (IncMasterN (-1)))  -- less wins
 , ((modm, xK_F1) , spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
 -- Toggle the status bar gap
 -- Use this binding with avoidStruts from Hooks.ManageDocks.
 -- See also the statusBar function from Hooks.DynamicLog.
 -- , ((modm , xK_b     ), sendMessage ToggleStruts)
 ] ++ [
   ((m .|. modm, k), windows $ f i) -- switch to workspace N
 | (i, k) <- zip (XMonad.workspaces conf) [xK_t, xK_r, xK_n, xK_s]
 , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- move to workspace
 ] ++ [
 -- Switch to physical/Xinerama screens 1, 2, or 3
 -- shift, Move client to screen 1, 2, or 3
   ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
 | (key, sc) <- zip [xK_w, xK_e, xK_z] [0..]
 , (f, m)    <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings :: XConfig t -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList
 [((modm, button1) , \w -> focus w >> mouseMoveWindow w
                           >> windows W.shiftMaster) -- dragging
 , ((modm, button2) , \w -> focus w >> windows W.shiftMaster) -- raise
 , ((modm, button3) , \w -> focus w >> mouseResizeWindow w    -- resize
                            >> windows W.shiftMaster)]

-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
myLayout :: Choose Tall (Choose (Mirror Tall) Full) a
myLayout = tiled ||| Mirror tiled ||| Full
  where
     tiled   = Tall nmaster delta ratio -- tiling algorithm
     nmaster = 1     -- number of windows in master pane
     ratio   = 1/2   -- proportion of screen occupied by master pane
     delta   = 3/100 -- percent to increment by when resizing

-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore ]

-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook :: Event -> X All
myEventHook = mempty

-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
myLogHook :: X ()
myLogHook = return ()

myStartupHook :: X () -- Startup hook, by default do nothing
myStartupHook = return ()

defaults :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
defaults               = XConfig
  { terminal           = myTerminal -- simple stuff
  , focusFollowsMouse  = myFocusFollowsMouse
  , clickJustFocuses   = myClickJustFocuses
  , borderWidth        = myBorderWidth
  , modMask            = myModMask
  , workspaces         = myWorkspaces
  , normalBorderColor  = myNormalBorderColor
  , focusedBorderColor = myFocusedBorderColor
  , keys               = myKeys -- key bindings
  , mouseBindings      = myMouseBindings
  , layoutHook         = myLayout -- hooks
  , manageHook         = myManageHook
  , handleEventHook    = myEventHook
  , logHook            = myLogHook
  , startupHook        = myStartupHook }

help :: String
help = unlines
  [ "The default modifier key is 'alt'. Default keybindings:"
  , ""
  , "-- launching and killing programs"
  , "mod-Shift-Enter  Launch xterminal"
  , "mod-p            Launch dmenu"
  , "mod-Shift-p      Launch gmrun"
  , "mod-Shift-c      Close/kill the focused window"
  , "mod-Space        Rotate through the available layout algorithms"
  , "mod-Shift-Space  Reset the layouts on the current workSpace to default"
  , "mod-n            Resize/refresh viewed windows to the correct size"
  , ""
  , "-- move focus up or down the window stack"
  , "mod-Tab        Move focus to the next window"
  , "mod-Shift-Tab  Move focus to the previous window"
  , "mod-j          Move focus to the next window"
  , "mod-k          Move focus to the previous window"
  , "mod-m          Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "mod-Return   Swap the focused window and the master window"
  , "mod-Shift-j  Swap the focused window with the next window"
  , "mod-Shift-k  Swap the focused window with the previous window"
  , ""
  , "-- resizing the master/slave ratio"
  , "mod-h  Shrink the master area"
  , "mod-l  Expand the master area"
  , ""
  , "-- floating layer support"
  , "mod-t  Push window back into tiling; unfloat and re-tile it"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "mod-comma  (mod-,)   Increment the number of windows"
  , "mod-period (mod-.)   Deincrement the number of windows"
  , ""
  , "-- quit, or restart"
  , "mod-Shift-q  Quit xmonad"
  , "mod-q        Restart xmonad"
  , "mod-[1..9]   Switch to workSpace N"
  , ""
  , "-- Workspaces & screens"
  , "mod-Shift-[1..9]   Move client to workspace N"
  , "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3"
  , "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3"
  , ""
  , "-- Mouse bindings: default actions bound to mouse events"
  , "mod-button1  Set the window to floating mode and move by dragging"
  , "mod-button2  Raise the window to the top of the stack"
  , "mod-button3  Set the window to floating mode and resize by dragging"]
