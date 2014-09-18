import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import System.IO

-- TODO: remove the red border when doing fullscreen? tried adding 'smartBorders' to the layoutHook but that didn't work
-- TODO: hook in TopicSpaces, start specific apps on specific workspaces

main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/quxbar/.xmobarrc"
  spawn $ unwords [ "trayer"
                  , "--edge"            , "top"
                  , "--align"           , "center"
                  , "--SetDockType"     , "true"
                  , "--SetPartialStrut" , "true"
                  , "--expand"          , "true"
                  , "--widthtype"       , "request"
                  , "--height"          , "17"
                  , "--transparent"     , "true"
                  , "--tint"            , "0x000000"]
  xmonad defaultConfig
    { modMask            = mod4Mask
    , terminal           = "xterm -rv -b 0 -w 0 -fa 8"
    , keys               = myKeys
    , workspaces         = myWorkspaces
    , normalBorderColor  = "#000000"
    , focusedBorderColor = "#002222"
    , borderWidth        = 3
    , manageHook         = manageDocks <+> manageHook defaultConfig
    , layoutHook         = avoidStruts $ layoutHook defaultConfig
    , logHook            = dynamicLogWithPP $ xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle  = xmobarColor "green" "" . shorten 50
                    }
    }

myWorkspaces :: [String] -- number and name of workspaces
myWorkspaces = fmap show ([1..4] :: [Int])

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ()) -- key bindings
myKeys conf@(XConfig {XMonad.modMask = m}) = M.fromList $
  [ ((m .|. shiftMask, xK_space) , setLayout $ XMonad.layoutHook conf)
  , ((m .|. shiftMask, xK_c)     , kill)               -- kill win
  , ((m .|. shiftMask, xK_j)     , windows W.swapDown) -- swap next
  , ((m .|. shiftMask, xK_k)     , windows W.swapUp)   -- swap prev
  , ((m .|. shiftMask, xK_i)     , sendMessage Shrink) -- shrink
  , ((m .|. shiftMask, xK_a)     , sendMessage Expand) -- expand
  , ((m, xK_h)      , spawn $ XMonad.terminal conf)    -- term
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
    --, ((m, xK_b)  , sendMessage ToggleStruts)
  ] ++
  [ ((n .|. m, k), windows $ f i) -- switch to workspace N
  | (i, k) <- zip (XMonad.workspaces conf) [xK_t, xK_r, xK_n, xK_s]
  , (f, n) <- [(W.greedyView, 0), (W.shift, shiftMask)] -- move to workspace
  ] ++
  [ -- switch to screen 1, 2, or 3
    -- move to screen 1, 2, or 3
    ((n .|. m, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_F1, xK_F2, xK_F3] [0..]
    , (f, n)    <- [(W.view, 0), (W.shift, shiftMask)]]
