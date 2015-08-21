module Main where

import qualified Data.Map                     as M
import           Graphics.X11.ExtraTypes.XF86
import           System.IO
import           XMonad hiding (Position)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W
import           XMonad.Util.Run              (spawnPipe)

main :: IO ()
main = do
  writeFile "/home/knupfer/.xmobarrc" . unwords
                                      . ("Config":)
                                      . tail
                                      . words
                                      $ show xmobarConfig
  xmproc <- spawnPipe "/home/knupfer/.xmonad/cpu-bar | xmobar"
  mapM_ (spawn . unwords)
    [
      [ "sleep"     , "0.5;"
      , "setxkbmap" , "de;"
      , "xmodmap"   , "/home/knupfer/git/dotfiles/keyboard/linux/normalkeyboard/xmodmapneo"
      ]
    , [ "emacs" , "--daemon" ]
    , [ "pkill" , "trayer"   ]
    , [ "trayer"
      , "--edge"            , "top"
      , "--align"           , "center"
      , "--SetDockType"     , "true"
      , "--SetPartialStrut" , "true"
      , "--expand"          , "true"
      , "--width"           , "10"
      , "--height"          , "17"
      , "--transparent"     , "true"
      , "--tint"            , "0x000000"
      ]
    ]
  xmonad defaultConfig
    { modMask            = mod4Mask
    , terminal           = "xterm -rv -b 0 -w 0"
    , keys               = myKeys
    , focusFollowsMouse  = False
    , mouseBindings      = myMouseBindings
    , workspaces         = myWorkspaces
    , normalBorderColor  = "#002222"
    , focusedBorderColor = "#008888"
    , borderWidth        = 2
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
  , ((0, xF86XK_MonBrightnessUp)   , spawn "light -A 5")
  , ((0, xF86XK_MonBrightnessDown) , spawn "light -U 5")
  , ((0, xF86XK_AudioLowerVolume)  , spawn "amixer set Master 2%-")
  , ((0, xF86XK_AudioRaiseVolume)  , spawn "amixer set Master 2%+")
  , ((0, xF86XK_AudioMute)         , spawn "amixer set Master toggle")
  , ((m, xK_h)      , spawn $ XMonad.terminal conf) -- term
  , ((m, xK_q)      , spawn "xmonad --recompile; xmonad --restart")
  , ((m, xK_e)      , spawn "emacsclient -c")
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

xmobarConfig :: XMobarConfig
xmobarConfig = XMobarConfig
  { font     = "xft:DejaVu Sans Mono:size=10:bold"
  , bgColor  = "black"
  , fgColor  = "grey"
  , persistent   = False
  , position = Top
  , border       = NoBorder
  , borderColor  = "#BFBFBF"
  , lowerOnStart = True
  , hideOnStart  = False
  , commands =
    map Run [ Date "%a %_d %b %H:%M" "date" 600
            , Battery
              [ "--template" , "<acstatus>"
              , "--Low"      , "20"
              , "--High"     , "20"
              , "--low"      , "#ff5050"
              , "--normal"   , "#ddaa50"
              , "--high"     , "#50aaff"
              , "--"
              , "-o"         , "<left>% <fc=#777777>(<timeleft>)</fc>" -- discharging
              , "-i"         , "<left>%" -- charged
              ] 40
            , StdinReader
            ]
  , sepChar  = "%"
  , alignSep = "}{"
  , template = " %StdinReader% |}{| %battery% | <fc=#ee9a00>%date%</fc> "
  }

data XMobarConfig = XMobarConfig { font         :: String
                                 , bgColor      :: String
                                 , fgColor      :: String
                                 , position     :: Position
                                 , lowerOnStart :: Bool
                                 , hideOnStart  :: Bool
                                 , persistent   :: Bool
                                 , border       :: Border
                                 , borderColor  :: String
                                 , commands     :: [Run Command]
                                 , sepChar      :: String
                                 , alignSep     :: String
                                 , template     :: String
                                 } deriving Show

data Position = Top    | TopW    Align Int | TopSize    Align Int Int
              | Bottom | BottomW Align Int | BottomSize Align Int Int
              | Static { xpos  :: Int, ypos   :: Int
                       , width :: Int, height :: Int
                       } deriving Show

data Align = L | C | R deriving Show

data Border = TopB    | TopBM    Int
            | BottomB | BottomBM Int
            | FullB   | FullBM   Int
            | NoBorder deriving Show

data Command = Uptime                                [String] Int
             | Weather            String             [String] Int
             | Network            String             [String] Int
             | DynNetwork                            [String] Int
             | Wireless           String             [String] Int
             | Memory                                [String] Int
             | Swap                                  [String] Int
             | Cpu                                   [String] Int
             | MultiCpu                              [String] Int
             | Battery                               [String] Int
             | BatteryP           [String]           [String] Int
             | TopProc                               [String] Int
             | TopMem                                [String] Int
             | DiskU              [(String, String)] [String] Int
             | DiskIO             [(String, String)] [String] Int
             | ThermalZone        Int                [String] Int
             | Thermal            String             [String] Int
             | CpuFreq                               [String] Int
             | CoreTemp                              [String] Int
             | Volume             String String      [String] Int
             | MPD                                   [String] Int
             | Mpris1             String             [String] Int
             | Mpris2             String             [String] Int
             | Mail               [(String, String)] String
             | Mbox               [(String, String, String)] [String] String
             | XPropertyLog       String
             | NamedXPropertyLog  String String
             | Brightness         [String]                    Int
             | Kbd                [(String, String)]
             | Locks
             | Com                String [String] String      Int
             | StdinReader
             | Date               String String               Int
             | DateZone           String String String String Int
             | CommandReader      String String
             | PipeReader         String String
             | BufferedPipeReader String [(Int, Bool, String)]
             | XMonadLog
             deriving Show

data Run a = Run Command

instance Show a => Show (Run a) where
  show (Run x) = unwords ["Run", show x]
