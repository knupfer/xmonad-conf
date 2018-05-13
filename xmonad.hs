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
import           XMonad.Layout.NoBorders
import qualified XMonad.StackSet              as W

main :: IO ()
main = do
  writeFile "/home/knupfer/.xmobarrc" $ show xmobarConfig
  mapM_ (spawn . unwords)
    [ [ "xmodmap", "/home/knupfer/git/dotfiles/keyboard/linux/normalkeyboard/xmodmapneo" ]
    , [ "xsetroot", "-cursor_name", "left_ptr" ]
    ]
  xmonad =<< xmobar def
    { modMask            = mod4Mask
    , terminal           = "st -f \"Hasklig:size=10\""
    , keys               = myKeys
    , focusFollowsMouse  = False
    , workspaces         = map show [1..4 :: Int]
    , normalBorderColor  = "#002222"
    , focusedBorderColor = "#008888"
    , borderWidth        = 2
    , manageHook         = manageDocks <+> manageHook def
    , layoutHook         = smartBorders . avoidStruts $ layoutHook def
    , logHook = dynamicLogString xmobarPP
                 {ppLayout = bool "" "F" . ("Full"==)}
                 >>= xmonadPropLog
    }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys XConfig{..} = let m = modMask in  M.fromList $
  [ ((m .|. shiftMask, xK_c)       , kill)
  , ((m .|. shiftMask, xK_e)       , windows W.swapDown) -- swap next
  , ((m .|. shiftMask, xK_k)       , windows W.swapUp)   -- swap prev
  , ((m .|. shiftMask, xK_i)       , sendMessage Shrink) -- shrink
  , ((m .|. shiftMask, xK_a)       , sendMessage Expand) -- expand
  , ((m, xK_h)      , spawn terminal)
  , ((m, xK_q)      , spawn "xmonad --recompile; xmonad --restart")
  , ((m, xK_e)      , spawn "emacsclient -c")
  , ((m .|. shiftMask, xK_p) , P.passPrompt def)
  , ((m, xK_p)      , P.shellPrompt (def{ P.bgColor="#000"
                                        , P.fgColor="grey"
                                        , P.position=P.Top
                                        , P.promptBorderWidth=0
                                        , P.font="xft:DejaVu Sans Mono:size=10:bold"}))
  , ((m, xK_space)  , sendMessage NextLayout)         -- rot algo
  , ((m, xK_i)      , windows W.focusUp)              -- focus prev
  , ((m, xK_a)      , windows W.focusDown)            -- focus next
  , ((m, xK_m)      , windows W.focusMaster)          -- focus mastr
  , ((m, xK_Return) , windows W.swapMaster)           -- swap foc
  , ((m, xK_j)      , withFocused $ windows . W.sink) -- tiling
  , ((m, xK_f)      , sendMessage ToggleStruts)

  ] ++ -- switch and move to workspace
  [ ((n .|. m, k), windows $ f i)
  | (i, k) <- zip workspaces [xK_t, xK_r, xK_n, xK_s]
  , (f, n) <- [(W.greedyView, 0), (W.shift, shiftMask)]
  ] ++ -- switch to screen
  [ ((0 .|. m, k), screenWorkspace i >>= flip whenJust (windows . W.view))
  | (i, k) <- zip [0..] [xK_m, xK_w]
  ]

xmobarConfig :: XMobarConfig
xmobarConfig = Config
  { font         = "xft:DejaVu Sans Mono:size=10:bold"
  , bgColor      = "black"
  , fgColor      = "grey"
  , persistent   = False
  , position     = Top
  , border       = NoBorder
  , borderColor  = "#BFBFBF"
  , lowerOnStart = True
  , hideOnStart  = False
  , commands     =
    map Run [ Date "%a %_d %b %H:%M" "date" 600
            , Battery
              [ "--template" , "<acstatus>"
              , "--Low"      , "20"
              , "--High"     , "80"
              , "--low"      , "#ff5050"
              , "--normal"   , "#ddaa50"
              , "--high"     , "#50aaff"
              , "--"
              , "-o"         , "<fc=#777777>♫☀☼☽☾♪♩♬(<timeleft>)</fc> <left>%" -- discharging
              , "-O"         , "<fc=#ffff00>↯</fc> <left>%" -- charging
              , "-i"         , "<fc=#ffff00>↯</fc> <left>%" -- charged
              ] 40
            , MultiCpu ["--template", "<vbar0><vbar1><vbar2><vbar3><vbar4><vbar5><vbar6><vbar7>" ] 5
            , Volume "default" "Master" [] 5
            , Memory [ "--template", "Mem: <usedratio>%"
                     , "-L"       , "33"
                     , "-H"       , "66"
                     , "--low"    , "green"
                     , "--normal" , "yellow"
                     , "--high"   , "red"
                     ] 5
            , XMonadLog
            ]
  , sepChar  = "%"
  , alignSep = "}{"
  , template = "%default:Master%| %memory% | %XMonadLog% }{%multicpu% | %battery% | <fc=#ee9a00>%date%</fc>"
  }

data XMobarConfig
  = Config
  { font         :: String
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
