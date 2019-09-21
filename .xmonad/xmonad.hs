{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow (first)
import Control.Monad
import Data.Char (isSpace)
import qualified Data.Map as M
import Data.Monoid (Endo)
import Graphics.X11.ExtraTypes.XF86
import System.Environment (getArgs)
import System.IO
import System.Process.Typed (proc, startProcess)
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.Search
import XMonad.Actions.Submap
import XMonad.Actions.Volume
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Magnifier (MagnifyMsg(..))
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import qualified XMonad.StackSet as W
import qualified XMonad.Util.Brightness as Bright
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

------------------------------------------- Keybinings Refresher
-- * mod-space: Rotate through available layout algorithms
-- * mod-j : Move focus to the next window
-- * mod-l : Expand the master area
-- * mod-i : Shrink the master area (Custom binding)
-- * mod-shift-/ : Show the keybindings!
-- * mod-return: Make the focused window with the master window
-- * mod-m : Focus on the master window
-- * mod-j : Focus on the next window
-- * mod-k : Focus on the previous window
-- * mod-f : Toggle fullscreen (xmobar presence)
-- * mod-o : Go to the specific application in that workspace
-- * mod-b : Bring the specific application from that workspace
-- * mod-shift-j: Swap the focused window with the next window
-- * mod-shift-k: Swap the focused window with the previous window
-- * mod-,: Increase the number of window in master area
-- * mod-.: Decrease the number of window in master area
-- * Searching: Select world + mod-(g|v|w)
-- * mod-q: Restart xmonad
--------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["main", "web", "chat", "dev", "media", "float", "misc"]

myTerminal :: String
myTerminal = "urxvt"

myManageHook :: XMonad.Query (Endo WindowSet)
myManageHook =
  composeAll . concat $
  [ [isFullscreen --> doFullFloat] -- For Media Players
    -- Applications that go to web
  , [className =? b --> viewShift "web" | b <- myClassWebShifts]
    -- Applications that go to chat
  , [resource =? c --> doF (W.shift "chat") | c <- myClassChatShifts]
  , [resource =? c --> doF (W.shift "dev") | c <- myClassDevShifts]
  , [resource =? c --> doF (W.shift "media") | c <- myClassMediaShifts]
  , [resource =? c --> doF (W.shift "float") | c <- myClassFloatShifts]
  , [resource =? c --> doF (W.shift "misc") | c <- myClassMiscShifts]
  , [resource =? c --> doF (W.shift "main") | c <- myClassMainShifts]
  ]
  where
    viewShift = doF . liftM2 (.) W.greedyView W.shift
    myClassWebShifts = ["google-chrome", "Google-chrome"]
    myClassChatShifts = ["Pidgin", "eboard", "slack"]
    myClassDevShifts = []
    myClassMediaShifts = ["mplayer", "vlc"]
    myClassFloatShifts = ["gimp", "SMPlayer", "smplayer"]
    myClassMiscShifts = ["nautilus", "seahorse"]
    myClassMainShifts = [".urxvt-wrapped"]

sibiStartupHook :: X ()
sibiStartupHook = do
  as <- io getArgs
  Bright.setBrightness 1260
  setWMName "LG3D"
  when (null as) $ do
    void $ startProcess (proc myTerminal ["-e", "screen"])
    void $ (startProcess (proc "emacs" ["--daemon"]))
    void $ startProcess "google-chrome-stable"
    void $ startProcess "seahorse"

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $
    withUrgencyHook
      NoUrgencyHook
      def
        { manageHook =
            manageDocks <+>
            myManageHook <+>
            namedScratchpadManageHook sibiScratchPads <+> manageHook def
      -- No red border for media players
        , terminal = myTerminal
        , startupHook = sibiStartupHook
        , layoutHook = smartBorders $ avoidStruts $ layoutHook def
        , logHook =
            dynamicLogWithPP
              xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                }
        , handleEventHook =
            handleEventHook def <+> fullscreenEventHook <+> docksEventHook
        , modMask = mod4Mask -- Rebind Mod to the Windows key
        , workspaces = myWorkspaces
        } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((0, xK_Print), spawn "xfce4-screenshooter")
    , ((mod4Mask, xK_x), spawn "xkill")
    , ((mod4Mask, xK_c), kill)
    , ((mod4Mask, xK_p), shellPrompt sibiXPConfig)
    , ((mod4Mask, xK_h), spawn "/home/sibi/.emacs_everywhere/bin/run")
    , ((0, xF86XK_MonBrightnessUp), Bright.increase)
    , ((0, xF86XK_MonBrightnessDown), Bright.decrease)
    , ( (mod4Mask, xK_s)
      , submap $ searchEngineMap $ promptSearch greenSibiXPConfig)
    , ((mod4Mask, xK_f), submap $ searchEngineMap $ selectSearch)
    , ((mod4Mask, xK_g), spawn "unity-control-center")
    , ((0, xF86XK_AudioRaiseVolume), raiseVolume 2 >> return ())
    , ((0, xF86XK_AudioLowerVolume), lowerVolume 2 >> return ())
    , ((0, xF86XK_AudioMute), toggleMute >> return ())
    , ((mod4Mask, xK_b), windowPrompt sibiXPConfig Bring allApplications)
    , ((mod4Mask, xK_o), windowPrompt sibiXPConfig Goto allApplications)
    , ((mod4Mask, xK_m), sendMessage MagnifyMore)
    , ((mod4Mask .|. shiftMask, xK_m), sendMessage MagnifyLess)
    , ((mod4Mask, xK_i), sendMessage Shrink)
    , ((mod4Mask, xK_f), sendMessage ToggleStruts)
    , ((mod4Mask, xK_slash), switchProjectPrompt sibiXPConfig)
    , ( (mod4Mask .|. controlMask, xK_k)
      , namedScratchpadAction sibiScratchPads "keepass")
    ]

sibiXPConfig :: XPConfig
sibiXPConfig =
  def
    { alwaysHighlight = True
    , promptKeymap = sibiEmacsKeymap
    , position = Top
    , font = "xft:Ubuntu Mono:size=12:bold:antialias=true"
    , completionKey = (controlMask, xK_i)
    , searchPredicate = fuzzyMatch
    }

greenSibiXPConfig :: XPConfig
greenSibiXPConfig =
  sibiXPConfig
    { alwaysHighlight = False
    , fgColor = "green"
    , bgColor = "black"
    , promptBorderWidth = 0
    }

sibiEmacsKeymap :: M.Map (KeyMask, KeySym) (XP ())
sibiEmacsKeymap = sibiEmacsKeymap' isSpace

-- Modified from source to suit my key binding
sibiEmacsKeymap' :: (Char -> Bool) -> M.Map (KeyMask, KeySym) (XP ())
sibiEmacsKeymap' p =
  M.fromList $
  map
    (first $ (,) controlMask)
    [ (xK_z, killBefore) --kill line backwards
    , (xK_k, killAfter) -- kill line fowards
    , (xK_a, startOfLine) --move to the beginning of the line
    , (xK_e, endOfLine) -- move to the end of the line
    , (xK_d, deleteString Next) -- delete a character foward
    , (xK_b, moveCursor Prev) -- move cursor forward
    , (xK_f, moveCursor Next) -- move cursor backward
    , (xK_BackSpace, killWord' p Prev) -- kill the previous word
    , (xK_y, pasteString)
    , (xK_g, quit)
    , (xK_bracketleft, quit)
    , (xK_h, deleteString Prev)
    , (xK_j, setSuccess True >> setDone True)
    ] ++
  map
    (first $ (,) mod1Mask)
    [ (xK_BackSpace, killWord' p Prev)
    , (xK_f, moveWord' p Next) -- move a word forward
    , (xK_b, moveWord' p Prev) -- move a word backward
    , (xK_d, killWord' p Next) -- kill the next word
    , (xK_n, moveHistory W.focusUp')
    , (xK_p, moveHistory W.focusDown')
    ] ++
  map
    (first $ (,) 0)
    [ (xK_Return, setSuccess True >> setDone True)
    , (xK_KP_Enter, setSuccess True >> setDone True)
    , (xK_Delete, deleteString Next)
    , (xK_Left, moveCursor Prev)
    , (xK_Right, moveCursor Next)
    , (xK_Home, startOfLine)
    , (xK_End, endOfLine)
    , (xK_Down, moveHistory W.focusUp')
    , (xK_Up, moveHistory W.focusDown')
    , (xK_Escape, quit)
    , (xK_BackSpace, deleteString Prev)
    ]

searchEngineMap ::
     forall a b. (Ord a, Num a)
  => (SearchEngine -> b)
  -> M.Map (a, KeySym) b
searchEngineMap method =
  M.fromList $
  [ ((0, xK_g), method google)
  , ((0, xK_h), method stackage)
  , ((0, xK_w), method wikipedia)
  , ((0, xK_v), method vocabulary)
  ]

sibiScratchPads :: [NamedScratchpad]
sibiScratchPads =
  [NS "keepass" "keepassxc" (className =? "keepassxc") nonFloating]
--
--
-- Use xprop for finding properties
-- Reference:
--  * https://wiki.haskell.org/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
