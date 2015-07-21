import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import System.IO
import Control.Monad
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageHelpers

myWorkspaces = ["main", "web", "chat", "dev", "media", "float", "misc"]

myTerminal = "urxvt -e screen"

-- Define the workspace an application has to go to
myManageHook = composeAll . concat $
    [
      [isFullscreen --> doFullFloat] -- For Media Players
          -- Applications that go to web
    , [ className =? b --> viewShift "web"      | b <- myClassWebShifts  ]
         -- Applications that go to chat
    , [ resource  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
    , [ resource  =? c --> doF (W.shift "dev") | c <- myClassDevShifts ]
    , [ resource  =? c --> doF (W.shift "media") | c <- myClassMediaShifts ]
    , [ resource  =? c --> doF (W.shift "float") | c <- myClassFloatShifts ]
    , [ resource  =? c --> doF (W.shift "misc") | c <- myClassMiscShifts ]
    , [ resource  =? c --> doF (W.shift "main") | c <- myClassMainShifts ]      
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts  = ["Firefox"]
        myClassChatShifts = ["Pidgin", "eboard" ]
        myClassDevShifts = ["emacs"]
        myClassMediaShifts = ["rhythmbox"]
        myClassFloatShifts = ["gimp", "SMPlayer", "smplayer"]
        myClassMiscShifts = ["nautilus"]
        myClassMainShifts = [".urxvt-wrapped"]

main = do

  xmproc <- spawnPipe "/home/sibi/.nix-profile/bin/xmobar /home/sibi/.xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
    {
      manageHook = manageDocks <+> myManageHook
                   <+> manageHook defaultConfig
    -- No red border for media players
    , terminal = myTerminal
    , layoutHook = smartBorders $ avoidStruts $ layoutHook defaultConfig 
    , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppUrgent = xmobarColor "yellow" "red" . xmobarStrip
                        }
    , modMask = mod4Mask     -- Rebind Mod to the Windows key
    , workspaces = myWorkspaces
    } `additionalKeys`
    [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
    , ((mod4Mask, xK_x), spawn "xkill")
    , ((mod4Mask, xK_c), kill)
    , ((0, xK_Print), spawn "scrot")
    , ((mod4Mask, xK_g), spawn "gnome-control-center")
    , ((mod4Mask, xK_p), spawn "dmenu_run")
    ]

-- main = do
--   xmonad $ defaultConfig
-- Reference
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
