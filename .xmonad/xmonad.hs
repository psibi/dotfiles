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

-- Define the workspace an application has to go to
myManageHook = composeAll . concat $
    [
      [isFullscreen --> doFullFloat] -- For Media Players
          -- Applications that go to web
    , [ className =? b --> viewShift "web"      | b <- myClassWebShifts  ]
         -- Applications that go to chat
    , [ resource  =? c --> doF (W.shift "chat") | c <- myClassChatShifts ]
    , [ resource  =? c --> doF (W.shift "dev") | c <- myClassDevShifts ]
    , [ resource  =? c --> doF (W.shift "float") | c <- myClassFloatShifts ]
    , [ resource  =? c --> doF (W.shift "misc") | c <- myClassMiscShifts ]
    ]
    where
        viewShift = doF . liftM2 (.) W.greedyView W.shift
        myClassWebShifts  = ["Firefox"]
        myClassChatShifts = ["Pidgin" ]
        myClassDevShifts = ["emacs"]
        myClassFloatShifts = ["gimp", "SMPlayer", "smplayer"]
        myClassMiscShifts = ["nautilus"]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/sibi/.xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook defaultConfig
    {
      manageHook = manageDocks <+> myManageHook
                   <+> manageHook defaultConfig
    -- No red border for media players
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
    ]

-- main = do
--   xmonad $ defaultConfig
