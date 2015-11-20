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
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.Search
import qualified Data.Map as M
import Control.Arrow (first)
import Data.Char (isSpace)

myWorkspaces = ["main", "web", "chat", "dev", "media", "float", "misc"]

myTerminal = "urxvt -e screen"

-- Notes
-- Mod Key + Q refreshes XMonad session

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
  xmproc <- spawnPipe "xmobar"
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
    , ((controlMask, xK_Print), spawn "xfce4-screenshooter")
    , ((mod4Mask, xK_x), spawn "xkill")
    , ((mod4Mask, xK_c), kill)
    , ((0, xK_Print), spawn "scrot")
    , ((mod4Mask, xK_o), shellPrompt $ defaultXPConfig {promptKeymap = sibiEmacsKeymap})
    , ((mod4Mask, xK_u), promptSearch (greenXPConfig {promptKeymap = emacsLikeXPKeymap}) multiEngine)
    , ((mod4Mask, xK_g), spawn "unity-control-center")
    , ((mod4Mask, xK_p), spawn "dmenu_run")
    ]

sibiEmacsKeymap = sibiEmacsKeymap' isSpace

-- Modified from source to suit my key binding
sibiEmacsKeymap' :: (Char -> Bool) -> M.Map (KeyMask,KeySym) (XP ())
sibiEmacsKeymap' p = M.fromList $
  map (first $ (,) controlMask) -- control + <key>
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
  ] ++
  map (first $ (,) mod1Mask) -- meta key + <key>
  [ (xK_BackSpace, killWord' p Prev)
  , (xK_f, moveWord' p Next) -- move a word forward
  , (xK_b, moveWord' p Prev) -- move a word backward
  , (xK_d, killWord' p Next) -- kill the next word
  , (xK_n, moveHistory W.focusUp')
  , (xK_p, moveHistory W.focusDown')
  ]
  ++
  map (first $ (,) 0) -- <key>
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
  ]

multiEngine :: SearchEngine
multiEngine = intelligent (wikipedia !> hackage !> (prefixAware google))
-- main = do
--   xmonad $ defaultConfig
-- Reference
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#I_need_to_find_the_class_title_or_some_other_X_property_of_my_program
