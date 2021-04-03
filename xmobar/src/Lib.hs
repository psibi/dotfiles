module Lib
  ( sibiXmobarMain
  ) where

import Data.List (foldl1')
import Network.HostName (getHostName)
import Xmobar

data Machine
  = Desktop
  | Laptop
  deriving (Eq)

guessMachine :: IO Machine
guessMachine = do
  hostname <- getHostName
  case hostname of
    "elric" -> pure Laptop
    _ -> pure Desktop

-- | The configuration.
config :: Machine -> Config
config machine =
  Config
    -- Appearance.
    { font = mainFont
    , bgColor = "black"
    , fgColor = colorFg
    , alpha = 255 -- Opacity.
    , additionalFonts -- Additional fonts to be specified.
       = [iconFont, altIconFont]
    -- Commands to be shown.
    , commands = (Run UnsafeXMonadLog): (map (fst . unRunnable) $ myCommands machine)
    -- General behaviour.
    , overrideRedirect = False -- Set the Override Redirect flag (Xlib).
    , lowerOnStart = False -- Send to bottom of window stack on start.
    , hideOnStart = False -- Don't hide the window on initialization.
    , persistent = False -- Bar is hideable.
    -- Bar position.
    , position = Top -- Where the bar gets drawn.
    , allDesktops = True -- Show on all desktops.
    , pickBroadest = False -- Use first display by default.
    -- Border configuration.
    , border = NoBorder -- No border around the bar.
    , borderColor = "#bd93f9"
    , borderWidth = 1
    , textOffset = -1 -- Offset from top of window for text.
    , iconOffset = -1 -- Offset from top of window for icons.
    , textOffsets = [11] -- Offsets for additional fonts.
    -- X names.
    , wmClass = "xmobar"
    , wmName = "xmobar"
    -- Layout.
    , sepChar = "%" -- Separation between plugin names and straight text.
    , alignSep = "}{" -- Separator between left/right alignment.
    -- How everything will look in the end.
    , template = xmobarTemplate machine
    -- Misc.
    , verbose = False -- This has no effect with a Haskell-based xmobar config.
    , iconRoot = "." -- Root folder for icons.
    }

inSquareBrackets =
  wrap (xmobarColor "#808080" "" "[") (xmobarColor "#808080" "" "]")

xmobarTemplate :: Machine -> String
xmobarTemplate machine =
  "%UnsafeXMonadLog% }{" ++
  concatMap
    inSquareBrackets (map (snd . unRunnable) $ myCommands machine)

{- Battery.
   Charge strings go *in front* of the AC off string, while AC on and idle
   strings ignore charge strings.
-}
type Template = String

newtype XMobarRunnable =
  XMobarRunnable
    { unRunnable :: (Runnable, Template)
    }

mkRunnable :: Runnable -> Template -> XMobarRunnable
mkRunnable run template = XMobarRunnable $ (run, template)

batteryCommand :: XMobarRunnable
batteryCommand = mkRunnable cmd "%battery%"
  where
    cmd =
      Run $
      Battery
        [ "--template"
        , "<acstatus>"
        , "--Low"
        , "15" -- Low  threshold for colours (in %)
        , "--High"
        , "70" -- High threshold for colours (in %)
        , "--low"
        , colorRed
        , "--normal"
        , colorFg
        , "--high"
        , colorGreen
        , "--suffix"
        , "True" -- Display '%' after '<left>'.
                                      -- battery specific options start here.
        , "--"
        , "--lowt"
        , "15" -- Low  threshold for charge strings (in %).
        , "--hight"
        , "70" -- High threshold for charge strings (in %).
        , "--lows"
        , inIconFont "\62020  "
        , "--mediums"
        , inIconFont "\62018  "
        , "--highs"
        , inIconFont "\62016  "
        , "--off"
        , "<left> (<timeleft>)" -- AC off.
        , "--on"
        , yellow "Charging" ++ ": <left> (<timeleft>)" -- AC on.
        , "--idle"
        , green "Charged" ++ " <left>" -- Fully charged.
        ]
        (seconds 10)

stationID :: String
stationID = "VOBL"

wirelessId :: Machine -> String
wirelessId Desktop = "wlp0s20f3"
wirelessId Laptop = "wlp2s0"

machineCommands :: Machine -> [XMobarRunnable]
machineCommands Desktop = [wirelessCommand Desktop]
machineCommands Laptop = [wirelessCommand Laptop, batteryCommand]

weatherCommand :: XMobarRunnable
weatherCommand =
  mkRunnable
    (Run $
     WeatherX
       stationID
       [ ("", inAltIconFont "🌑")
       , ("clear", inAltIconFont "🌣")
       , ("sunny", inAltIconFont "🌣")
       , ("mostly clear", inAltIconFont "🌤")
       , ("mostly sunny", inAltIconFont "🌤")
       , ("partly sunny", inAltIconFont "⛅")
       , ("fair", inAltIconFont "🌑")
       , ("cloudy", inAltIconFont "☁")
       , ("overcast", inAltIconFont "☁")
       , ("partly cloudy", inAltIconFont "⛅")
       , ("mostly cloudy", inAltIconFont "🌧")
       , ("considerable cloudines", inAltIconFont "☔")
       ]
       [ "--template"
       , foldl1'
           (\a b -> wrap a b (magenta " : "))
           ["<weather>", "<skyConditionS>", "<tempC>°C"]
        -- Weather specific options.
       , "--"
       , "--weathers"
       , "BLR" -- Display this when <weather> is empty.
       ]
       (minutes 30))
    ("%" ++ stationID ++ "%")

dateCommand :: XMobarRunnable
dateCommand =
  mkRunnable
    (Run $ Date ("%a %b %d-%m-%Y " ++ cyan "%l:%M") "date" (seconds 60))
    ("%date%")

-- Volume, with an event based refresh (via alsactl).
-- 08dec2019 +slot+ event based
volumeCommand :: XMobarRunnable
volumeCommand =
  mkRunnable
    (Run $
     Alsa
       "default"
       "Master"
       [ "--template"
       , "<volumestatus>"
       , "--suffix"
       , "True" -- Show "%" at the end of the <volume> string.
        -- Volume specific options.
       , "--"
       , "--on"
       , ""
       , "--off"
       , inAltIconFont "🔇"
       , "--lowv"
       , "20" -- Low  threshold for strings (in %).
       , "--highv"
       , "60" -- High threshold for strings (in %).
       , "--lows"
       , inIconFont "\61478  " -- Low    charge string: 
       , "--mediums"
       , inIconFont "\61479  " -- Medium charge string: 
       , "--highs"
       , inIconFont "\61480  " -- High   charge string: 
       , "--onc"
       , colorFg -- On  colour.
       , "--offc"
       , colorFg -- Off colour.
       ])
    ("%alsa:default:Master%")

-- Disk usage for root.
diskCommand :: XMobarRunnable
diskCommand =
  mkRunnable
    (Run $ DiskU [("/", "ROOT: <used>/<size>")] [] (minutes 30))
    ("%disku%")

cpuCommand :: XMobarRunnable
cpuCommand =
  mkRunnable
    (Run $
     Cpu
       ["-L", "3", "-H", "50", "--normal", "green", "--high", "red"]
       (seconds 20))
    ("%cpu%")

-- | Commands that I want displayed in my bar.
allCommands :: Machine -> [XMobarRunnable]
allCommands machine =
  cpuCommand :
  (machineCommands machine <>
   [weatherCommand, volumeCommand, diskCommand, dateCommand])

myCommands :: Machine -> [XMobarRunnable]
myCommands machine =
  cpuCommand: (machineCommands machine <>
   [weatherCommand, volumeCommand, diskCommand, dateCommand])

-- | Convenience functions
seconds, minutes :: Int -> Int
seconds t = t * 10

minutes t = 60 * seconds t

-- Name of wireless network currently connected to.
wirelessCommand :: Machine -> XMobarRunnable
wirelessCommand machine =
  mkRunnable
    (Run $
     Wireless
       (wirelessId machine)
       [ "--template"
       , "<essid> <quality>"
       , "--suffix"
       , "True" -- Display '%' after '<quality>'.
       , "--Low"
       , "40"
       , "--High"
       , "70"
       , "--low"
       , colorRed
       , "--normal"
       , colorYellow
       , "--high"
       , colorGreen
       ]
       (minutes 10))
    ("%" <> (wirelessId machine) <> "wi%")

--------------------------------------------------------------------------------
-- COLOURS
-- Unless stated otherwise, all colours are from the "Dracula" color scheme.
--------------------------------------------------------------------------------
colorZenburnBgPlus2 :: String
colorZenburnBgPlus2 = "#5f5f5f"

colorBg, colorCyan, colorFg, colorGreen, colorMagenta, colorRed, colorYellow ::
     String
colorRed = "#ff5555"

colorFg = "#f8f8f2"

colorBg = "#282a36"

colorGreen = "#50fa7b"

colorYellow = "#f1fa8c"

colorCyan = "#8be9fd"

colorMagenta = "#ff79c6"

cyan, green, magenta, yellow :: String -> String
green = xmobarColor colorGreen ""

yellow = xmobarColor colorYellow ""

cyan = xmobarColor colorCyan ""

magenta = xmobarColor colorMagenta ""

{- | Use xmobar escape codes to output a string with given foreground and
   background colors.
   Source: https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/src/XMonad.Hooks.DynamicLog.html#xmobarColor
-}
xmobarColor ::
     String -- ^ foreground color: a color name, or #rrggbb format
  -> String -- ^ background color
  -> String -- ^ output string
  -> String
xmobarColor fg bg = wrap t "</fc>"
  where
    t =
      concat
        [ "<fc="
        , fg
        , if null bg
            then ""
            else "," ++ bg
        , ">"
        ]

-- | Wrap a string in delimiters, unless it is empty.
-- Source: https://hackage.haskell.org/package/xmonad-contrib-0.15/docs/src/XMonad.Hooks.DynamicLog.html#wrap
wrap ::
     String -- ^ left delimiter
  -> String -- ^ right delimiter
  -> String -- ^ output string
  -> String
wrap _ _ "" = ""
wrap l r m = l ++ m ++ r

--------------------------------------------------------------------------------
-- FONTS
--------------------------------------------------------------------------------
mainFont :: String
mainFont = "xft:Ubuntu Mono:size=14:bold:antialias=true"

iconFont :: String
iconFont = "xft:FontAwesome-9"

altIconFont :: String
altIconFont = "xft:Symbola-9"

-- | Wrap stuff so it uses the icon font.
inIconFont :: String -> String
inIconFont = wrap "<fn=1>" "</fn>"

-- | Wrap stuff so it uses the alt icon font.
inAltIconFont :: String -> String
inAltIconFont = wrap "<fn=2>" "</fn>"

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
-- | Execute xmobar with the config.
sibiXmobarMain :: IO ()
sibiXmobarMain = do
  machine <- guessMachine
  xmobar $ config machine
