import XMonad.Layout.FocusTracking
import XMonad.StackSet qualified as W
import Data.Map qualified as M
import XMonad.Util.EZConfig ( mkKeymap )
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog -- ( xmobar, def, ppOutput, ppTitle ) 
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Prompt
import XMonad.Prompt.Shell
import Prelude
import Control.Monad
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Util.Themes
import XMonad.Actions.CycleWS as CWS
import XMonad qualified as X
import XMonad.Layout.Tabbed
import qualified Graphics.X11.Xlib.Extras as XL (killClient)
import Data.Ratio ((%))
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.ClickableWorkspaces

main :: IO ()
main = do 
    X.xmonad . ewmh . withEasySB (statusBarProp "xmobar" (clickablePP myXmobarPP)) toggleStrutsKey $ myConfig
    where 
        toggleStrutsKey :: X.XConfig X.Layout -> (X.KeyMask, X.KeySym)
        toggleStrutsKey X.XConfig{ X.modMask = m } = (m, X.xK_Tab)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = gray " | "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = blue . wrap " " "" . xmobarBorder "Bottom" "#6881b5" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = gray . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _] -> [ws, l]
    , ppExtras          = []
    }
  where
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    gray, blue, red, white, yellow :: String -> String
    gray     = xmobarColor "#636363" ""
    blue     = xmobarColor "#6881b5" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""

myTabTheme = def {
            activeColor         = "#000000"  
          , activeBorderColor   = "#6881b5"  
          , inactiveColor       = "#000000"  
          , inactiveBorderColor = "#636363"

          , activeTextColor     = "#6881b5"  
          , inactiveTextColor   = "#636363"  
          , urgentTextColor     = "#FF0000"
          , fontName            = "xft:Iosevka Fixed SS14:size=13"
          , decoHeight          = 26
          , windowTitleAddons   = []
          , windowTitleIcons    = []
 }

myPromptConfig :: XPConfig
myPromptConfig = def {
          font                = "xft:Iosevka Fixed SS14:size=18"
        , bgColor             = "#000000"
        , fgColor             = "#FFFFFF"
        , fgHLight            = "#f85cab"
        , bgHLight            = "#202020"
        , borderColor         = "#f85cab"
        , promptBorderWidth   = 2
        , position            = CenteredAt (1 % 2) (2 % 3)
        , height              = 37
        , historySize         = 256
        , historyFilter       = id
        , defaultText         = []
        , showCompletionOnTab = False
        , alwaysHighlight     = True
        , maxComplRows        = Just 8
}

myCopyPromptConfig :: XPConfig
myCopyPromptConfig = myPromptConfig {
          borderColor = "#5cf8aa"
        , fgHLight    = "#5cf8aa"
}

forceKillWindow :: X.Window -> X.X ()
forceKillWindow w = X.withDisplay $ \dpy -> X.io (void $ XL.killClient dpy w) 

data CustomShell = CustomShell
instance XPrompt CustomShell where
    showXPrompt CustomShell = "please stupid shell (run) => "

customShellPrompt :: XPConfig -> X.X ()
customShellPrompt c = do
    cmds <- X.io getCommands
    mkXPrompt CustomShell c (getShellCompl cmds (searchPredicate c)) (\s -> safeSpawn "/bin/env" ["-S", "bash", "-c", s])

data CopyShell = CopyShell
instance XPrompt CopyShell where
    showXPrompt CopyShell = "please stupid shell (copy) => " 

copyShellPrompt :: XPConfig -> X.X ()
copyShellPrompt c = do
    cmds <- X.io getCommands
    mkXPrompt CopyShell c (getShellCompl cmds (searchPredicate c)) (\s -> X.spawn $ "/bin/env -S bash " ++ s ++ " | xargs -I {} /usr/bin/copyq copy {}")   

myManageHook = X.composeAll
    [ isDialog X.--> (doFocus >> doCenterFloat)
    , X.className X.=? "unityhub" X.--> doCenterFloat
    , X.className X.=? "pavucontrol" X.--> doCenterFloat
    , X.className X.=? "copyq" X.--> (doFocus >> doCenterFloat)
    , X.className X.=? "mpv" X.--> (doFocus >> doCenterFloat)
    , X.className X.=? "gcr-prompter" X.--> (doFocus >> doCenterFloat)
    , X.className X.=? "Gcr-prompter" X.--> (doFocus >> doCenterFloat)
    , X.title X.=? "Picture-in-Picture" X.--> (doFocus >> doCenterFloat)
    , X.title X.=? "Discord Updater" X.--> (doFocus >> doCenterFloat)
    , X.title X.=? "Settings" X.--> (doFocus >> doCenterFloat)
    , X.title X.=? "Friends List" X.--> (doFocus >> doCenterFloat)
    ]

myWorkspaces = map show [1 .. 9]
myConfig = def {
      X.terminal           = "kitty"
    , X.borderWidth        = 3
    , X.normalBorderColor  = "#000000"
    , X.focusedBorderColor = "#6881b5"
    , X.keys               = flip mkKeymap myKeys
    , X.modMask            = X.mod4Mask
    , X.focusFollowsMouse  = False
    , X.manageHook = myManageHook X.<+> X.manageHook def
    , X.startupHook        = X.spawn "stalonetray &" >> 
                             X.spawn "unclutter --timeout 1 --hide-on-touch --ignore-scrolling --fork &" >>
                             X.spawn "sleep 1 && setxkbmap us,ru,ua -variant colemak_dh_ortho,diktor,diktor -option grp:ctrls_toggle -option caps:capslock && redshift -x && redshift -O 4500 && xset dpms 0 0 0 && xset s noblank && xset s off" >> 
                             X.spawn "nitrogen --set-zoom-fill $HOME/dotfiles/bgs/ascii/16_10/plotnishek_tohru1610_ascii.png" >>
                             X.spawn "copyq &" >>
                             X.spawn "picom &" -- >>
                             -- X.spawn "flameshot &"
    , X.workspaces         = myWorkspaces
    , X.layoutHook         = smartBorders ( focusTracking $ tabbed shrinkText myTabTheme X.||| X.Full X.||| ResizableTall 1 (3/100) (1/2)[])
}

myKeys = [
    ("M-1", ((X.windows $ W.greedyView $ myWorkspaces !! 0)))
  , ("M-2", ((X.windows $ W.greedyView $ myWorkspaces !! 1)))
  , ("M-3", ((X.windows $ W.greedyView $ myWorkspaces !! 2)))
  , ("M-4", ((X.windows $ W.greedyView $ myWorkspaces !! 3)))
  , ("M-5", ((X.windows $ W.greedyView $ myWorkspaces !! 4)))
  , ("M-6", ((X.windows $ W.greedyView $ myWorkspaces !! 5)))
  , ("M-7", ((X.windows $ W.greedyView $ myWorkspaces !! 6)))
  , ("M-8", ((X.windows $ W.greedyView $ myWorkspaces !! 7)))
  , ("M-9", ((X.windows $ W.greedyView $ myWorkspaces !! 8)))
  , ("M-C-1", (X.windows $ W.shift $ myWorkspaces !! 0) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 0))
  , ("M-C-2", (X.windows $ W.shift $ myWorkspaces !! 1) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 1))
  , ("M-C-3", (X.windows $ W.shift $ myWorkspaces !! 2) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 2))
  , ("M-C-4", (X.windows $ W.shift $ myWorkspaces !! 3) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 3))
  , ("M-C-5", (X.windows $ W.shift $ myWorkspaces !! 4) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 4))
  , ("M-C-6", (X.windows $ W.shift $ myWorkspaces !! 4) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 5))
  , ("M-C-7", (X.windows $ W.shift $ myWorkspaces !! 4) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 6))
  , ("M-C-8", (X.windows $ W.shift $ myWorkspaces !! 4) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 7))
  , ("M-C-9", (X.windows $ W.shift $ myWorkspaces !! 4) X.<+> (X.windows $ W.greedyView $ myWorkspaces !! 8))
  , ("M-r",                    X.spawn "$HOME/dotfiles/run_dmenu.d --theme xmonad") 
  , ("M-m", customShellPrompt myPromptConfig)
  , ("M-C-m", copyShellPrompt myCopyPromptConfig)
  , ("M-t",                    X.spawn "kitty")
  , ("M-<Backspace>",          X.kill)
  , ("M-S-<Backspace>",        X.withFocused forceKillWindow)
  , ("M-S-q",                  X.spawn "pkill xmonad")
  , ("M-S-v",                  X.spawn "copyq toggle")
  , ("M-v",                    X.spawn "copyq toggle")
  , ("M-c",                    X.spawn "xcolor | xargs -I {} copyq copy {}")
  , ("M-p",                    X.spawn "maim -s | xclip -selection clipboard -t image/png")
  , ("M-S-p",                  X.spawn "maim | xclip -selection clipboard -t image/png")
  , ("<Print>",                X.spawn "maim -s | xclip -selection clipboard -t image/png")
  , ("S-<Print>",              X.spawn "maim | xclip -selection clipboard -t image/png")
  , ("<XF86AudioLowerVolume>", X.spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%")
  , ("<XF86AudioRaiseVolume>", X.spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%")
  , ("S-<XF86AudioLowerVolume>", X.spawn "brightnessctl set 1%-")
  , ("S-<XF86AudioRaiseVolume>", X.spawn "brightnessctl set +1%")
  , ("<XF86AudioMute>",        X.spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
  , ("M--",                    X.spawn "pactl set-sink-volume @DEFAULT_SINK@ -1%")
  , ("M-=",                    X.spawn "pactl set-sink-volume @DEFAULT_SINK@ +1%")
  , ("M-S--",                  X.spawn "brightnessctl set 1%-")
  , ("M-S-=",                  X.spawn "brightnessctl set +1%")
  , ("M-z",                    X.spawn "boomer")
  , ("M-S-n",                  X.windows W.swapUp)
  , ("M-S-i",                  X.windows W.swapDown)
  , ("M-S-.",                  X.windows W.focusDown)
  , ("M-S-,",                  X.windows W.focusUp)
  , ("M-C-.",                  CWS.shiftToNext >> CWS.nextWS)
  , ("M-C-,",                  CWS.shiftToPrev >> CWS.prevWS)
  , ("M-.",                    CWS.nextWS)
  , ("M-,",                    CWS.prevWS)
  , ("M-<Space>",              X.sendMessage X.NextLayout)
  , ("M-C-n",                  X.sendMessage X.Shrink)
  , ("M-C-i",                  X.sendMessage X.Expand)
  , ("M-f",                    X.withFocused toggleFloat)
  , ("M-S-r",                  X.spawn "pkill stalonetray" >> X.spawn restart_command) ]
  where 
        restart_command = "if type xmonad; then xmonad --recompile && xmonad --restart;" ++
          " else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
        toggleFloat w = X.windows (\s -> if M.member w (W.floating s)
                                   then W.sink w s
                                   else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
