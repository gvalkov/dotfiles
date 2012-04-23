-- Imports

import XMonad
import System.IO
import System.Exit

import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.NamedScratchpad
import qualified XMonad.Util.Paste as Paste

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import Data.Monoid
import Data.List
import Data.Ratio ((%))

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.Warp
import XMonad.Actions.Search
import XMonad.Actions.TopicSpace
import XMonad.Actions.FloatSnap 
import XMonad.Actions.WindowBringer
import XMonad.Actions.Submap
import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.MouseGestures

-- Prompts
import XMonad.Prompt                
import XMonad.Prompt.Man           
import XMonad.Prompt.Ssh
import XMonad.Prompt.Input
import XMonad.Prompt.Workspace
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.AppendFile

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.IndependentScreens
import XMonad.Layout.PerWorkspace
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat
import XMonad.Layout.FixedColumn
import XMonad.Layout.IM
import XMonad.Layout.TwoPane
import XMonad.Layout.Grid

import XMonad.Util.EZConfig (mkKeymap)



-- Utility functions
join :: [a] -> [[a]] -> [a]
join delim l = concat $ intersperse delim l

spawnOn :: String -> String -> X ()
spawnOn workspace program = do
                            spawn program     
                            windows $ W.greedyView workspace


scratchpads :: [NamedScratchpad]
scratchpads = [
      NS "konsole" 
       "konsole -p tabtitle=konsole-scratchpad"
       (title =? "konsole-scratchpad") 
       (customFloating $ W.RationalRect (1/4) 0 (1/2) 1)

    , NS "ipython"
       "konsole -p tabtitle=ipython-konsole -e ipython"
       (title =? "ipython-konsole")
       (customFloating $ W.RationalRect (1/4) 0 (1/2) 1)

    , NS "notes"
        "gvim --role vimnotes -c ':Note index'"
        (role =? "vimnotes")
        (customFloating $ W.RationalRect (1/4) 0 (1/2) 1)
    ] where role = stringProperty "WM_WINDOW_ROLE"


topics :: [Topic]
topics =
  [ "-"
  , "web"
  , "dev"
  , "skype"
  , "irc"
  , "media"
  , "wip"
  , "vm"
  ]

myTopicConfig = TopicConfig
    { topicDirs = M.fromList $
        [ (" -", "./"      ) 
        , ("xmonad"   , ".xmonad" )
        , ("dev"      , "./source"  )
        , ("music"    , "./music" )
        , ("wip"      , "wip"     )
        ]
    , defaultTopic = "dashboard"
    , maxTopicHistory = 10
    , topicActions = M.fromList $
        [ ("media"   , spawn "clementine" )
        , ("web"     , spawn "firefox")
        , ("re-logs" , spawnOn "re-logs" "urxvt --title logs -tr -tint white -sh 35 -e multimon.sh")
        , ("re-web"  , spawnOn "re-web" "urxvt --title logs -tr -tint white -sh 35 -e multimon.sh")
        ]
    }

dmenu_run_cmd = [
     "dmenu-launch.py"
    ,"-nb", "'#3F3F3F'" -- normal background color
    ,"-nf", "'#DCDCCC'" -- normal foreground color
    ,"-sb", "'#7F9F7F'" -- selected background color
    ,"-sf", "'#DCDCCC'" -- selected foreground color
    ,"-b"               -- at the bottom of the screen
    ]


xpconfig = 
    XPC { font              = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        , bgColor           = "#333333"
        , fgColor           = "#FFFFFF"
        , fgHLight          = "#000000"
        , bgHLight          = "#BBBBBB"
        , borderColor       = red_color
        , promptBorderWidth = 1
        , position          = Bottom
        , height            = 18
        , historySize       = 256
        , historyFilter     = id
        , defaultText       = []
        , autoComplete      = Nothing
        , showCompletionOnTab = False
        }


keymap = \conf -> mkKeymap conf $
    [( "M-a" ,    spawn $ XMonad.terminal conf)
    ,( "M-r" ,    spawn $ join " " dmenu_run_cmd)
    ,( "M-f" ,    spawn "firefox")
    ,( "M-g" ,    spawn "gvim")
    ,( "M-e" ,    spawn "dolphin")
    ,( "C-A-l" ,  spawn "firefox")

    -- actions
    ,( "M-c" ,    kill                             ) 
    ,( "M-n" ,    refresh                          ) 
    ,( "M-z" ,    warpToWindow (1/2) (1/2)         ) 
    ,( "M-x" ,    currentTopicAction myTopicConfig ) 
    ,( "M-S-g" ,  gotoMenu ) 

    -- focus ops
    ,( "M-<Tab>"   , windows W.focusDown   ) 

    ,( "M-b"       , rotUnfocusedUp)
    ,( "M-n"       , rotUnfocusedDown)

    ,( "M-i"       , rotUnfocusedUp)
    ,( "M-u"       , rotUnfocusedDown)
    ,( "M-C-i"     , rotFocusedUp)
    ,( "M-C-u"     , rotFocusedDown)

    ,( "M-j"        , windows W.focusDown   ) 
    ,( "M-k"        , windows W.focusUp     ) 
    ,( "M-m"        , windows W.focusMaster ) 
    ,( "M-<Return>" , windows W.swapMaster  )
    ,( "M-S-j"      , windows W.swapDown    )
    ,( "M-S-k"      , windows W.swapUp      )

    -- multimedia keys
    ,( "<XF86AudioLowerVolume>" , spawn "amixer -D pulse set Master 5%- unmute" )
    ,( "<XF86AudioRaiseVolume>" , spawn "amixer -D pulse set Master 5%+ unmute" )
    ,( "<XF86AudioMute>"        , spawn "amixer set Master toggle"              )

    -- scratchpads
    ,( "M-S-n", namedScratchpadAction scratchpads "notes")
    ,( "M-S-i", namedScratchpadAction scratchpads "ipython")
    ,( "M-S-a", namedScratchpadAction scratchpads "konsole")

    -- prompts
    ,( "M-p",   xmonadPrompt xpconfig     ) 
    ,( "M-s",   shellPrompt xpconfig      ) 
    ,( "M-C-p",	runOrRaisePrompt xpconfig ) 

    -- expand/shrink master area
    ,( "M-h"        , sendMessage Shrink ) 
    ,( "M-l"        , sendMessage Expand ) 

    -- push window back into tiling
    ,( "M-t"        , withFocused $ windows . W.sink )

    ,( "M-<Space>"  , sendMessage NextLayout             ) 
    ,( "M-S-<Space>", setLayout $ XMonad.layoutHook conf ) 

    -- increment/decrement the number of windows in the master area
    ,( "M-,"        , sendMessage (IncMasterN 1)    )
    ,( "M-."        , sendMessage (IncMasterN (-1)) )

    -- quit/restart xmonad
    ,( "M-C-S-q"     , io (exitWith ExitSuccess))
    ,( "M-q"         , spawn "xmonad --recompile && xmonad --restart")

    ,( "M-<Right>"   , nextWS)
    ,( "M-<Left>"    , prevWS)
    ,( "M-S-`"       , viewEmptyWorkspace)
    ,( "M-`"         , toggleWS)
    ,( "M-S-<Right>" , shiftToNext)
    ,( "M-S-<Left>"  , shiftToPrev)
    
    ]
    ++

    -- switch to workspace n - mod-[1..9]
    [ ("M-" ++ k, windows $ W.greedyView w) | (w, k) <- zip (workspaces conf) $ map show [1..9] ]
    ++

    -- move client to workspace n - mod-shift-[1..9]
    [ ("M-S-" ++ k, windows $ W.shift w) | (w, k) <- zip (workspaces conf) $ map show [1..9] ]
    ++

    -- switch to xinerama screen - mod-{w,e,r} -- (I really need R for run, though)
    [ ("M-" ++ k:""  , screenWorkspace s >>= flip whenJust (windows . W.view)) | (k, s) <- zip "w" [0..] ]
    ++

    -- switch client to xinerama screen - mod-shift-{w,e,r} 
    [ ("M-S-" ++ k:"" , screenWorkspace s >>= flip whenJust (windows . W.shift)) | (k, s) <- zip "w" [0..] ]


-- Mouse config
button6     =  6 :: Button
button7     =  7 :: Button
button8     =  8 :: Button
button9     =  9 :: Button

mouse_keymap (XConfig {XMonad.modMask = modm}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> snapMagicMove (Just 25) (Just 25) w))

    -- shift-mod-button1, Set the window to floating mode and move by dragging
    , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w
                                                     >> snapMagicResize [L,R,U,D] (Just 25) (Just 25) w))

    -- mod-button2, Raise the window to the top of the stack
    {- , ((modm, button2), (\w -> focus w >> windows W.shiftMaster)) -}

    , ((modm, button2), mouseGesture gestures)
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    , ((0, button9),          (\w -> Paste.sendKey altMask xK_Right))
    , ((0, button8),          (\w -> Paste.sendKey altMask xK_Left))
    , ((altMask, button9),    (\w -> Paste.sendKey altMask xK_Up))
    ] 
    where
    gestures = M.fromList 
        [ 
          ([R], \_ -> spawn "qdbus org.mpris.MediaPlayer2.clementine /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
        , ([L], \_ -> spawn "qdbus org.mpris.MediaPlayer2.clementine /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
        ]


-- Layout config
layout_hook = smartBorders $ 
              onWorkspace "skype"  (avoidStruts (withIM (0.15) skypeRoster (Grid))) $
              avoidStruts $ standard_layouts
    where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
 
    -- The default number of windows in the master pane
    nmaster = 1
 
    -- Default proportion of screen occupied by master pane
    ratio   = 1/2
 
    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

    standard_layouts = smartBorders (tiled ||| Mirror tiled ||| Full)
    skypeRoster = (ClassName "Skype") `And` (Not (Title "Options")) `And` (Not (Role "Chats")) `And` (Not (Role "CallWindowForm"))
 

-- Window rules:
manage_hook = namedScratchpadManageHook scratchpads <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Vlc"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "gimp"           --> doFloat
    , className =? "Skype"          --> doShift "skype"
    , className =? "Pidgin"         --> doShift "skype"
    , className =? "stalonetray"    --> doShift "dashboard"
    , className =? "Tilda"          --> doFloat
    , className =? "stalonetray"    --> doIgnore
    , className =? "VirtualBox"     --> doShift "vm"
    , resource  =? "desktop_window" --> doIgnore
    , className =? "plasma-desktop" --> doFloat
    , className =? "Plasma-desktop" --> doFloat
    ]

 
-- Event config
event_hook = mempty

log_hook = do ewmhDesktopsLogHook
              return () 
 
-- Startup hook
startup_hook = return ()
 

term = "konsole"
red_color = "#d46464"
altMask = mod1Mask

defaults = defaultConfig {
          modMask  = mod4Mask
        , terminal = term
        , borderWidth = 1
        , workspaces = topics
        , focusFollowsMouse = True

        --- key bindings
        , keys = keymap
        , mouseBindings = mouse_keymap

        -- colors
        ,normalBorderColor = "#99968b"
        ,focusedBorderColor = red_color 

        --- hooks
        , manageHook = manage_hook <+> manageDocks
        , layoutHook = layout_hook
        , handleEventHook = event_hook
        , logHook = log_hook
        , startupHook = startup_hook
        } 


main = do
    {-checkTopicConfig topics topic_config-}
    xmonad $ ewmh defaults
