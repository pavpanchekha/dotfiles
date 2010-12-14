import XMonad
import XMonad.Config.Desktop (desktopLayoutModifiers)
import XMonad.Config.Gnome
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import Data.Monoid
import System.Exit
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Input

myPrompt = defaultXPConfig {
             bgColor = "black"
           , fgColor = "white"
           , font = "xft:Terminus-15"
           , height = 24
           , promptBorderWidth = 0
           }

myScratchPads = [NS "terminal" spawnTerm findTerm manageTerm] where
  spawnTerm  = "exec sakura --name scratchpad"
  findTerm   = resource =? "scratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h where
    h = 0.4 -- Terminal height
    w = 1 + 10/1024  -- Width
    t = 1 - h - 17 / 1024 -- Distance from top
    l = 1 - w + 8/1024-- Distance from left

myApps = ["gvim", "firefox", "terminal", "task", "event", "alarm", "msg", "reply", "gajim-remote toggle_roster_appearance", "zathura", "jumanji"]

execPrompt = inputPromptWithCompl myPrompt "$" (mkComplFunFromList myApps) ?+ \c-> spawn ("exec " ++ c)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_x), spawn $ XMonad.terminal conf)
    -- close focused window
    , ((modm, xK_q     ), kill)
     -- Rotate through the available layout algorithms
    , ((modm,               xK_r ), sendMessage NextLayout)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm,               xK_d     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    -- Scratchpad
    , ((modm              , xK_x     ), namedScratchpadAction myScratchPads "terminal")
    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm              , xK_space ), execPrompt)
    
    , ((modm,               xK_Right),  nextWS)
    , ((modm,               xK_Left),   prevWS)
    , ((modm .|. shiftMask, xK_Right),  shiftToNext)
    , ((modm .|. shiftMask, xK_Left),   shiftToPrev)
    , ((modm,               xK_Down),   nextScreen)
    , ((modm,               xK_Up),     prevScreen)
    , ((modm .|. shiftMask, xK_Down),   shiftNextScreen)
    , ((modm .|. shiftMask, xK_Up),     shiftPrevScreen)
    ]
    ++
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

myManageHook = [ resource  =? "Do"         --> doIgnore
               , className =? "Guake.py"   --> doFloat
               , className =? "stalonetray"--> doIgnore
               , namedScratchpadManageHook myScratchPads]
               
main = xmonad defaults
defaults = gnomeConfig {
      -- simple stuff
        terminal           = "sakura",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = ["1", "2", "3", "4"],
        
        normalBorderColor  = "#888888",
        focusedBorderColor = "#dddddd",
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        manageHook = manageHook gnomeConfig <+> composeAll myManageHook,
        layoutHook = layoutHook gnomeConfig
    }
