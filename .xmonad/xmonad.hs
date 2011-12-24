-- -*- mode: haskell -*-

import XMonad
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import System.Exit
import System.IO
 
import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)

myPrompt = defaultXPConfig {
             bgColor = "black"
           , fgColor = "white"
           , font = "xft:Terminus-12"
           , height = 24
           , promptBorderWidth = 0
           }

myScratchPads = [NS "terminal" spawnTerm findTerm manageTerm] where
  spawnTerm  = "exec sakura --name scratchpad"
  findTerm   = resource =? "scratchpad"
  manageTerm = customFloating $ W.RationalRect l t w h where
    h = 0.5   -- Terminal height
    w = 1     -- Width
    t = 1 - h -- Distance from top
    l = 1 - w -- Distance from left

myApps = readFile "/home/pavpanchekha/.config/apps" >>= return . lines
myCompletion s = myApps >>= flip mkComplFunFromList s

execPrompt = inputPromptWithCompl myPrompt "$" myCompletion ?+ \c-> spawn ("exec " ++ c)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [ ((modm .|. shiftMask, xK_x     ), spawn $ XMonad.terminal conf)

    , ((modm,               xK_q     ), kill)
    , ((modm,               xK_r     ), sendMessage NextLayout)
    , ((modm,               xK_b     ), sendMessage ToggleStruts)
    
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp)
    , ((modm,               xK_j     ), windows W.focusDown)
    , ((modm,               xK_k     ), windows W.focusUp  )

    , ((modm,               xK_m     ), windows W.focusMaster  )
    , ((modm,               xK_Return), windows W.swapMaster)
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    , ((modm,               xK_h     ), sendMessage Shrink)
    , ((modm,               xK_l     ), sendMessage Expand)
    
    -- Number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))
    
    -- Push window back into tiling
    , ((modm,               xK_d     ), withFocused $ windows . W.sink)

    , ((modm              , xK_x     ), namedScratchpadAction myScratchPads "terminal")
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm              , xK_space ), spawn "emacsclient -c --alternate-editor=")
    , ((modm .|. shiftMask, xK_space ), execPrompt)
    
    -- Cycle workspaces
    , ((modm,               xK_Right),  nextWS)
    , ((modm,               xK_Left),   prevWS)
    , ((modm .|. shiftMask, xK_Right),  shiftToNext)
    , ((modm .|. shiftMask, xK_Left),   shiftToPrev)
    ]
    ++ -- Switch to workspace
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

myManageHook = [ className =? "stalonetray"--> doIgnore
               , className =? "plugin-container" --> doIgnore
               , namedScratchpadManageHook myScratchPads]

myLayout = smartBorders $ avoidStruts $ (tiled ||| Mirror tiled ||| Full)
    where tiled = Tall 1 (3/100) (1/2)

myLog pipe = dynamicLogWithPP $ dzenPP {
      ppOutput  = hPutStrLn pipe
    , ppCurrent = dzenColor "black" "#dddddd" . pad
    , ppVisible = dzenColor "black" "#aaaaaa"  . pad
    , ppHidden  = dzenColor "white" "black" . pad . (\tag ->
                            case tag of
                                 "NSP"         -> ""
                                 _             -> tag
                            )
    , ppLayout  = dzenColor "white" "black" . pad . (\layout ->
                            case layout of
                                 "Tall"        -> "|-"
                                 "Mirror Tall" -> "--"
                                 "Full"        -> "[]"
                                 _             -> layout
                            )
    , ppTitle   = dzenColor "white" "black" . dzenEscape
    }
    
fontTerminus = "-*-terminus-medium-*-*-*-*-140-*-*-*-*-*-*"

main = do
    statusbar <- spawnPipe ("dzen2 -fg white -bg black -fn " ++ fontTerminus ++ " -ta l")
    
    xmonad $ defaultConfig {
      -- simple stuff
        terminal           = "sakura",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = ["1", "2", "3", "4", "5", "S"],
        
        normalBorderColor  = "#888888",
        focusedBorderColor = "#dddddd",
 
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        manageHook = composeAll myManageHook,
        layoutHook = myLayout,
        logHook = myLog statusbar
    }
