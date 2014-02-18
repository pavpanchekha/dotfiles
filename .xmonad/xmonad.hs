-- -*- mode: haskell -*-

import XMonad
import XMonad.Core
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks
import System.Directory
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
           , position = Top
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

nonScratchpadWS =
                do cur <- (W.tag . W.workspace . W.current) `fmap` gets windowset
                   return $ (/= "NSP") . W.tag

myApps = do home <- getHomeDirectory
            readFile (home ++ "/.config/apps") >>= return . lines
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
    , ((modm .|. shiftMask, xK_f     ), spawn "firefox")
    , ((modm .|. shiftMask, xK_t     ), spawn "thunar")
    , ((modm .|. shiftMask, xK_n     ), spawn "notepaster")
    , ((modm .|. shiftMask, xK_space ), execPrompt)
    
    -- Cycle workspaces
    , ((modm,               xK_Right),  moveTo Next $ WSIs nonScratchpadWS)
    , ((modm,               xK_Left),   moveTo Prev $ WSIs nonScratchpadWS)
    , ((modm .|. shiftMask, xK_Right),  shiftToNext)
    , ((modm .|. shiftMask, xK_Left),   shiftToPrev)

    -- Volume up and down
    , ((0, 0x1008FF11), spawn "amixer set Master 5dB-")
    , ((0, 0x1008FF13), spawn "amixer set Master 5dB+")
    , ((0, 0x1008FFB2), spawn "amixer set Capture toggle")
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

myLog pipe = dynamicLogWithPP $ xmobarPP {
      ppOutput  = hPutStrLn pipe
    , ppCurrent = xmobarColor "black" "#dddddd" . pad
    , ppVisible = xmobarColor "black" "#aaaaaa" . pad
    , ppHidden  = xmobarColor "white" "black" . (\tag ->
                              case tag of
                                   "NSP"         -> ""
                                   "1"           -> " 1" -- Left edge padding
                                   _             -> tag
                              )
    , ppLayout  = xmobarColor "white" "black" . (\layout ->
                              case layout of
                                   "Tall"        -> "Vt"
                                   "Mirror Tall" -> "Hz"
                                   "Full"        -> "Fl"
                                   _             -> layout
                              )
    , ppTitle   = xmobarColor "white" "black" -- . xmobarEscape
    }
    
fontTerminus = "-*-terminus-medium-*-*-*-*-140-*-*-*-*-*-*"

main = do
    statusbar <- spawnPipe ("xmobar ~/.xmonad/xmobar.hs")
    
    xmonad $ defaultConfig {
      -- simple stuff
        terminal           = "sakura",
        focusFollowsMouse  = True,
        borderWidth        = 1,
        modMask            = mod4Mask,
        workspaces         = ["1", "2", "3", "4", "5", "S"],
        
        normalBorderColor  = "#000000",
        focusedBorderColor = "#8ae234",
 
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        manageHook = composeAll myManageHook,
        layoutHook = myLayout,
        logHook = myLog statusbar
    }
