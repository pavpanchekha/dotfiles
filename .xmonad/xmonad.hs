-- -*- mode: haskell -*-

import XMonad
import XMonad.Core
import XMonad.Util.NamedScratchpad
import XMonad.Layout.NoBorders
import XMonad.Layout.Column
import XMonad.Hooks.ManageDocks
import System.Directory
import System.Exit
import System.IO
import XMonad.Util.Run
import Data.Monoid
import Data.List

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
           , font = "xft:Deja Vu Sans Mono-14"
           , height = 28
           , position = Top
           , promptBorderWidth = 0
           }

myScratchPads = [NS "terminal" spawnTerm findTerm manageTerm] where
  --spawnTerm  = "exec emacsclient -c -a '' -e '(scratchpad)'"
  spawnTerm  = "sakura --class scratchpad"
  --findTerm   = title =? "scratchpad" <&&> className =? "Emacs"
  findTerm   = className =? "scratchpad"
  manageTerm = (customFloating $ W.RationalRect l t w h) where
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
    , ((modm .|. shiftMask, xK_q     ), withPID killPID)
    , ((modm,               xK_r     ), sendMessage NextLayout)
    , ((modm,               xK_b     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_Delete), io (exitWith ExitSuccess))

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
    , ((modm .|. shiftMask, xK_x     ), spawn "sakura")
    , ((modm .|. shiftMask, xK_r     ), spawn "xmonad --recompile; xmonad --restart")
    , ((modm              , xK_space ), spawn "emacsclient -c --alternate-editor=")
    , ((modm .|. shiftMask, xK_f     ), spawn "$BROWSER")
    , ((modm .|. shiftMask, xK_t     ), spawn "thunar")
    , ((modm .|. shiftMask, xK_space ), execPrompt)

    , ((modm,               xK_p     ), spawn "xrandr --output HDMI1 --mode 1920x1080")

    -- Cycle workspaces
    , ((modm,               xK_Right),  moveTo Next $ WSIs nonScratchpadWS)
    , ((modm,               xK_Left),   moveTo Prev $ WSIs nonScratchpadWS)
    , ((modm .|. shiftMask, xK_Right),  shiftToNext)
    , ((modm .|. shiftMask, xK_Left),   shiftToPrev)

    -- Volume up and down
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume 0 -10%")
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume 0 +10%")
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute 0 toggle")
    , ((shiftMask, 0x1008FF11), withPID (pactl "set-sink-input-volume" "-10%"))
    , ((shiftMask, 0x1008FF13), withPID (pactl "set-sink-input-volume" "+10%"))
    , ((shiftMask, 0x1008FF12), withPID (pactl "set-sink-input-mute" "toggle"))
    , ((0, 0x1008ff16), spawn "tomahawk --prev")
    , ((0, 0x1008ff14), spawn "tomahawk --playpause")
    , ((0, 0x1008ff17), spawn "tomahawk --next")
    ]
    ++ -- Switch to workspace
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

getPID :: Display -> Window -> X (Maybe Int)
getPID display window =
  do pid_str <- getAtom "_NET_WM_PID"
     xs <- io $ (fmap . fmap . fmap) fromIntegral $ getWindowProperty32 display pid_str window
     case xs of
      Just (x:_) -> return $ Just $ fromIntegral x
      Just [] -> return Nothing
      Nothing -> return Nothing

withPID :: (Int -> X ()) -> X ()
withPID f =
  withDisplay (\d-> withFocused (\w-> getPID' d w)) where
  getPID' d w =
    do pid' <- getPID d w
       case pid' of
         Nothing -> return ()
         Just pid -> f pid

getSinkInputNumber :: Int -> IO (Maybe Int)
getSinkInputNumber pid =
  do out <- runProcessWithInput "/usr/bin/pactl" ["list", "sink-inputs"] ""
     return $ parseSIDFromPID pid out

parseSIDFromPID :: Int -> String -> Maybe Int
parseSIDFromPID pid out = go pid (lines out) Nothing where
  _sinkinput = "Sink Input #"
  _app_pid = "\t\tapplication.process.id = \""
  go :: Int -> [String] -> Maybe Int -> Maybe Int
  go p (l : ls) Nothing
    | isPrefixOf _sinkinput l =
      go p ls $ Just $ read $ drop (length _sinkinput) l
    | otherwise = go p ls Nothing
  go p (l : ls) (Just sid)
    | l == (_app_pid ++ show pid ++ "\"") = return sid
    | l == "" = go p ls Nothing
    | otherwise = go p ls (Just sid)
  go p [] _ = Nothing

pactl :: String -> String -> Int -> X ()
pactl cmd delta pid =
  do sid' <- io $ getSinkInputNumber pid
     case sid' of
       Nothing -> return ()
       Just sid -> spawn $ "pactl " ++ cmd ++ " " ++ show sid ++ " " ++ delta

killPID :: Int -> X ()
killPID pid = spawn $ "kill -9 " ++ show pid

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
               , className =? "Pinentry" --> doFloat
               , namedScratchpadManageHook myScratchPads]

myLayout = smartBorders $ avoidStruts $ (tiled ||| Mirror (Column 1.0) ||| Full)
    where tiled = Tall 1 (3/100) (1/2)

myLog pipe = dynamicLogWithPP $ xmobarPP {
      ppOutput  = hPutStrLn pipe
    , ppCurrent = xmobarColor "#444444" "#cccccc" . pad
    , ppVisible = xmobarColor "#444444" "#bbbbbb" . pad
    , ppHidden  = xmobarColor "#444444" "#f6f5f4" . (\tag -> if tag == "NSP" then "" else tag)
    , ppLayout  = xmobarColor "#444444" "#f6f5f4" . (\layout ->
                              case layout of
                                   "Tall"        -> "Vt"
                                   "Mirror Tall" -> "Hz"
                                   "Full"        -> "Fl"
                                   _             -> layout
                              )
    , ppTitle   = xmobarColor "#444444" "#f6f5f4"
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
        workspaces         = ["1", "2", "3", "4", "5"],

        normalBorderColor  = "#000000",
        focusedBorderColor = "#8ae234",

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

        manageHook = composeAll myManageHook,
        layoutHook = myLayout,
        handleEventHook = (\_-> (return (All True))) <+> docksEventHook,
        logHook = myLog statusbar
    }
