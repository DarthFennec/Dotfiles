{-# LANGUAGE DeriveDataTypeable #-}

import XMonad
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Hooks.EwmhDesktops hiding (fullscreenEventHook)
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import Data.Typeable
import Data.Monoid
import Data.List
import System.Exit
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet             as W
import qualified Data.Map                    as M

data SysMond = SysMond [Int] deriving Typeable

instance ExtensionClass SysMond where
  initialValue = SysMond []

main = spawnPipe dzen >>= xmonad.chwm.ewmh.cfg
  where dzen = "dzen2 -ta l -w 1820 -h 25 -e 'button7=exit:13'"

term = "roxterm"
termexec n = term ++ " -e " ++ n

cfg h = defaultConfig
  { terminal         = term
  , borderWidth      = 0
  , clickJustFocuses = False
  , modMask          = mod4Mask
  , workspaces       = map show [1..9] ++ ["0","~","NSP"]
  , keys             = myKeys
  , mouseBindings    = myMouse
  , layoutHook       = fullscreenFull.gaps [(U,25)] $ Tall 1 0.03 0.53 ||| Full
  , manageHook       = myManageHook
  , handleEventHook  = fullscreenEventHook <+> tickerHook <+> logoutHook
  , logHook          = dynamicLogWithPP $ pp h }

chwm c = c { startupHook = mappend (startupHook c) (setWMName "LG3D") }

scratchpads =
  [ NS "weechat" (termexec "weechat") m idHook
  , NS "pulseaudio" "pavucontrol" (className =? "Pavucontrol") idHook ]
  where m = className =? "Roxterm" <&&> fmap (isInfixOf "WeeChat") title

myManageHook = composeAll
  [ fullscreenManageHook
  , className =? "trayer"         --> doIgnore
  , className =? "Pygtk-shutdown" --> doCenterFloat
  , className =? "Pavucontrol"    --> doShift "NSP"
  , namedScratchpadManageHook scratchpads ]

tickerHook (ClientMessageEvent _ _ _ _ _ typ dat) = do
  a <- getAtom "TUCKER_TICKER"
  if typ /= a then return () else do
    XS.put $ SysMond ((map fromIntegral dat) :: [Int])
    ask >>= logHook.config
  return $ All True
tickerHook _ = return $ All True

logoutHook (ClientMessageEvent _ _ _ _ _ typ _) = do
  a <- getAtom "TUCKER_LOGOUT"
  if typ /= a then return () else io (exitWith ExitSuccess)
  return $ All True
logoutHook _ = return $ All True

charSize = 9

dStatColors =
  [ [ "#008000", "#198019", "#338033", "#4D804D" ]
  , [ "#000080", "#191980", "#333380", "#4D4D80" ]
  , [ "#800000", "#801919", "#803333", "#804D4D" ]
  , [ "#800080", "#801980", "#803380", "#804D80" ] ]

dStatus (cw, args) = (intercalate (pos ";" n) $ map draw args) ++ finish
  where finish = pos ";-" (y - n) ++ border ++ " "
        border = dzenColor "#2C2B2A" "" $ write ++ box "^ro" (charSize*13) y
        draw x = dzenColor (color x) "" $ box "^r" x n ++ pos "-" x
        write = text ++ pos "-" (charSize * length text)
        text = concatMap ((" " ++).showPad) args
        color x = cw !! (min 3 $ x `div` 25)
        showPad x = (if x < 10 then "0" else []) ++ show x
        box r x z = r ++ "(" ++ show x ++ "x" ++ show z ++ ")"
        pos h x = "^p(" ++ h ++ show x ++ ")"
        n = y `div` length args
        y = 20

dBackSh shape color = (++).concat $ ["^p(",a,")",s y,"^p(",b,")"]
  where (x, v) = (charSize `div` 2, -y `div` 2)
        (a, b) = (show $ x + v, show $ v - x)
        s = dzenColor color "".concat.d shape
        d "s" n = ["^r(",show n,"x",show n,")"]
        d "c" n = ["^c(",show n,")"]
        y = 16

pp h = PP
  { ppCurrent         = dBackSh "s" "#003D7B".dzenColor "yellow" ""
  , ppVisible         = dBackSh "s" "#2C2B2A".dzenColor "yellow" ""
  , ppHidden          = dBackSh "s" "#2C2B2A"
  , ppHiddenNoWindows = const ""
  , ppUrgent          = dBackSh "s" "#2C2B2A".dzenColor "red" ""
  , ppSep             = ""
  , ppWsSep           = " "
  , ppTitle           = dzenColor "green" "".pad.pad.dzenEscape.shorten 90
  , ppTitleSanitize   = id
  , ppLayout          = pad.dBackSh "c" "#003D7B".dzenColor "yellow" "".l
  , ppOrder           = \(x:y:z) -> "^ib(1)^p(_LEFT)":y:x:z
  , ppOutput          = hPutStrLn h
  , ppSort            = getSortByIndex >>= return.(f.)
  , ppExtras          = map fst mon }
  where f x = let x' = init x in last x':init x'
        l x = if x == "Tall" then "| " else "+ "

drawSpacer = return $ Just str
  where str = "^p(_RIGHT)^p(-" ++ n mon ++ ")^p(;0)"
        n = show.sum.map snd

drawStatusBars = XS.get >>= return. Just .concatMap dStatus.etc
  where etc = zip dStatColors.listn.(\(SysMond n) -> n)
        listn [w,x,y,z,t,v,m,s,u,d] = [[w,x,y,z],[m,s],[t,v],[k u,k d]]
        listn _ = [[0],[0],[0],[0]]
        k = round.(*5).log.fromIntegral.(+1)

mon = [ (drawSpacer, 0), (drawStatusBars, charSize*56)
      , (dzenColorL "lightblue" "" $ date "%a %b %d %Y %I:%M:%S", charSize*25) ]

runScrot = spawn "sleep 0.2; scrot -s -q 100 -e 'mv $f /home/tucker/Downloads'"

recompileXmonad = recompile True >>= (\n -> spawn (if n then s else f))
  where s = t ++ "'Xmonad has compiled successfully.'"
        f = t ++ "'Error: Xmonad failed to compile.'"
        t = "notify-send 'Recompiling Xmonad' "

myKeys c@(XConfig {modMask = m}) = let s = m.|.shiftMask in M.fromList $
  [ ((s, xK_Return   ), spawn $ term)
  , ((m, xK_Return   ), spawn "my_dmenu_run")
  , ((s, xK_Escape   ), spawn "pygtk-shutdown")
  , ((m, xK_Escape   ), spawn $ termexec "htop")
  , ((s, xK_BackSpace), recompileXmonad)
  , ((m, xK_BackSpace), spawn "xmonad --restart")
  , ((s, xK_Tab      ), namedScratchpadAction scratchpads "pulseaudio")
  , ((m, xK_Tab      ), namedScratchpadAction scratchpads "weechat")
  , ((0, xK_Print    ), runScrot)
  , ((m, xK_x        ), kill)
  , ((m, xK_t        ), withFocused $ windows. W.sink)
  , ((m, xK_r        ), spawn "rm ~/.cache/dmenu_run")
  , ((m, xK_m        ), spawn $ termexec "ncmpcpp")
  , ((m, xK_b        ), spawn "mpc prev")
  , ((m, xK_n        ), spawn "mpc next")
  , ((m, xK_p        ), spawn "mpc toggle")
  , ((m, xK_s        ), spawn "mpc stop")
  , ((m, xK_space    ), sendMessage NextLayout)
  , ((m, xK_Down     ), windows W.focusDown)
  , ((m, xK_Up       ), windows W.focusUp)
  , ((s, xK_Down     ), windows W.swapDown)
  , ((s, xK_Up       ), windows W.swapUp)
  , ((m, xK_Left     ), sendMessage Shrink)
  , ((m, xK_Right    ), sendMessage Expand)
  , ((s, xK_Left     ), sendMessage (IncMasterN 1))
  , ((s, xK_Right    ), sendMessage (IncMasterN (-1)))
  ] ++ [((n.|.m, k), windows $ f i)
    | (i, k) <- zip (workspaces c) ([xK_1..xK_9] ++ [xK_0,xK_grave])
    , (f, n) <- [(W.greedyView, 0),(W.shift, shiftMask)]]

myMouse (XConfig {modMask = m}) = M.fromList
  [ ((m, button1), s mouseMoveWindow)
  , ((m, button3), s mouseResizeWindow)
  , ((m, button4), v "+1%")
  , ((m, button5), v "-1%") ]
  where v = const.spawn.(concat ["pactl set-sink-volume ", n, " -- "] ++)
        n = "alsa_output.pci-0000_00_14.2.analog-stereo"
        s f w = let x = runQuery isFullscreen w
                    y = focus w >> f w >> windows W.shiftMaster
                in  x >>= (\k -> if k then return () else y)
