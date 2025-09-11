:set -fno-warn-orphans -Wno-type-defaults -XMultiParamTypeClasses -XOverloadedStrings
:set prompt ""
:set -package tidal

-- Import all the boot functions and aliases.
import Sound.Tidal.Boot

import           Data.Char          as Char
import qualified Data.Map.Strict    as Map

default (Rational, Integer, Double, Pattern String)

let mspTarget = Target { oName = "msp", oAddress = "127.0.0.1", oHandshake = False, oPort = 7400, oBusPort = Nothing, oLatency = 0.2, oSchedule = Pre BundleStamp, oWindow = Nothing }
let mspShape = OSC "/tidal/event/" $ Named {requiredArgs = ["s_max"]}
let s_max = pS "s_max"
let bus = pI "bus"

let editorTarget = Target { oName = "editor", oAddress = "127.0.0.1", oPort = 6013, oLatency = 0.02, oSchedule = Live, oWindow = Nothing, oHandshake = False, oBusPort = Nothing }
let editorShape = OSCContext "/editor/highlights"

-- oscmap = [(mspTarget, [mspShape]), (superdirtTarget { oPort = 57120, oLatency = 0.02 }, [superdirtShape]), (editorTarget, [editorShape])]

-- Create a Tidal Stream with the default settings.
-- To customize these settings, use 'mkTidalWith' instead
-- tidalInst <- mkTidal
-- tidalInst <- mkTidalWith oscmap defaultConfig 
tidalInst <- mkTidalWith [(superdirtTarget { oPort = 57120, oLatency = 0.02 }, [superdirtShape]), (editorTarget, [editorShape])] (defaultConfig {cFrameTimespan = 1/50, cProcessAhead = 1/20}) 

-- tidalInst <- mkTidalWith [(superdirtTarget { oLatency = 0.01 }, [superdirtShape])] (defaultConfig {cFrameTimespan = 1/50, cProcessAhead = 1/20})

-- This orphan instance makes the boot aliases work!
-- It has to go after you define 'tidalInst'.
instance Tidally where tidal = tidalInst

-- `enableLink` and `disableLink` can be used to toggle synchronisation using the Link protocol.
-- Uncomment the next line to enable Link on startup.
-- enableLink

-- You can also add your own aliases in this file. For example:
-- fastsquizzed pat = fast 2 $ pat # squiz 1.5

:{
    let all = streamAll tidal
:}

:script "/Users/bss/livecoding/tidal/setup/functions-tidal2.tidal"
:script "/Users/bss/livecoding/tidal/setup/functions-midi.tidal"
:script "/Users/bss/livecoding/tidal/setup/functions-superdirt.tidal"
:script "/Users/bss/livecoding/tidal/setup/functions-msp.tidal"

:set prompt "tidal> "
:set prompt-cont ""
