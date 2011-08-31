Config { font = "xft:Terminus-12:"
       , bgColor = "black"
       , fgColor = "white"
       , position = Top
       , lowerOnStart = True
       , commands = [ Run StdinReader
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%H:%M" "date" 10
                    , Run BatteryP ["BAT0"] ["-t", "Batt: <left>%"] 60
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %StdinReader%}{| %battery% | %memory% | %date% "
       }

