Config { font = "xft:Deja Vu Sans Mono-14"
       , position = Top
       , bgColor = "#f6f5f4"
       , fgColor = "#444444"

       , sepChar = "%"
       , alignSep = "}{"
       , template = "            %StdinReader% }{ %battery% | %date% "

       , commands = [ Run StdinReader
                    , Run Battery ["-L", "10", "-H", "95", "-l", "#cc0000",
                                   "-h", "#729fcf", "-t", "<acstatus>",
                                   "--", "-o", "Batt: <left>% (<timeleft>)",
                                   "-O", "AC: <left>%+ (<timeleft>)", "-i", "AC"] 60
                    , Run Date "%H:%M" "date" 60
                    ]
       }
