Config { font = "-*-terminus-medium-*-*-*-*-180-*-*-*-*-*-*"
       , position = Top
       , bgColor = "black"
       , fgColor = "white"

       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %battery% | %dbstatus% | %date% "

       , commands = [ Run StdinReader
                    , Run Battery ["-L", "10", "-H", "95", "-t", "Batt: <left>%",
                                   "-l", "#cc0000", "-n", "white", "-h", "#729fcf"] 10
                    , Run Com "dropbox" ["status"] "dbstatus" 600
                    , Run Date "%H:%M" "date" 10
                    ]
       }
