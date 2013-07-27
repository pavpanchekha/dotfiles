Config { font = "xft:Terminus-12"
       , position = Top
       , bgColor = "black"
       , fgColor = "white"

       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %battery% | %dbstatus% | %date% "

       , commands = [ Run StdinReader
                    , Run Battery ["-L", "10", "-H", "95", "-l", "#cc0000",
                                   "-n", "white", "-h", "#729fcf", "-t", "<acstatus>",
                                   "--", "-o", "Batt: <left>% (<timeleft>)",
                                   "-O", "AC: <left>%+ (<timeleft>)", "-i", "AC"] 10
                    , Run Com "dropbox" ["status"] "dbstatus" 600
                    , Run Date "%H:%M" "date" 10
                    ]
       }
