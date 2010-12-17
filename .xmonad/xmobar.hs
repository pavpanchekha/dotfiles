Config { font = "xft:Terminus-15:"
       , bgColor = "black"
       , fgColor = "white"
       , position = Bottom
       , lowerOnStart = True
       , commands = [ Run StdinReader
                    , Run MBox [("", "inbox", "green")] ["-d", "/home/pavpanchekha/mail/", "-s", " Emails"] "mbox"
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%H:%M" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %mbox% | %StdinReader%}{| %memory% | %date% "
       }

