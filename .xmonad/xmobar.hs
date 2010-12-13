Config { font = "xft:Terminus-15:"
       , bgColor = "#32322D"
       , fgColor = "white"
       , position = Bottom
       , lowerOnStart = True
       , commands = [ Run PipeReader "/home/pavpanchekha/.cache/notification.pipe" "nots"
                    , Run MBox [("", "inbox", "green")] ["-d", "/home/pavpanchekha/mail/", "-s", " Emails"] "mbox"
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%H:%M" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = " %mbox% | %nots%}{| %memory% | %date% "
       }

