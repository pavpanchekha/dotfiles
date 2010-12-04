Config { font = "xft:Ubuntu-12:"
       , bgColor = "#32322D"
       , fgColor = "#B4D0A9"
       , position = Bottom
       , lowerOnStart = True
       , commands = [ Run Com "python" ["/home/pavpanchekha/dev/lastnot.py"] "nots" 1
                    , Run MBox [("", "inbox", "green")] ["-d", "/home/pavpanchekha/mail/", "-s", " Emails"] "mbox"
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%H:%M" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%mbox% | %nots%}{| %memory% | %date% "
       }

