Config { font = "xft:Ubuntu-12:"
       , bgColor = "#32322D"
       , fgColor = "#B4D0A9"
       , position = Bottom
       , lowerOnStart = True
       , commands = [ Run Cpu ["-H", "50", "--high", "red"] 10
                    , Run MBox [("", "pavpanchekha", "green")] ["-d", "/var/spool/mail/", "-s", " Emails"] "mbox"
                    , Run Memory ["-t","Mem: <usedratio>%"] 10
                    , Run Date "%b %_d %H:%M" "date" 10
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%mbox% }{ %cpu% | %memory% | %date%             "
       }

