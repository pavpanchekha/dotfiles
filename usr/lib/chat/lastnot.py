# -*- coding: utf8 -*-


class Notification(object):
    def __init__(self, title, body, time, process, type=""):
        self.title = title
        self.body = body
        self.time = time
        self.process = process
        self.type = type

def parse(s):
    import re
    NOTBRACKETS = "[^\[\]]"
    nots = re.findall("\[(" + NOTBRACKETS + "*?), (" + NOTBRACKETS + "*?)(, " + NOTBRACKETS + "*?)?\] (.*)\n((?:.|\n)*?)\n\n", s)
    ret = []
    for n in nots:
        ts, proc, t, title, body = n
        _, ts = ts.split("T")
        ts = ts[:5]
        if ret and body.startswith(ret[-1].body):
            ret[-1] = Notification(title, body, ts, proc, t)
        else:
            ret.append(Notification(title, body, ts, proc, t))
    return ret

if __name__ == "__main__":
    nots = parse(open("/home/pavpanchekha/.cache/notify-osd.log").read())
    n = nots[-1]
    print n.title + ":", n.body.replace("\n", "… "), "(%s)" % n.time
