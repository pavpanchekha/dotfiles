#!/usr/bin/env python

DATA = """
set:MAILDIR    val:$HOME/mail/
set:DEFAULT    val:$MAILDIR/inbox.spool
set:LOGFILE    val:/tmp/procmail.log

to:reuse@mit.edu \
    keyword:reuse  archive:
to:@racket-lang.org \
    keyword:racket archive:
to:lisp-hug@lispworks.com \
    keyword:lisp   archive:
to:esp.*@mit.edu \
    keyword:esp
to:hmmt.*@mit.edu \
    keyword:hmmt
"""

def action(condition, keyword=None, archive=None):
    actions = []
    if keyword is not None:
        actions.append(("f", "| formail -a 'X-RMAIL-KEYWORDS: %s'" % keyword))
    if archive is not None:
        actions.append((":", "archive.spool"))
    
    if len(actions) == 1:
        return ":0%(flags)s\n%(condition)s\n%(action)s\n" % \
            {"flags": actions[0][0],
             "condition": condition,
             "action": actions[0][1]}
    else:
        innerblock = "\n\n".join(":0%s\n%s" % action for action in actions)
        innerblock = "    " + innerblock.replace("\n", "\n    ") # Indent properly
        
        return ":0\n%(condition)s\n{\n%(inner)s\n}\n" % \
            {"condition": condition,
             "inner": innerblock}

COMMANDS = {}

def command(f):
    COMMANDS[f.__name__] = f
    return f

@command
def to(email, **kwargs):
    return action("* ^(To|Cc): .*%s" % email, **kwargs)

@command
def set(variable, val=""):
    return "%s=%s" % (variable, val)

lines = DATA.strip().replace("\\\n", "").split("\n")
for line in lines:
    args = line.split()
    if not args:
        print("\n")
        continue

    args = [arg.split(":", 1) for arg in args]
    cmd = args[0][0]

    assert cmd in COMMANDS
    print(COMMANDS[cmd](args[0][1], **dict(args[1:])))
