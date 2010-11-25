
import os

run = [
      lambda f, tempdir, args: ["gmcs", "-out:", os.path.join(tempdir, "-out.exe"), f]
    , lambda f, tempdir, args: ["mono", os.path.join(tempdir, "-out")] + args
    ]

compile = [
      lambda f, tempdir, target, args:  ["gmcs", "-out:", target, f] + args
    ]

def outfile(f):
    return f.rsplit(".", 1)[0] + ".exe"
