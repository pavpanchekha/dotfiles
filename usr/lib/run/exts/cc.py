
import os

run = [
      lambda f, tempdir, args: ["g++", "-O3", "-o", os.path.join(tempdir, "-out"), f]
    , lambda f, tempdir, args: [os.path.join(tempdir, "-out")] + args
    ]

compile = [
      lambda f, tempdir, target, args:  ["g++", f, "--O3", "-o", target, "-g"] + args
    ]

def outfile(f):
    return f.rsplit(".", 1)[0]
