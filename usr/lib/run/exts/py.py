
import os
import py_compile

run = [
      lambda f, tempdir, args: ["python", f] + args
    ]

compile = [
      lambda f, tempdir, target, args: py_compile.compile(f, target)
    ]

def outfile(f):
    return f.rsplit(".", 1)[0] + ".pyc"
