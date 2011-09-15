
import os

run = [
      lambda f, tempdir, args: ["rubber", "-d", "--into", tempdir, f] + args
    , lambda f, tempdir, args: ["emacsclient", os.path.join(tempdir, os.path.split(outfile(f))[1])]
    ]


compile = [
      lambda f, tempdir, target, args: ["rubber", "-d", "--into", tempdir, f] + args
    , lambda f, tempdir, target, args: ["mv", os.path.join(tempdir, os.path.split(outfile(f))[1]), target]
    ]

def outfile(f):
    return f.rsplit(".", 1)[0] + ".pdf"
