
import os

run = [
      lambda f, tempdir, args: ["ghc", "-O3", "-o", os.path.join(tempdir, "-out"), f, "-outputdir", tempdir, "--make"]
    , lambda f, tempdir, args: [os.path.join(tempdir, "-out")] + args
    ]

compile = [
    lambda f, tempdir, args: ["ghc", "-O3", "-o", target, f, "-outputdir", tempdir, "--make"] + args
    ]

def outfile(f):
    return f.rsplit(".", 1)[0]
