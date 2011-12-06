import os

EMACS_COMMAND = lambda f: [ "emacs",
      "--batch", "-Q",
      "--load", "~/.emacs.d/init.el",
      "--eval", "(require 'org-install)",
      "--visit", f,
      "--funcall", "org-export-as-latex-batch"]

def tempname(tempdir, f):
    return os.path.join(tempdir, os.path.split(f)[1])

def texname(tempdir, f):
    return os.path.join(tempdir, os.path.split(texfile(f))[1])

def outname(tempdir, f):
    return os.path.join(tempdir, os.path.split(outfile(f))[1])

run = [
      lambda f, tempdir, args: ["cp", f, tempdir]
    , lambda f, tempdir, args: EMACS_COMMAND(tempname(tempdir, f)) + args
    , lambda f, tempdir, args: ["rubber", "-d", "--into", tempdir, texname(tempdir, f)] + args
    , lambda f, tempdir, args: ["emacsclient", outname(tempdir, f)]
    ]


compile = [
      lambda f, tempdir, target, args: ["cp", f, tempdir]
    , lambda f, tempdir, target, args: ["ls", "-la", tempdir]
    , lambda f, tempdir, target, args: EMACS_COMMAND(tempname(tempdir, f)) + args
    , lambda f, tempdir, target, args: ["rubber", "-d", "--into", tempdir, texname(tempdir, f)] + args
    , lambda f, tempdir, target, args: ["mv", outname(tempdir, f), target]
    ]

def texfile(f):
    return f.rsplit(".", 1)[0] + ".tex"

def outfile(f):
    return f.rsplit(".", 1)[0] + ".pdf"
