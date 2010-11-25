
import os

run = [
      lambda f, tempdir, args: ["javac", "-d", tempdir, f]
    , lambda f, tempdir, args: ["java", "-cp", tempdir, classfile(f)] + args
    ]

compile = [
      lambda f, tempdir, target, args: ["javac", "-d", tempdir, f] + args
    , lambda f, tempdir, target, args: ["mv", os.path.join(tempdir, classfile(f) + ".class"), target]
    ]

def classfile(f):
    return f.split("/")[-1].rsplit(".", 1)[0]
