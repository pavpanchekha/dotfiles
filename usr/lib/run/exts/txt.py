
import os

run = [
      lambda f, tempdir, args: ["rst2html", f, "--stylesheet=/home/pavpanchekha/usr/lib/run/data/tripoli.simple.css", os.path.join(tempdir, "-out.html")] + args
    , lambda f, tempdir, args: ["luakit", "file://"+os.path.join(tempdir, "-out.html")]
    ]

compile = [
      lambda f, tempdir, target, args: ["rst2html", f, "--stylesheet=/home/pavpanchekha/usr/lib/run/data/tripoli.simple.css", target] + args
    ]

def outfile(f):
    return f.rsplit(".", 1)[0] + ".html"
