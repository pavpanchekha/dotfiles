
(setq newsticker-url-list
      '(("Ars Technica, Open Ended"
         "http://feeds.arstechnica.com/arstechnica/open-source" nil nil nil)
        ("Motho ke motho ka botho"
         "http://kmandla.wordpress.com/feed/" nil nil nil)
        ("Planet GNOME"
         "http://planet.gnome.org/atom.xml" nil nil nil)
        ("Good Math, Bad Math"
         "http://scientopia.org/blogs/goodmath/feed/" nil nil nil)
        ("Planet Python"
         "http://planet.python.org/rss20.xml" nil nil nil)
        ("Existential Type"
         "http://existentialtype.wordpress.com/feed/" nil nil nil)
        ("Lambda the Ultimate"
         "http://lambda-the-ultimate.org/rss.xml" nil nil nil)
        ("Lingua Computa"
         "http://nonchalantlytyped.net/blog/feed/" nil nil nil)
        ("One Div Zero"
         "http://james-iry.blogspot.com/feeds/posts/default" nil nil nil)
        ("The Axis Of Eval"
         "http://www.blogger.com/feeds/5722310642266356003/posts/default" nil nil nil)
        ("Coding Horror"
         "http://feeds.feedburner.com/codinghorror/" nil nil nil)
        ("dive into mark"
         "http://feeds.feedburner.com/diveintomark/all" nil nil nil)
        ("Paul Graham: Essays"
         "http://www.aaronsw.com/2002/feeds/pgessays.rss" nil nil nil)
        ("Abstruse Goose"
         "http://abstrusegoose.com/feed/atom" nil nil nil)
        ("XKCD Blog"
         "http://blag.xkcd.com/feed/" nil nil nil)
        ("XKCD Comic"
         "http://www.xkcd.com/rss.xml" nil nil nil)))

(setq newsticker-cache-filename "~/.emacs.d/newsticker/cache"
      newsticker-html-renderer 'w3m-region
      newsticker-url-list-defaults nil)
