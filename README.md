meta-open
=========

This program helps determine which program should open a file.  You need to
create a config file called `.meta-open.hs` in your home directory.  Your
config file simply informs meta-open the priority of each program with a file
type.  It checks running processes to determine which program is running and
opens it with the first available.  If none are available, it defaults to 
your system's `open` command.

Here's an example config file:

```haskell
-- ~/.meta-open.hs
programMap =
    [ ("intellij", [ ("pycharm",  "/usr/local/bin/charm")
                   , ("rubymine", "/usr/local/bin/mine")
                   , ("idea",     "/usr/local/bin/idea")
                   ]),
      ("idea",     [ ("idea",     "/usr/local/bin/idea")
                   ])
    ]

fileTypeAssociations =
    [ (".py",   "intellij")
    , (".js",   "intellij")
    , (".json", "intellij")
    , (".css",  "intellij")
    , (".html", "intellij")
    , (".xml",  "intellij")
    , (".sh",   "intellij")
    , (".sql",  "intellij")
    , (".java", "idea")
    ]
```

For example, if you run `meta-open foo.py`, meta-open will determine that
`.py` files are opened by the `intellij` map from the `fileTypeAssociations`.
It then looks in the `programMap` to determine which programs to look for.
It will grep for a process containing `pycharm`.  If it exists, it will
then execute `/usr/local/bin/charm foo.py` for you.  Otherwise, it will
continue down the list of `intellij` programs until it finds one.

Ummmmm, but why?
----------------

Good question, I'm glad you asked.  I enjoy enjoy using iTerm's cmd+click
feature to open files that have output from, say, ack.  I may associate `.json`
files with PyCharm, but if I'm using RubyMine I'd like iTerm to figure that out
and open my file in the program I'm actually using.  Well, good news for me,
iTerm supports running a custom command when you cmd+click via
*Preferences > Profiles > Advanced > Semantic History*.  In my case,
all I needed to do was use `path/to/meta-open \1` and ack'ing and opening
became far simpler.
