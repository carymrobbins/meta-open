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
programMap =
    [ ("intellij", [ ("pycharm",  "/usr/local/bin/charm")
                   , ("rubymine", "/usr/local/bin/mine")
                   , ("intellij", "/usr/local/bin/idea")
                   ]),
      ("idea",     [ ("intellij", "/usr/local/bin/idea")
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

You define your program groups in the `programMap` config and your file
associations in (you guessed it!) the `fileTypeAssociations` config.
