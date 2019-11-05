# Bitbucket Server integration

The plugin provides some handy functions for interacting with a Bitbucket Server
git server:

- `M-x bitbucket-server-open-pr` will open a browser webpage to your Bitbucket
  Server pull request creation page
- `M-x bitbucket-server-open-file` will open a browser webpage to your Bitbucket
  Server pointing to the current buffer where the command was ran
- `M-x bitbucket-server-open-file-at-point` will open a browser webpage to your
  Bitbucket Server pointing to the current buffer **and** the current line
  number
  
**Caution**: This plugin does not work with Bitbucket Cloud (https://bitbucket.org)

When running a command for the first time, you will be prompted to add the
Bitbucket Server URL (e.g. https://mybitbucket.com). This is stored in the
`bitbucket-server-url` variable, but it will be cleared once you exit Emacs. To
avoid being prompted every time, set the variable in your `init.el` file.

```emacs-lisp
(setq bitbucket-server-url "https://mybitbucket.com")
```

More functionality to come...

## Prerequisites

- magit
- projectile

## Install

### Doom Emacs

1. Download the `bitbucket-server.el` file to `~/.doom.d/lisp`
   `wget https://raw.githubusercontent.com/mihai-dinu/bitbucket-server.el/master/bitbucket-server.el -O ~/.doom.d/lisp/bitbucket-server.el`
2. Add `(load! "lisp/bitbucket-server")` to `~/.doom.d/config.el`

### Vanilla Emacs

1. Download the `bitbucket-server.el` file to `~/.emacs.d/lisp`
   `wget https://raw.githubusercontent.com/mihai-dinu/bitbucket-server.el/master/bitbucket-server.el -O ~/.emacs.d/lisp/bitbucket-server.el`
2. Add `(load "lisp/bitbucket-server")` to your Emacs init file

Tested with Emacs 26.3 and Bitbucket Server 5.8.0
