[![MELPA](https://melpa.org/packages/uptimes-badge.svg)](https://melpa.org/#/uptimes)

# Commentary

`uptimes.el` provides a simple system for tracking and displaying the
uptimes of your Emacs sessions. Simply loading uptimes.el from your
`~/.emacs` file will start the tracking of any session.

The latest version of uptimes.el can be found at:

```
  <URL:https://github.com/davep/uptimes.el>
```

# TODO

o The auto-update stuff won't work with xemacs because I'm making use of
  `run-at-time`. If anyone knows of a high-level function that does the same
  thing and is available in GNU emacs and xemacs I'd love to hear about it.

# Thanks

Istvan Marko <imarko@pacificnet.net> for pointing out that a default of one
second for `uptimes-auto-save-interval' was probably a little OTT.

Doug Elias <dme7@cornell.edu> for pointing out that midnight.el is a recent
addition to emacs.

Nix <nix@esperi.demon.co.uk> for pointing out that some XEmacs users might
need `inhibit-clash-detection' set to t at points in this code.
