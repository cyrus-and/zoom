<!--

Please make sure to have searched for existing issues before opening new ones:

    https://github.com/cyrus-and/zoom/issues?q=

Also check the FAQ section of the README:

    https://github.com/cyrus-and/zoom#faq

-->

## Environment

<!-- Fill in the following information. -->

| Component | Version |
|-|-|
| Operative system | ??? |
| Emacs (`M-x emacs-version`) | ??? |
| Zoom (`M-x describe-package RET zoom`) | ??? |

## Description

<!-- Provide a DETAILED description of your problem. -->

## Minimal `.emacs`

<!-- Complete the minimal standalone .emacs that reproduces your issue. -->

```el
(setq custom-file "/dev/null")

(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-install 'zoom)
;; install other packages here...

(custom-set-variables
 ;; set up and enable other packages here
 '(zoom-mode t))
```

Place the above `.emacs` in a new directory then run Emacs like follows:

```sh
HOME="/path/to/dir" emacs
```
