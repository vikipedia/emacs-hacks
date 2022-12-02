# org-roam
org-roam installation is very tricky. 
1. in init.el comment out org-roam stuff
1. clean  ~/.emacs.d folder
1. launch emacs
1. after everything is auto installed, manually install org-roam using package manager
1. if installation succeeds, open init.el and uncomment org-roam settings.
1. relaunch emacs and search for org-roam node.

### How to deal with this message : IMPORTANT: please install Org from GNU ELPA as Org ELPA will close before Org 9.6
(from stack overflow.)
In my experience there are 4 step you may need to take to avoid this annoying message:

1. Remove the org package source in your emacs config. Just remove the line (add-to-list ' package-archives '("org" . "https://orgmode.org/elpa/") t)
2. Delete all org-* packages which status is installed in your M-x list-package. You can use / s installed to filter all the installed packages in list-package page. Careful, don't delete the org package which status is builtin, or you need to reinstall it later.
3. Delete the package cache in elpa cache directory, the default location is ~/.emacs.d/elpa/. Delete all files started with org-*.
4. If you are using native lisp compilation, delete the compiled caches, the default location is ~/.emacs.d/eln-cache/<your-emacs-version>/.

After the 4 step, restart your emacs, the message should have disappeared.

Additionally, if your install your package using use-package you may want to pin the org package to your GNU source like follow, then use-package would try to download org package only from the GNU source.

(use-package org
  :pin gnu)

