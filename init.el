;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Vikas Mishra
;;
;; Author: Vikas Mishra <vikas.mishra@hey.com>
;; Maintainer: Vikas Mishra <vikas.mishra@hey.com>
;; Created: March 22, 2022
;; Modified: March 22, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/vikasmis/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))

;;; init.el ends here
