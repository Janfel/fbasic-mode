;;; fbasic-mode-flycheck.el --- Flycheck integration for fbasic-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jan Felix Langenbach
;;
;; Author: Jan Felix Langenbach <http://github/janfel>
;; Maintainer: Jan Felix Langenbach <o.hase3@gmail.com>
;; Created: 2020-10-09
;; Modified: 2020-10-09
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/janfel/fbasic-mode-flycheck
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Flycheck integration for fbasic-mode
;;
;;; Code:

(require 'flycheck)

(flycheck-def-args-var flycheck-fbc-args basic-fbc)

(flycheck-def-option-var flycheck-fbc-warnings '("all") basic-fbc
  "A list of additional warnings for FBC.

This variable is a list of strings, where each string is a numeric
warning level or the name of a warning category to enable.

Refer to the FBC manual at URL `https://www.freebasic.net/wiki/CompilerOptw'
for more information about warning levels."
  :type '(choice (const  :tag "No additional warnings" nil)
                 (repeat :tag "Additional warnings"
                         (string :tag "Warning name or level")))
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-fbc-dialect nil basic-fbc
  "The BASIC dialect to use in FBC."
  :type '(choice (const :tag "Default dialect" nil)
                 (const :tag "FreeBASIC" "fb")
                 (const :tag "FreeBASIC Lite" "fblite")
                 (const :tag "QuickBASIC" "qb")
                 (const :tag "Deprecated" "deprecated"))
  :safe #'flycheck-string-or-nil-p)

(flycheck-def-option-var flycheck-fbc-includes nil basic-fbc
  "A list of include files to be prepended to the file checked by FBC."
  :type '(repeat (file :tag "Include file"))
  :safe #'flycheck-string-list-p)

(flycheck-def-option-var flycheck-fbc-include-path nil basic-fbc
  "A list of include directories for FBC."
  :type '(repeat (directory :tag "Include directory"))
  :safe #'flycheck-string-list-p)

(flycheck-define-checker basic-fbc
  "A FreeBASIC syntax checker using the fbc compiler."
  :command ("fbc" "-pp" "-noerrline" "-maxerr" "inf"
            (option "-lang" flycheck-fbc-dialect)
            (option-list "-w" flycheck-fbc-warnings)
            (option-list "-i" flycheck-fbc-include-path)
            (option-list "-include" flycheck-fbc-includes)
            (eval flycheck-fbc-args)
            "-o" temporary-file-name "-b" source)
  :error-patterns
  ((warning line-start (file-name) ?\( line ?\) " warning " (id (+ digit))
            ?\( (+ digit) ?\) ": " (message) line-end)
   (error line-start (file-name) ?\( line ?\) " error " (id (+ digit))
          ": " (message) line-end))
  :modes (fbasic-mode))

(add-to-list 'flycheck-checkers 'basic-fbc)

(provide 'fbasic-mode-flycheck)
;;; fbasic-mode-flycheck.el ends here
