;;; fbasic-mode.el --- Major mode for FreeBASIC -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Jan Felix Langenbach
;;
;; Author: Jan Felix Langenbach <http://github/janfel>
;; Maintainer: Jan Felix Langenbach <o.hase3@gmail.com>
;; Created: 2020-10-02
;; Modified: 2020-10-09
;; Version: 0.0.1
;; Keywords: languages
;; Homepage: https://github.com/janfel/fbasic-mode
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Major mode for FreeBASIC
;;
;;; Code:

(require 'subr-x)

;; Keywords
;; These keywords are taken, in part, from “fbfull.lng” which is part of FBIde.
;; The keyword list is currently at version 0.4.6r4 taken from
;; the official website at https://fbide.freebasic.net/.

(defconst fbasic-constant-keywords
  '("__DATE__" "__DATE_ISO__" "__FB_64BIT__" "__FB_ARGC__" "__FB_ARGV__"
    "__FB_ARM__" "__FB_ASM__" "__FB_BACKEND__" "__FB_BIGENDIAN__"
    "__FB_BUILD_DATE__" "__FB_CYGWIN__" "__FB_DARWIN__" "__FB_DEBUG__"
    "__FB_DOS__" "__FB_ERR__" "__FB_FPMODE__" "__FB_FPU__" "__FB_FREEBSD__"
    "__FB_GCC__" "__FB_LANG__" "__FB_LINUX__" "__FB_MAIN__" "__FB_MIN_VERSION__"
    "__FB_MT__" "__FB_NETBSD__" "__FB_OPENBSD__" "__FB_OPTION_BYVAL__"
    "__FB_OPTION_DYNAMIC__" "__FB_OPTION_ESCAPE__" "__FB_OPTION_EXPLICIT__"
    "__FB_OPTION_GOSUB__" "__FB_OPTION_PRIVATE__" "__FB_OUT_DLL__"
    "__FB_OUT_EXE__" "__FB_OUT_LIB__" "__FB_OUT_OBJ__" "__FB_PCOS__"
    "__FB_SIGNATURE__" "__FB_SSE__" "__FB_UNIX__" "__FB_VECTORIZE__"
    "__FB_VER_MAJOR__" "__FB_VER_MINOR__" "__FB_VER_PATCH__" "__FB_VERSION__"
    "__FB_WIN32__" "__FB_XBOX__" "__FILE__" "__FILE_NQ__" "__FUNCTION__"
    "__FUNCTION_NQ__" "__LINE__" "__PATH__" "__TIME__" "ASSERT" "ASSERTWARN"
    "BIT" "BITRESET" "BITSET" "DEFINED" "FALSE" "HIBYTE" "HIWORD" "LOBYTE"
    "LOWORD" "OFFSETOF" "ONCE" "RGB" "RGBA" "STOP" "TRUE" "VA_ARG" "VA_FIRST"
    "VA_NEXT"))

(defconst fbasic-keywords
  '("ACCESS" "ADD" "ALIAS" "ALPHA" "AND" "ANDALSO" "APPEND" "AS" "ASM" "BASE"
    "BINARY" "BYREF" "BYVAL" "CALL" "CASE" "CDECL" "CIRCLE" "COMMON" "CONS"
    "CONST" "CONSTRUCTOR" "CONTINUE" "CUSTOM" "DATA" "DECLARE" "DEFBYTE"
    "DEFDBL" "DEFINT" "DEFLNG" "DEFLONGINT" "DEFSHORT" "DEFSNG" "DEFSTR"
    "DEFUBYTE" "DEFUINT" "DEFULONGINT" "DEFUSHORT" "DELETE" "DESTRUCTOR" "DIM"
    "DO" "DRAW" "DYNAMIC" "ELSE" "ELSEIF" "ENCODING" "END" "ENUM" "ENVIRON"
    "EQV" "ERR" "ESCAPE" "EXIT" "EXPLICIT" "EXPORT" "EXTENDS" "EXTERN" "FIELD"
    "FOR" "FUNCTION" "GET" "GOSUB" "GOTO" "IF" "IIF" "IMP" "IMPORT" "INPUT" "IS"
    "LET" "LIB" "LINE" "LOCAL" "LOCK" "LOOP" "LPRINT" "LPT" "MOD" "NAKED"
    "NAMESPACE" "NEW" "NEXT" "NOGOSUB" "NOKEYWORD" "NOT" "ON" "OPEN" "OPERATOR"
    "OPTION" "OR" "ORELSE" "OUTPUT" "OVERLOAD" "PAINT" "PALETTE" "PASCAL" "PIPE"
    "PRESERVE" "PRESET" "PRINT" "PRIVATE" "PROPERTY" "PROTECTED" "PSET" "PUBLIC"
    "PUT" "PROCPTR" "RANDOM" "READ" "REDIM" "REM" "RESTORE" "RESUME" "RETURN"
    "SADD" "SCOPE" "SCREEN" "SCRN" "SELECT" "SHL" "SHR" "SIZEOF" "STDCALL"
    "STEP" "STRPTR" "SUB" "THEN" "THIS" "TO" "TRANS" "TYPE" "TYPEOF" "UNION"
    "UNLOCK" "UNTIL" "USING" "VARPTR" "WEND" "WHILE" "WIDTH" "WINDOW" "WITH"
    "WRITE" "XOR"))

(defconst fbasic-builtin-keywords
  '("Abs" "Acos" "Allocate" "Asc" "Asin" "Atan2" "Atn" "BLoad" "BSave" "Beep"
    "Bin" "CAllocate" "CBool" "CByte" "CDbl" "CInt" "CLng" "CLngInt" "CPtr"
    "CShort" "CSign" "CSng" "CUByte" "CUInt" "CULng" "CULngInt" "CUShort"
    "CUnsg" "Cast" "ChDir" "Chain" "Chr" "Clear" "Close" "Cls" "Color" "Com"
    "Command" "CondBroadcast" "CondCreate" "CondDestroy" "CondSignal" "CondWait"
    "Cos" "CsrLin" "CurDir" "CvLongInt" "CvShort" "Cvd" "Cvi" "Cvl" "Cvs" "Date"
    "DateAdd" "DateDiff" "DatePart" "DateSerial" "DateValue" "Day" "Deallocate"
    "Dir" "DylibFree" "DylibLoad" "DylibSymbol" "Eof" "Erase" "Erfn" "Erl"
    "Ermn" "Error" "ExePath" "Exec" "Exp" "FileAttr" "FileCopy" "FileDateTime"
    "FileExists" "FileLen" "Fix" "Flip" "Format" "Frac" "Fre" "FreeFile"
    "GetJoystick" "GetKey" "GetMouse" "Hex" "Hour" "ImageConvertRow"
    "ImageCreate" "ImageDestroy" "ImageInfo" "InKey" "InStr" "InStrRev" "Inp"
    "Int" "IsDate" "Kill" "LBound" "LCase" "LPos" "LSet" "LTrim" "Lang" "Left"
    "Len" "Loc" "Locate" "Lof" "Log" "Mid" "Minute" "MkDir" "MkLongInt"
    "MkShort" "Mkd" "Mki" "Mkl" "Mks" "Month" "MonthName" "MultiKey"
    "MutexCreate" "MutexDestroy" "MutexLock" "MutexUnlock" "Name" "Now" "Oct"
    "Out" "PCopy" "PMap" "Peek" "Point" "Poke" "Pos" "RSet" "RTrim" "Randomize"
    "Reallocate" "Reset" "Right" "RmDir" "Rnd" "Run" "ScreenControl"
    "ScreenCopy" "ScreenEvent" "ScreenGLProc" "ScreenInfo" "ScreenList"
    "ScreenLock" "ScreenPtr" "ScreenRes" "ScreenSet" "ScreenSync" "ScreenUnlock"
    "Second" "Seek" "SetDate" "SetEnviron" "SetMouse" "SetTime" "Sgn" "Shell"
    "Sin" "Sleep" "Space" "Spc" "Sqr" "Stick" "Str" "Strig" "Swap" "System"
    "Tab" "Tan" "ThreadCreate" "ThreadWait" "Time" "TimeSerial" "TimeValue"
    "Timer" "Trim" "UBound" "UCase" "Val" "ValInt" "ValLng" "ValUInt" "ValULng"
    "Var" "View" "WBin" "WChr" "WHex" "WInput" "WOct" "WSpace" "WStr" "Wait"
    "Weekday" "WeekdayName" "WindowTitle" "Year"))

(defconst fbasic-type-keywords
  '("Any" "Boolean" "Byte" "Double" "Integer" "Long" "LongInt" "Object"
    "Pointer" "Ptr" "Shared" "Short" "Single" "Static" "String" "UByte"
    "UInteger" "ULong" "ULongInt" "Unsigned" "UShort" "WString" "ZString"))

(defconst fbasic-combined-keyword-list
  (append fbasic-keywords
          fbasic-constant-keywords
          fbasic-builtin-keywords
          fbasic-type-keywords))


;; Variables

(defgroup fbasic nil
  "Major mode for FreeBASIC."
  :group 'languages)

(defcustom fbasic-indent-level 4
  "Indent level for `fbasic-mode'."
  :type 'integer
  :group 'fbasic)
;; (defalias 'fbasic-indent-level 'indent-level)

(defcustom fbasic-electric-indent-chars '(?:)
  "Electric indent characters for `fbasic-mode'."
  :type '(repeat character)
  :group 'fbasic)

(defgroup fbasic-autocaps nil
  "Automatic capitalization for FreeBASIC keywords."
  :group 'fbasic)

(defcustom fbasic-autocaps-keywords fbasic-combined-keyword-list
  "The list of keywords that should be transformed (not case sensitive)."
  :type '(repeat string)
  :group 'fbasic-autocaps)

(defcustom fbasic-autocaps-electric-chars '(?\s ?\( ?\) ?, ?\; ?=)
  "The list of chars that trigger `fbasic-autocaps-fix-last-symbol'."
  :type '(repeat char)
  :group 'fbasic-autocaps)

(defcustom fbasic-autocaps-transformer-function nil
  "An optional function to transform the fixed keyword.
If specified, this will take precedence over `fbasic-autocaps-keywords'.
This function should take a string as single argument and return a string of
equal length. Sensible choices are `upcase', `downcase' and `capitalize'."
  :type '(choice function (const :tag "Predefined" nil))
  :group 'fbasic-autocaps)


;; Font Lock

(defconst fbasic-constant-regexp
  (regexp-opt fbasic-constant-keywords 'symbols))
(defconst fbasic-keyword-regexp
  (regexp-opt fbasic-keywords 'symbols))
(defconst fbasic-builtin-keyword-regexp
  (regexp-opt fbasic-builtin-keywords 'symbols))
(defconst fbasic-type-keyword-regexp
  (regexp-opt fbasic-type-keywords 'symbols))

(defconst fbasic-label-regexp "^\\s-*\\([A-Z_][A-Z_.0-9]*:\\)")
(defconst fbasic-line-number-regexp "^\\s-*\\([[:digit:]]+\\)")
(defconst fbasic-preprocessor-regexp "^\\s-*#[A-Z][A-Z0-9_.]*")
;; (defconst fbasic-rem-comment-regexp "\\_<\\(REM\\)\\_>\\(.*\\)")
(defconst fbasic-rem-comment-regexp "\\_<REM\\_>")
(defconst fbasic-number-regexp
  (rx (? (or "+" "-"))
      (or (seq (or (+ digit)
                   (seq "&H" (+ xdigit))
                   (seq "&O" (+ (any "0-7")))
                   (seq "&B" (+ (or "0" "1"))))
               (? (or "%" "U" "L" "&" "UL" "LL" "ULL")))
          (seq (or (seq (+ digit) (? "." (* digit)))
                   (seq "." (+ digit)))
               (? (or "D" "E") (? (or "+" "-")) (* digit))
               (? (or "!" "F" "#" "D"))))))

(defconst fbasic-font-lock-keywords
  `((,fbasic-rem-comment-regexp 0 font-lock-comment-delimiter-face)
    (,fbasic-line-number-regexp 0 font-lock-constant-face)
    (,fbasic-label-regexp 0 font-lock-constant-face)
    (,fbasic-keyword-regexp 0 font-lock-keyword-face)
    (,fbasic-constant-regexp 0 font-lock-constant-face)
    (,fbasic-builtin-keyword-regexp 0 font-lock-builtin-face)
    (,fbasic-type-keyword-regexp 0 font-lock-type-face)
    (,fbasic-preprocessor-regexp 0 font-lock-preprocessor-face)))


;; Indentation

(defconst fbasic-block-keywords
  '("CLASS" "CONSTRUCTOR" "DESTRUCTOR" "ENUM" "FUNCTION" "NAMESPACE" "OPERATOR"
    "PROPERTY" "SCOPE" "SELECT" "SUB" "TYPE" "UNION" "WITH"))

(defconst fbasic-conditional-block-keywords
  `(("IF" .
     ,(lambda (limit)
        (and (re-search-forward "\\_<THEN\\_>" limit t)
             (fbasic-looking-at-blank-p limit))))
    ("ASM" .
     ,(lambda (limit)
        (goto-char (match-end 0))
        (fbasic-looking-at-blank-p limit)))
    ("EXTERN" .
     ,(lambda (limit) (not (re-search-forward "\\_<AS\\_>" limit t)))))
  "Association list of keywords that conditionally open a block.
Each element is a cons (KW . PRED) where KW is the keyword as a string.
PRED is a function that takes one argument LIMIT and returns non-nil
only when the KW instance after the point should open a block.
The position LIMIT marks the end of the current semantic line.
When PRED is called, the point is before KW and the match data contains
a match of KW at index zero. PRED is allowed to change the point and
the match data.")

(defconst fbasic-block-end-keywords
  (mapcar (lambda (k) (concat "END " k))
          (append fbasic-block-keywords
                  (mapcar #'car fbasic-conditional-block-keywords))))

(defconst fbasic-dedented-keywords
  '("ELSEIF" "ELSE" "CASE"))

(defconst fbasic-indenting-keywords
  '("WHILE" "FOR" "DO"))

(defconst fbasic-dedenting-keywords
  '("WEND" "NEXT" "LOOP"))

(defconst fbasic-dedented-line-regexp
  (regexp-opt fbasic-dedented-keywords 'symbols))
(defconst fbasic-block-start-regexp
  (regexp-opt (append fbasic-block-keywords fbasic-indenting-keywords) 'symbols))
(defconst fbasic-conditional-block-start-regexp
  (regexp-opt (mapcar #'car fbasic-conditional-block-keywords) 'symbols))
(defconst fbasic-block-end-regexp
  (regexp-opt (append fbasic-block-end-keywords fbasic-dedenting-keywords) 'symbols))


(defun fbasic-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment."
  (save-excursion (ppss-comment-depth (syntax-ppss (or pos (point))))))

(defun fbasic-in-string-p (&optional pos)
  "Return non-nil if POS is in a string."
  (save-excursion (ppss-string-terminator (syntax-ppss (or pos (point))))))

(defun fbasic-in-comment-or-string-p (&optional pos)
  "Return non-nil if POS is in a comment or string."
  (save-excursion (ppss-comment-or-string-start (syntax-ppss (or pos (point))))))

(defun fbasic-continued-line-p (&optional n)
  "Return non-nil if this line plus N is a continuation line."
  (let ((pos (line-end-position (or n 0))))
    (and (eq ?_ (char-before pos))
         (not (fbasic-in-comment-p pos)))))

(defun fbasic-continued-string-p (&optional n)
  "Return non-nil if this line plus N continues a string."
  (fbasic-in-string-p (line-end-position (or n 0))))

(defun fbasic-preprocessor-line-p (&optional n)
  "Return non-nil if this line plus N is a preprocessor directive."
  (save-excursion
    (when n (forward-line n))
    (back-to-indentation)
    (looking-at-p fbasic-preprocessor-regexp)))

(defun fbasic-move-after-last-code-char ()
  "Move after the last character of code on this line."
  (end-of-line)
  (while (and (not (bolp)) (fbasic-in-comment-or-string-p (point)))
    (backward-char)
    (skip-chars-backward " \t\r")))

(defun fbasic-semantic-end-of-line ()
  "Return the end of the current semantic line."
  (save-excursion
    (catch 'return
      (while t
        (while (and (re-search-forward "\\W:\\|[^/\n]'" (line-end-position) 'noerror)
                    (fbasic-in-comment-or-string-p (match-beginning 0))))
        (if (eolp)
            (if (fbasic-continued-line-p +1)
                (forward-line)
              (throw 'return (point)))
          (throw 'return (1- (point))))))))

(defun fbasic-looking-at-blank-p (limit)
  "Return non-nil if the buffer from point to LIMIT is semantically blank."
  (save-excursion
    (catch 'break
      (while (< (point) limit)
        (skip-syntax-forward "-" limit)
        (cond ((looking-at-p "/'") (forward-comment 1))
              ((looking-at-p "'")  (end-of-line))
              (t (throw 'break nil)))))
    (>= (point) limit)))

(defun fbasic-indent-this-delta ()
  "Calculate the indent level delta this line wants to have."
  (back-to-indentation)
  (cond ((fbasic-continued-line-p) (if (fbasic-continued-line-p +1) 0 -1))
        ((fbasic-continued-string-p) 'arb)
        ((looking-at fbasic-preprocessor-regexp) 'bol)
        ((looking-at fbasic-dedented-line-regexp) -1)
        ((looking-at fbasic-block-end-regexp) -1)
        (t 0)))

(defun fbasic-indent-next-delta ()
  "Calculate the indent level delta this line wants the next line to have."
  (let ((delta 0))
    (back-to-indentation)
    (when (and (not (fbasic-in-comment-or-string-p))
               (looking-at fbasic-dedented-line-regexp))
      (cl-incf delta))
    (while (not (eolp))
      (let ((sem-eol (fbasic-semantic-end-of-line)))
        (unless (fbasic-in-comment-or-string-p)
          (cond ((looking-at fbasic-conditional-block-start-regexp)
                 (let ((match-end (match-end 0))
                       (sequence fbasic-conditional-block-keywords)
                       (predicate
                        (lambda (elt)
                          (when (looking-at (concat "\\_<" (car elt) "\\_>"))
                            (if (funcall (cdr elt) sem-eol) 1 0)))))
                   (cl-incf delta (or (seq-some predicate sequence) 0))
                   (goto-char match-end)))
                ((looking-at fbasic-block-start-regexp) (cl-incf delta))
                ((looking-at fbasic-block-end-regexp)   (cl-decf delta))))
        (goto-char sem-eol)
        (unless (eolp)
          (forward-char)
          (skip-syntax-forward "-" (line-end-position)))))
    (if (> delta 0) 1 0)))

(defun fbasic-calculate-indent ()
  "Calculate the indentation for the current line of FreeBASIC code."
  (save-excursion
    (catch 'indent
      (beginning-of-line)
      (when (bobp) (throw 'indent 0))
      (let ((this-indent (current-indentation))
            (this-delta (fbasic-indent-this-delta))
            (continued (fbasic-continued-line-p)))
        (when (eq this-delta 'bol) (throw 'indent 0))
        (beginning-of-line)
        (when (and (eq this-delta 'arb) (not (looking-at "\\s-*$")))
          (throw 'indent this-indent))
        ;; Move to beginning of code on previous line containing code.
        (progn
          (skip-chars-backward " \t\r\n")
          (beginning-of-line)
          (skip-chars-forward " \t")
          (while (or (fbasic-continued-line-p)
                     (fbasic-in-comment-or-string-p (point))
                     (looking-at fbasic-label-regexp)
                     (looking-at fbasic-preprocessor-regexp))
            (cond
             ((bobp) (throw 'indent 0))
             ((looking-at "\\s-*#MACRO") (throw 'indent this-indent))
             ((looking-at "\\s-*#ENDMACRO")
              (throw 'indent (max 0 (* fbasic-indent-level this-delta)))))
            (skip-chars-backward " \t\r\n")
            (beginning-of-line)
            (skip-chars-forward " \t")))
        (let ((other-indent (current-indentation)))
          (when (eq this-delta 'arb) (throw 'indent other-indent))
          (let* ((other-delta (if continued 1 (fbasic-indent-next-delta)))
                 (delta (+ this-delta other-delta)))
            (max 0 (+ other-indent (* fbasic-indent-level delta)))))))))

(defvar fbasic-autocaps-mode)
(defun fbasic-indent-line ()
  "Indent current line as FreeBASIC code."
  (interactive)
  (let* ((old-indent (current-indentation))
         (new-indent (or (fbasic-calculate-indent) old-indent)))
    (unless (= old-indent new-indent)
      (delete-region (line-beginning-position)
                     (save-excursion (back-to-indentation) (point)))
      (indent-line-to new-indent))
    ;; This is cleaner than advice.
    (when fbasic-autocaps-mode (fbasic-autocaps-fix-line))))


;; Syntax Table

(defvar fbasic-mode-syntax-table
  (let ((table (make-syntax-table)))
    (mapc (lambda (c) (modify-syntax-entry c "." table))
          '(?% ?* ?< ?= ?> ?\\ ?|))
    (modify-syntax-entry ?'   "< 23"   table)
    (modify-syntax-entry ?/   ". 14cn" table)
    (modify-syntax-entry ?\n  ">"   table)
    (modify-syntax-entry ?$   "'"   table)
    (modify-syntax-entry ?!   "'"   table)
    (modify-syntax-entry ?&   "'"   table)
    (modify-syntax-entry ?_   "_"   table)
    ;; MAYBE: REM and line numbers using Syntax Properties.
    table)
  "Syntax table used while in `fbasic-mode'.")

(defun fbasic-syntax-propertize-function (beg end)
  "Propertize the text from BEG to END."
  (goto-char beg)
  (let* ((clauses '("\\_<REM\\_>\\(.\\)"
                    "\\(\\\\\\)\\s\""
                    "\\(\\s\"\\)\\s\""
                    "\\(_\\)$"))
         (regex (string-join clauses "\\|")))
    (while (and (< (point) end) (re-search-forward regex end t))
      (cond
       ;; Propertize REM comments.
       ((match-beginning 1)
        (let ((ppss (syntax-ppss (match-beginning 1))))
          (unless (or (ppss-string-terminator ppss) (ppss-comment-depth ppss))
            (put-text-property
             (match-beginning 1) (match-end 1) 'syntax-table
             (string-to-syntax "<")))
          (goto-char (match-beginning 1))))
       ;; Make ?\\ escape ?" only in escaped strings.
       ((match-beginning 2)
        (let* ((ppss (syntax-ppss (match-beginning 2)))
               (in-string  (ppss-string-terminator ppss))
               (string-start (ppss-comment-or-string-start ppss))
               (prefix-op  (char-before string-start)))
          ;; NOTE: When supporting further dialects, check for OPTION ESCAPE.
          (when (and in-string (eq prefix-op ?!))
            (put-text-property
             (match-beginning 2) (match-end 2) 'syntax-table
             (string-to-syntax "\\")))
          (goto-char (match-end 2))))
       ;; Make ?" escape ?" in strings.
       ((match-beginning 3)
        (when (ppss-string-terminator (syntax-ppss (match-beginning 3)))
          (put-text-property
           (match-beginning 3) (match-end 3) 'syntax-table
           (string-to-syntax "\\")))
        (goto-char (match-end 3)))
       ;; Propertize line continuation as whitespace.
       ((match-beginning 4)
        )
       ;; MAYBE: Propertize line numbers as whitespace so `back-to-indentation'
       ;; would skip over them. Would require changing `fbasic-calculate-indent'
       ;; et al to use syntax classes. That being said, supporting line numbers
       ;; would require changing `fbasic-calculate-indent' anyway.
       ))))


;; Completion at Point Integration

(defun fbasic--completion-at-point-annotation-function (str)
  "Annotate STR as required by `completion-at-point-functions'."
  (declare (side-effect-free t))
  (cond ((string-match-p fbasic-keyword-regexp str) " <k>")
        ((string-match-p fbasic-builtin-keyword-regexp str) " <b>")
        ((string-match-p fbasic-constant-regexp str) " <d>")))

(defun fbasic-completion-at-point ()
  "Function used for `completion-at-point-functions' in `fbasic-mode'."
  (with-syntax-table fbasic-mode-syntax-table
    (when (and (memq (char-syntax (char-before)) '(?w ?_))
               (not (fbasic-in-comment-or-string-p)))
      (let ((pos (point))
            (beg (progn (forward-symbol -1) (point)))
            (end (progn (forward-symbol +1) (point))))
        (goto-char pos)
        (list beg end fbasic-combined-keyword-list
              :annotation-function
              #'fbasic--completion-at-point-annotation-function)))))


;; FBasic Autocaps Mode

(defun fbasic-autocaps-inhibit-fix-p ()
  "Return non-nil if the word before `point' should not be fixed."
  (or (fbasic-preprocessor-line-p)
      (fbasic-in-string-p (point))
      ;; (fbasic-in-asm-block-p (point))
      (fbasic-in-comment-p (point))))

(defun fbasic-autocaps-fix-last-symbol ()
  "Autocapitalize the symbol directly before point."
  (interactive)
  (unless (fbasic-autocaps-inhibit-fix-p)
    (let* ((case-fold-search t)
           (end (point-marker))
           (beg (progn (forward-symbol -1) (point))))
      (when (< beg end)
        (let* ((sym (buffer-substring-no-properties beg end))
               (rep (car (member-ignore-case sym fbasic-autocaps-keywords))))
          (when (and rep (not (string= sym rep)))
            (set-marker-insertion-type end t)
            (delete-region beg end)
            (insert (if (fboundp fbasic-autocaps-transformer-function)
                        (funcall fbasic-autocaps-transformer-function rep) rep))))
        (goto-char end)))))

(defun fbasic-autocaps-fix-line ()
  "Autocapitalize every word on the current line."
  (interactive)
  (save-excursion
    (let ((eol (line-end-position)))
      (beginning-of-line)
      (while (and (forward-word-strictly) (<= (point) eol))
        (fbasic-autocaps-fix-last-symbol)))))

(defun fbasic-autocaps-post-self-insert-function ()
  "Autocapitalize the last symbol when an electric character is typed."
  (when (memq last-command-event fbasic-autocaps-electric-chars)
    (backward-char)
    (fbasic-autocaps-fix-last-symbol)
    (forward-char)))

;;;###autoload
(define-minor-mode fbasic-autocaps-mode
  "Toggle automatic BASIC keyword capitalization (FBasic Autocaps Mode)."
  :group 'fbasic-autocaps
  (if fbasic-autocaps-mode
      (add-hook 'post-self-insert-hook #'fbasic-autocaps-post-self-insert-function nil t)
    (remove-hook 'post-self-insert-hook #'fbasic-autocaps-post-self-insert-function t)))


;; FBasic Mode

(defun fbasic-open-block (type)
  "Create a block of TYPE around the current line or region."
  (interactive "sBlock type: ")
  (setq type (upcase type))
  (let ((open (format "%s\n" type))
        (close (format "END %s\n" type))
        (pos (point-marker))
        (beg (line-beginning-position))
        (end (line-end-position)))
    (when (region-active-p)
      (setq beg (progn (goto-char (region-beginning)) (line-beginning-position)))
      (setq end (progn (goto-char (region-end)) (line-end-position))))
    (goto-char end)
    (newline)
    (insert close)
    (setq end (point-marker))
    (goto-char beg)
    (when (= beg pos) (set-marker-insertion-type pos t))
    (insert open)
    (indent-region beg end)
    (goto-char pos)))

(defun fbasic-indent-new-comment-line (&optional soft)
  "Break the current line onto a new comment line.
Use soft linebreaks when SOFT is non-nil."
  (interactive)
  ;; TODO: Handle `fill-prefix' and `soft'.
  (let ((comment-line-break-function nil))
    (cond ((save-excursion
             (back-to-indentation)
             (looking-at-p fbasic-rem-comment-regexp))
           (newline-and-indent)
           (insert "REM ")
           (when fbasic-autocaps-mode
             (fbasic-autocaps-fix-last-symbol)))
          ((eq (nth 4 (save-excursion (syntax-ppss))) t)
           (comment-indent-new-line soft))
          (t (newline-and-indent)))))

(defvar fbasic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-b") #'fbasic-open-block)
    map)
  "Keymap for `fbasic-mode'.")

;;;###autoload
(define-derived-mode fbasic-mode prog-mode "FreeBASIC"
  "Major mode for editing FreeBASIC code.

\\{fbasic-mode-map}"
  :group 'fbasic
  (setq-local case-fold-search t)
  (setq-local completion-ignore-case t)
  (setq-local indent-line-function #'fbasic-indent-line)
  (setq-local syntax-propertize-function #'fbasic-syntax-propertize-function)
  (setq-local comment-line-break-function #'fbasic-indent-new-comment-line)
  (setq-local comment-start "'")
  (setq-local comment-add 1)
  (setq-local comment-use-syntax t)
  ;; (setq-local comment-quote-nested nil)
  (setq-local comment-start-skip "\\(?:/?'+\\|\\_<REM\\_>\\)\\s-*")
  (setq-local comment-end-skip nil)
  ;; (setq-local comment-end-skip "[ \t\r]*\\(\\s>\\|\n\\)")
  (setq-local electric-indent-chars
              (append (when (boundp 'electric-indent-chars)
                        (default-value 'electric-indent-chars))
                      fbasic-electric-indent-chars))
  (setq-local font-lock-defaults '(fbasic-font-lock-keywords nil t))
  (add-hook 'completion-at-point-functions #'fbasic-completion-at-point t t))

(add-hook 'fbasic-mode-hook #'fbasic-autocaps-mode)
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.b\\(as\\|i\\)\\'" . fbasic-mode))


(provide 'fbasic-mode)
;;; fbasic-mode.el ends here
