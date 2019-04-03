;;; logcat.el --- Major mode for reading Android logs --- -*- lexical-binding: t -*-

;; Copyright (C) 2015 Daniel Colascione <dancol@dancol.org>

;; Author: Daniel Colascione <dancol@dancol.org>
;; Version 1
;; Date: 2015-03-18
;; Keywords: android, logfile, logcat
;; URL: https://github.com/dcolascione/logcat-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary

;; logcat-mode provides a convenient interface for viewing Android
;; system log files.  It supports recording, filtering, and searching
;; log output.
;;
;; It relies on fb-adb [1], which you can find at
;; https://github.com/facebook/fb-adb.

;; N.B. This package is best used when byte-compiled.

;; [1] We need fb-adb because need a binary-clean channel to the
;; device in order for logcat -B to work.  The stock adb package
;; mangles LF to CRLF (because it runs all subprocesses in a pty) and
;; corrupts the logcat binary format, preventing our parsing it.

(require 'cl-lib)
(require 'bytecomp)
(require 'ring)
(require 'widget)
(require 'wid-edit)

(cl-declaim (optimize (safety 0) (speed 3)))

(defconst logcat-display-style-alist
  '((threadtime . logcat-insert-record-threadtime)
    (brief . logcat-insert-record-brief))
  "Available log rendering styles.
Alist mapping symbol to logcat inserter function.  Each function
accepts a single argument, RECORD, and inserts that record
somehow at point.  The insertion function should use
`logcat-insert-as' to mark which characters in the inserted text
correspond to which aspects of a log.")

(defconst logcat--fields
  '(date time pid tid priority tag message)
  "List of all available fields.")

(defconst logcat--field-to-invisible-spec
  (cl-loop
       for field in logcat--fields
     collect (cons field (intern (format "logcat-field-%s" field))))
  "Alist mapping field symbols to invisibility-spec entries.")

(defun logcat--field-to-ispec (field)
  (or (cdr (assq field logcat--field-to-invisible-spec))
      (error "unknown field %s" field)))

(defconst logcat--filterable-fields
  '(pid tid priority tag message)
  "List of available filterable fields.")

(defun logcat--field-displayed-p (field)
  (not (memq (logcat--field-to-ispec field)
             buffer-invisibility-spec)))

(defgroup logcat nil "Customizations for `logcat-mode`"
  :prefix "logcat-"
  :group 'tools)

(define-widget 'logcat-schoice 'choice
  "Choose among `logcat-sgroup' widgets.
Each `logcat-sgroup' widget contains a `logcat-schoice-item'
button that allows the user to check which sgroup is selected."
  :format "%v")

(defun logcat--sgroup-menu-tag-get (widget)
  (let ((args (widget-get widget :args))
        (found nil))
    (while (and args (not found))
      (let ((arg (car args)))
        (setf args (cdr args))
        (when (eq (car-safe arg) 'logcat-schoice-item)
          (setf found (widget-get arg :tag)))))
    found))

(define-widget 'logcat-sgroup 'group
  "One choice for a `logcat-schoice'."
  :action 'widget-parent-action
  :menu-tag-get 'logcat--sgroup-menu-tag-get
  :format "%v")

(defun logcat-schoice-item-create (widget)
  (funcall (widget-get '(choice-item) :create) widget)
  (unless (widget-get widget :tag)
    (let* ((parent (widget-get widget :parent))
           (parent-tag (and parent (widget-get parent :tag))))
      (widget-put widget :tag parent-tag))))

(define-widget 'logcat-schoice-item 'choice-item
  "Widget that lets users choose among `logcat-sgroup's."
  :button-face 'link
  :format "%[%t%] ")

(defun logcat--transpose-infix-prefix (_widget value)
  (or (ignore-errors (append (list (cadr value) (car value))
                             (cddr value)))
      value))

(defun logcat--string-subtype-match (_widget value field)
  (ignore-errors
    (and (memq (nth 0 value) '(= =~))
         (eq (nth 1 value) field)
         (stringp (nth 2 value))
         (null (nthcdr 3 value)))))

(defun logcat--make-string-subtype (field tag)
  `(logcat-sgroup
    :value (=~ ,field "")
    :match ,(lambda (widget value)
              (logcat--string-subtype-match widget value field))
    :value-to-internal logcat--transpose-infix-prefix
    :value-to-external logcat--transpose-infix-prefix
    (logcat-schoice-item :tag ,tag :value ,field)
    (logcat-schoice
     :inline t
     (logcat-sgroup
      :inline t
      (logcat-schoice-item :tag "matches" :value =~)
      (regexp :format "%v"))
     (logcat-sgroup
      :inline t
      (logcat-schoice-item :tag "equals" :value =)
      (string :format "%v")))))

(defun logcat--make-numeric-subtype (field tag)
  `(logcat-sgroup
    :match ,(lambda (_widget value)
              (ignore-errors
                (and (eq (nth 0 value) '=)
                     (eq (nth 1 value) field)
                     (integerp (nth 2 value))
                     (null (nthcdr 3 value)))))
    :value-to-internal logcat--transpose-infix-prefix
    :value-to-external logcat--transpose-infix-prefix
    (logcat-schoice-item :tag ,tag :value ,field)
    (const :format " equals " :value =)
    (integer :format "%v")))

(defun logcat--make-priority-subtype (field tag)
  `(logcat-sgroup
    :match ,(lambda (_widget value)
              (ignore-errors
                (and (memq (nth 0 value) '(< <= = >= >))
                     (eq (nth 1 value) field)
                     (memq (nth 2 value) '(E W I D V))
                     (null (nthcdr 3 value)))))
    :value-to-internal logcat--transpose-infix-prefix
    :value-to-external logcat--transpose-infix-prefix
    (logcat-schoice-item :tag ,tag :value ,field)
    (logcat-schoice
     (logcat-schoice-item :tag "is as or more severe than" :value >=)
     (logcat-schoice-item :tag "is as or less severe than" :value <=)
     (logcat-schoice-item :tag "is more severe than" :value >)
     (logcat-schoice-item :tag "is less severe than" :value <)
     (logcat-schoice-item :tag "equals" :value =))
    (logcat-schoice
     (logcat-schoice-item
      :tag "error" :format "%[%t%]\n" :value E
      :button-face (logcat-error link))
     (logcat-schoice-item
      :tag "warning" :format "%[%t%]%n" :value W
      :button-face (logcat-warning link))
     (logcat-schoice-item
      :tag "info" :format "%[%t%]%n" :value I
      :button-face (logcat-info link))
     (logcat-schoice-item
      :tag "debug" :format "%[%t%]%n" :value D
      :button-face (logcat-debug link))
     (logcat-schoice-item
      :tag "verbose" :format "%[%t%]%n" :value V
      :button-face (logcat-verbose link)))))

(defun logcat--make-recursive-subtype (op tag)
  ;; N.B. We work around a widgets library bug below.  We're in a
  ;; group, and group tries to add the correct indentation for each
  ;; item if that item starts on its own line.  If one such item is an
  ;; editable-list, however, we get double indentation, because
  ;; editable-list also tries to add indentation.  By putting the
  ;; newline at the start of the item format instead of at the end of
  ;; the previous item's format, we defeat the first check, rely
  ;; entirely on the editable-list formatting, and get correct
  ;; indentation.  We should still be correct even after someone fixes
  ;; the widget library bug.
  `(logcat-sgroup
    :offset 0
    :extra-offset 2
    (logcat-schoice-item :tag ,tag :format "%[%t%]:" :value ,op)
    (,(if (eq op 'not) 'group 'editable-list)
      :inline t
      :format ,(concat "\n%v" (if (eq op 'not) "" "%i\n"))
      :offset 0
      :extra-offset 0
      :args ((logcat-filter-condition)))))

(defun logcat--make-predicate-subtype (tag)
  `(logcat-sgroup
    (logcat-schoice-item :tag ,tag
                         :value funcall
                         :format "%[%t%] returns true ")
    (function :format "%v")))

(define-widget 'logcat-filter-condition 'lazy
  "A criterion for `logcat-mode' filtering."
  :format "%v"
  :type `(logcat-schoice
          :value (>= priority I)
          ,(logcat--make-string-subtype 'tag "tag")
          ,(logcat--make-string-subtype 'message "message")
          ,(logcat--make-numeric-subtype 'pid "process ID")
          ,(logcat--make-numeric-subtype 'tid "thread ID")
          ,(logcat--make-priority-subtype 'priority "priority")
          ,(logcat--make-predicate-subtype "a predicate")
          ,(logcat--make-recursive-subtype
            'and "all of the following conditions hold")
          ,(logcat--make-recursive-subtype
            'or "any of the following conditions holds")
          ,(logcat--make-recursive-subtype
            'not "the following condition does not hold")))

(define-widget 'logcat-filter-widget 'group
  "Widget for editing a single logcat filter."
  :tag "test group"
  :format "%v"
  :args '((logcat-schoice
           (logcat-schoice-item :tag "Require that" :value !)
           (logcat-schoice-item :tag "Include when" :value +)
           (logcat-schoice-item :tag "Exclude when" :value -))
          (logcat-filter-condition)))

(define-widget 'logcat-filter-list 'editable-list
  "Widget for editing a list of logcat filters."
  :tag "Filters"
  :offset 0
  :entry-format "%i %d %v"
  :args '(logcat-filter-widget))

(defcustom logcat-fb-adb-program "fb-adb"
  "Location of the fb-adb executable.")

(defcustom logcat-fb-adb-arguments ""
  "Extra arguments (as a shell string) to pass to fb-adb.")

(defcustom logcat-follow-link-function 'logcat-follow-link-using-locate
  "Function used for following links.
It should take three arguments: the name of the file to find and
the line number (as an integer) in that file, and a string
containing fullname (package and function) information.  The line
number and fullname information may be `nil'."
  :type 'function)

(defcustom logcat-suggest-regular-expression t
  "Suggest regular expression match for message filter.
When this variable is non-nil, `logcat-filter-tag' and `logcat-filter-message',
generate a regular expression match instead of a string literal
match in the suggested filter rule."
  :type 'boolean)

(defcustom logcat-default-filters nil
  "Default filters for logcat buffers."
  :type '(logcat-filter-list
          :format "Default filters:\n%v%i\n"))

(defcustom logcat-display-style 'threadtime
  "Style in which to render log entries."
  :type `(radio
          ,@(cl-loop for (tag . function) in logcat-display-style-alist
               collect `(const ,tag))))

(defcustom logcat-default-fields logcat--fields
  "Fields to display by default in logcat buffers."
  :type `(set
          :greedy t
          ,@(cl-loop for field in logcat--fields
                 collect `(const ,field))))

(defvar logcat-read-filter-expression-history nil
  "History for `logcat-read-filter-expression'.")

(defvar-local logcat--handle-log-record-function nil
  "Function used for rendering log entries.
Do not set directly: use `logcat-set-display-style'.")

(defvar-local logcat--logcat-process nil
  "The logcat process object associated with the buffer.")

(defvar-local logcat--insert-record-function nil
  "Function to use to insert record in the buffer.")

(defvar-local logcat--filter-function nil
  "Current automatically-generated logcat filter function.")

(defvar-local logcat-filters nil
  "Filters that apply to logcat messages.")

(defvar logcat--used-fields)

(defvar-local logcat--filter-ring nil
  "Holds a history of filter lists.")

(defcustom logcat-filter-ring-size 64
  "Number of filter sets to keep in filter history."
  :type 'integer)

(defconst logcat--edit-filters-buffer-name "*logcat-filter-edit*")

(defconst logcat--java-exception-regexp
  (rx bol
      (+ whitespace)
      "at "
      (group-n 5 (+ (in "a-zA-Z0-9_.<>$")))
      "("
      (group-n 1
               (group-n 2 (+ (not (in ": \n\t\r\v"))))
               ":"
               (group-n 3 (+ (in "0-9"))))
      ")" eol)
  "Regular expression matching a Java file reference.
Match group 1 is the whole link; match group 2 is the filename;
match group 3 (optional) is the line number.  Match group
5 (optional) is the fullname and function name.")

(defconst logcat-priority-letters
  [?? ?Q ?V ?D ?I ?W ?E ?F ?S]
  "Array mapping logcat priority levels to single-letter names.")

(defconst logcat-edit-filters-intro-text
  "This buffer contains a list of rules logcat-mode uses to \
filter log messages.  If more than one rule matches a log message, \
the last one to match wins.")

(defface logcat-error
  '((t :inherit error))
  "Face used to highlight logcat error-level messages."
  :group 'logcat)

(defface logcat-warning
  '((t :inherit warning))
  "Face used to highlight logcat warning-level messages."
  :group 'logcat)

(defface logcat-info
  '((t :inherit success))
  "Face used to highlight logcat information-level messages."
  :group 'logcat)

(defface logcat-debug
  '((t :inherit default))
  "Face used to highlight logcat debug-level messages."
  :group 'logcat)

(defface logcat-verbose
  '((t :inherit default))
  "Face used to highlight logcat verbose-level messages."
  :group 'logcat)

(defvar logcat-choose-file-history nil
  "History of names chosen when following links.")

;; 1 date
;; 2 pid
;; 3 tid
;; 4 level
;; 5 facility
;; 6 message

(eval-and-compile
  (defconst logcat-date-rx
  '(: (= 2 (in "0-9")) "-" (= 2 (in "0-9"))
    " "
    (= 2 (in "0-9")) ":"
    (= 2 (in "0-9")) ":"
    (= 2 (in "0-9")) "." (= 3 (in "0-9"))))

  (defconst logcat-brief-format
    (rx bol
        (group-n 4 (in "VDIWEFS"))      ; level
        "/"
        (group-n 5 (*? any))            ; facility
        (0+ " ") "(" (0+ " ")
        (group-n 2 (1+ (in "0-9")))
        "): "
        (group-n 6 (0+ any))
        eol))

  (defconst logcat-process-format
    (rx bol
        (group-n 4 (in "VDIWEFS"))      ; level
        "(" (0+ " ")
        (group-n 2 (1+ (in "0-9")))     ; pid
        ") "
        (group-n 6 (0+ any))
        eol))

  (defconst logcat-tag-format
    (rx bol
        (group-n 4 (in "VDIWEFS"))      ; level
        "/"
        (group-n 5 (*? any))            ; facility
        (0+ " ") ":" (1+ " ")
        (group-n 6 (0+ any))
        eol))

  (defconst logcat-thread-format
    (rx bol
        (group-n 4 (in "VDIWEFS"))      ; level
        "(" (0+ " ")
        (group-n 2 (1+ (in "0-9")))     ; pid
        ":" (0+ " ")
        (group-n 3 (1+ (in "0-9")))     ; pid
        ") "
        (group-n 6 (0+ any))
        eol))

  (defconst logcat-time-format
    (rx bol
        (group-n 1 (eval logcat-date-rx))
        (1+ " ")
        (group-n 4 (in "VDIWEFS"))      ; level
        "/"
        (group-n 5 (*? any))            ; facility
        (0+ " ") "(" (0+ " ")
        (group-n 2 (1+ (in "0-9")))
        "): "
        (group-n 6 (0+ any))
        eol))

  (defconst logcat-threadtime-format
    (rx bol
        (group-n 1 (eval logcat-date-rx))
        (1+ " ")
        (group-n 2 (1+ (in "0-9")))     ; pid
        (1+ " ")
        (group-n 3 (1+ (in "0-9")))     ; tid
        (1+ " ")
        (group-n 4 (in "VDIWEFS"))      ; level
        (1+ " ")
        (group-n 5 (*? any))            ; facility
        (0+ " ") ":" (1+ " ")
        (group-n 6 (0+ any))
        eol)))

(defconst logcat-formats
  '(logcat-brief-format
    logcat-process-format
    logcat-tag-format
    logcat-thread-format
    logcat-time-format
    logcat-threadtime-format))

(defconst logcat-line-re
  (concat "\\(?:"
              (mapconcat #'symbol-value
                         logcat-formats
                         "\\|")

              "\\)"))

(defconst logcat-highlights
  '((1 font-lock-preprocessor-face nil t)
    (2 font-lock-string-face nil t)
    (3 font-lock-string-face nil t)
    (4 font-lock-keyword-face nil t)
    (5 font-lock-function-name-face nil t)))

(defmacro logcat--trace (fmt &rest args)
  (when nil
    `(with-current-buffer (get-buffer-create "*logcat-debug*")
       (goto-char (point-max))
       (insert (format ,(concat fmt "\n") ,@args)))))

(defun logcat-font-lock-matcher (limit)
  (while (re-search-forward logcat-line-re limit t)
    (mapc #'font-lock-apply-highlight logcat-highlights)
    (let ((level (and (match-beginning 4)
                      (save-excursion
                        (goto-char (match-beginning 4))
                        (char-after)))))
      (cond ((eq level ?E)
             (font-lock-apply-highlight '(6 'logcat-error)))
            ((eq level ?W)
             (font-lock-apply-highlight '(6 'logcat-warning)))
            ((eq level ?I)
             (font-lock-apply-highlight '(6 'logcat-info)))
            ((eq level ?D)
             (font-lock-apply-highlight '(6 'logcat-debug)))
            ((eq level ?V)
             (font-lock-apply-highlight '(6 'logcat-verbose)))))))

(defconst logcat-font-lock-keywords
  '((logcat-font-lock-matcher)))

(defconst logcat-font-lock-defaults
  '(logcat-font-lock-keywords t))

(defun logcat-record-at-point ()
  "Return the logcat record at point."
  (get-text-property (point) 'logcat-record))

(defun logcat-read-filter-expression (&optional initial-expression)
  (let ((expression
         (read-from-minibuffer
          "Filter: "
          (if initial-expression
              (with-output-to-string (prin1 initial-expression))
            "")
          nil t 'logcat-read-filter-expression-history)))
    ;; Validate expression by side effect
    (logcat--compile-filter expression)
    expression))

(defun logcat--call-with-preserved-apparent-positions (function)
  "Call FUNCTION and try not to be visually jarring.
Call FUNCTION while preserve the relationship between the point
and the window start for each window displaying the
current buffer.

If, however, the end of the buffer would be visible after calling
FUNCTION, make sure the end of the buffer is at the end of the
window, avoiding wasting of space."
  (let ((saved
         (cl-loop for window in (get-buffer-window-list nil nil t)
            collect (with-selected-window window
                      (let* ((column (current-column))
                             (lines-from-top
                              (save-excursion
                                (save-restriction
                                  (narrow-to-region (window-start) (point))
                                  (goto-char (point-min))
                                  (vertical-motion (point-max))))))
                        (logcat--trace "w:%S lt:%S" window lines-from-top)
                        (list window lines-from-top column))))))
    (unwind-protect
         (funcall function)
      (cl-loop for (window lines-from-top column) in saved
         do (when (window-live-p window)
              (with-selected-window window
                (save-excursion
                  (logcat--trace "Before vertical motion: at %S"
                                 (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                  (vertical-motion (- lines-from-top))
                  (logcat--trace "After vertical motion: at %S"
                                 (buffer-substring-no-properties (point-at-bol) (point-at-eol)))
                  (set-window-start (selected-window) (point))
                  (when (pos-visible-in-window-p (point-max))
                    (goto-char (point-max))
                    (recenter -1)))
                (move-to-column column)))))))

(defun logcat--handle-filter-command (field show-message &optional do-remove)
  (let* ((arg (if do-remove '- current-prefix-arg))
         (base-record (or (logcat-record-at-point)
                          (error "no logcat record")))
         (base-value (cl-ecase field
                       (pid (logcat-record-pid base-record))
                       (tid (logcat-record-tid base-record))
                       (priority
                        (intern
                         (format "%c"
                                 (aref logcat-priority-letters
                                       (logcat-record-priority base-record)))))
                       (tag (logcat-record-tag base-record))
                       (message (logcat-record-message base-record))))
         (subtract (or (eq arg '-)
                       (and (consp arg)
                            (integerp (car arg))
                            (> (car arg) 4))))
         (edit (not (memq arg '(nil -))))
         (op (cond
               ;; DWIM when the user talks about priority.
               ((eq field 'priority) (if subtract '<= '>=))
               ((and logcat-suggest-regular-expression
                     (memq field '(message tag)))
                '=~)
               (t '=)))
         (base-value (if (eq op '=~)
                         (concat "^" (regexp-quote base-value) "$")
                       base-value))
         (condition `(,op ,field ,base-value))
         (expr `(,(if subtract '- '!) ,condition))
         (expr (if edit (logcat-read-filter-expression expr) expr)))

    (logcat--call-with-preserved-apparent-positions
     (lambda ()
       (logcat--update-filters (append logcat-filters (list expr)))))
    (when show-message
      (message  "Added filter: %S" expr))))

(defun logcat-filter-pid ()
  "Add a PID filter to the end of the filter set.
By default, add a filter that shows only log messages from the
process that wrote the log entry under point.  With M-- prefix
argument, hide all messages from this process.  With C-u prefix
argument, read a filter expression that defaults to showing only
messages from this process.  With C-u C-u prefix, read a filter
expression that defaults to hiding messages from this process."
  (declare (interactive-only logcat-add-filter))
  (interactive)
  (logcat--handle-filter-command
   'pid
   (called-interactively-p 'interactive)))

(defun logcat-filter-tid ()
  "Add a TID filter to the end of the filter set.
By default, add a filter that shows only log messages from the
thread that wrote the log entry under point.  With M-- prefix
argument, hide all messages from this thread.  With C-u prefix
argument, read a filter expression that defaults to showing only
messages from this thread.  With C-u C-u prefix, read a filter
expression that defaults to hiding messages from this thread."
  (declare (interactive-only logcat-add-filter))
  (interactive)
  (logcat--handle-filter-command
   'tid
   (called-interactively-p 'interactive)))

(defun logcat-filter-priority ()
  "Add a priority filter to the end of the filter set.
By default, add a filter that shows only log messages with a
priority equal to or greater than the priority of the message
under point.  With M-- prefix argument, hide all messages with a
priority less than or equal to the priority of the message under
point.  With C-u prefix argument, read a filter expression that
defaults to showing only messages with priority equal to or
greater than the priority of the message under point.  With C-u
C-u prefix, read a filter expression that defaults to hiding
messages with priority less than or equal to message
under point."
  (declare (interactive-only logcat-add-filter))
  (interactive)
  (logcat--handle-filter-command
   'priority
   (called-interactively-p 'interactive)))

(defun logcat-filter-tag ()
  "Add a tag filter to the end of the filter set.
By default, add a filter that shows only log messages with a tag
equal to that of the message under point.  With M-- prefix
argument, hide all message with a tag equal to that of the
message under point.  With C-u prefix argument, read a filter
expression that defaults to showing only messages with a tag
equal to that of the message under point.  With C-u C-u prefix,
read a filter expression that defaults to hiding messages with
tag equal to that of the message under point.

If `logcat-suggest-regular-expression' is non-`nil', default to
using a regular expression match.  Otherwise, default to a
string match."
  (declare (interactive-only logcat-add-filter))
  (interactive)
  (logcat--handle-filter-command
   'tag
   (called-interactively-p 'interactive)))

(defun logcat-filter-message ()
  "Add a message filter to the end of the filter set.
By default, add a filter that shows only log messages with a
message equal to that of the message under point.  With M--
prefix argument, hide all message with a message equal to that of
the message under point.  With C-u prefix argument, read a filter
expression that defaults to showing only messages with a message
equal to that of the message under point.  With C-u C-u prefix,
read a filter expression that defaults to hiding messages with
message equal to that of the message under point.

If `logcat-suggest-regular-expression' is non-`nil', default to
using a regular expression match.  Otherwise, default to a
string match."
  (declare (interactive-only logcat-add-filter))
  (interactive)
  (logcat--handle-filter-command
   'message
   (called-interactively-p 'interactive)))

(defun logcat-add-filter (filter-expression)
  "Add FILTER-EXPRESSION to the end of the filter set."
  (interactive (list (logcat-read-filter-expression)))
  (logcat--call-with-preserved-apparent-positions
   (lambda ()
     (logcat--update-filters (append logcat-filters
                                     (list filter-expression)))))
  (when (called-interactively-p 'interactive)
    (message "Added filter specification.")))

(defun logcat-clear-filters ()
  "Remove all logcat filters."
  (interactive)
  (logcat--call-with-preserved-apparent-positions
   (lambda ()
     (logcat--update-filters nil)))
  (when (called-interactively-p 'interactive)
    (message "Cleared filters")))

(defvar logcat-mode-filter-map
  (let ((keymap (make-sparse-keymap "Filters")))
    (define-key keymap [?P]  '(menu-item "pid"     logcat-filter-pid))
    (define-key keymap [?T]  '(menu-item "tid"     logcat-filter-tid))
    (define-key keymap [?p]  '(menu-item "prio"    logcat-filter-priority))
    (define-key keymap [?t]  '(menu-item "tag"     logcat-filter-tag))
    (define-key keymap [?m]  '(menu-item "message" logcat-filter-message))
    (define-key keymap [?e]  '(menu-item "edit"    logcat-edit-filters))
    (define-key keymap [?a]  '(menu-item "add"     logcat-add-filter))
    (define-key keymap [(control c)] '(menu-item "clear" logcat-clear-filters))
    keymap)
  "Keymap for logcat-mode filters.")

(defun logcat--handle-display-command (field enable show-message)
  (let* ((ispec (logcat--field-to-ispec field))
         (make-visible (cond ((null enable)
                              (memq ispec buffer-invisibility-spec))
                             ((< enable 0) nil)
                             (t t))))
    (if make-visible
        (remove-from-invisibility-spec ispec)
      (add-to-invisibility-spec ispec))
    (redraw-display)
    (when show-message
      (message "%s %s" (if make-visible "Displaying" "Hiding") field))))

(defun logcat-display-date (&optional enable)
  "Show or hide the date portion of logcat messages.
If ENABLE is greater than or equal to zero, show the date.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'date
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-time (&optional enable)
  "Show or hide the time portion of logcat messages.
If ENABLE is greater than or equal to zero, show the time.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'time
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-pid (&optional enable)
  "Show or hide the PID portion of logcat messages.
If ENABLE is greater than or equal to zero, show the PID.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'pid
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-tid (&optional enable)
  "Show or hide the TID portion of logcat messages.
If ENABLE is greater than or equal to zero, show the TID.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'tid
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-priority (&optional enable)
  "Show or hide the priority portion of logcat messages.
If ENABLE is greater than or equal to zero, show the priority.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'priority
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-tag (&optional enable)
  "Show or hide the tag portion of logcat messages.
If ENABLE is greater than or equal to zero, show the tag.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'tag
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-message (&optional enable)
  "Show or hide the message portion of logcat messages.
If ENABLE is greater than or equal to zero, show the message.
If ENABLE is less than zero, hide it.  If ENABLE is nil, toggle."
  (interactive)
  (logcat--handle-display-command
   'message
   enable
   (called-interactively-p 'interactive)))

(defun logcat-display-all ()
  "Display all log fields."
  (interactive)
  (setf buffer-invisibility-spec '(logcat-filtered))
  (redraw-display))

(defun logcat-set-display-style (style &optional no-reformat)
  "Set the style in which `logcat-mode' will render log entries.
STYLE is a style name from `logcat-display-style-alist'.
If NO-REFORMAT is non-`nil' (or, interactively, if called with a
prefix argument), `logcat-set-display-style' does not reformat
existing entries."
  (interactive (list
                (intern (completing-read
                         "Logcat display style: "
                         (mapcar #'car logcat-display-style-alist)
                         nil
                         t))
                current-prefix-arg))
  (setf logcat--insert-record-function
        (or (cdr (assq style logcat-display-style-alist))
            (error "no logcat display style called %S" style)))
  (unless no-reformat
    (logcat--refresh-records t)))

(defvar logcat-mode-display-map
  (let ((keymap (make-sparse-keymap "Display")))
    (define-key keymap [?d]  '(menu-item "date"    logcat-display-date))
    (define-key keymap [?i]  '(menu-item "time"    logcat-display-time))
    (define-key keymap [?P]  '(menu-item "pid"     logcat-display-pid))
    (define-key keymap [?T]  '(menu-item "tid"     logcat-display-tid))
    (define-key keymap [?p]  '(menu-item "prio"    logcat-display-priority))
    (define-key keymap [?t]  '(menu-item "tag"     logcat-display-tag))
    (define-key keymap [?m]  '(menu-item "message" logcat-display-message))
    (define-key keymap [?a]  '(menu-item "all"     logcat-display-all))
    (define-key keymap [(control s)]
      '(menu-item "style" logcat-set-display-style))
    keymap))

(defun logcat--font-lock-fontify-buffer ()
  ;; Use Emacs 25 functions when available.
  (if (and (fboundp 'font-lock-flush)
           (fboundp 'font-lock-ensure))
      (progn
        (funcall 'font-lock-flush)
        (funcall 'font-lock-ensure))
    (with-no-warnings
      (font-lock-fontify-buffer))))

(defun logcat-log-info ()
  (interactive)
  (cond (mark-active
         (let ((start (point))
               (end (mark)))
           (when (< end start)
             (cl-psetf start end end start))
           (let* ((r-start (or (save-excursion
                                 (goto-char start)
                                 (logcat-record-at-point))
                               (error "no log record at %s"
                                      (if (eql start (point)) "point" "mark"))))
                  (r-end (or (save-excursion
                               (goto-char end)
                               (logcat-record-at-point))
                             (error "no log record at %s"
                                    (if (eql end (point)) "point" "mark"))))
                  (time-diff (time-subtract
                              (logcat-record-timestamp r-end)
                              (logcat-record-timestamp r-start))))
             (message "Time difference: %gms"
                      (* 1000 (float-time time-diff))))))
        (t
         (let ((record (save-excursion
                         (logcat--forward-invisible-character
                          most-positive-fixnum)
                         (logcat-record-at-point))))
           (unless record
             (error "no record at point"))
           (with-temp-buffer
             (logcat-mode)
             (logcat-insert-record-threadtime record)
             (logcat--font-lock-fontify-buffer)
             (message "%s" (buffer-substring (point-min) (1- (point-max)))))))))

(defun logcat--strip-invisible-text (text)
  (let ((length (length text)))
    (if (and (not (invisible-p (get-text-property 0 'invisible text)))
             (= (next-single-property-change 0 'invisible text length)
                length))
        text
      (let ((pos 0)
            (parts nil))
        (while (< pos length)
          (let ((start pos))
            (setf pos (next-single-property-change
                       pos 'invisible text length))
            (unless (invisible-p  (get-text-property start 'invisible text))
              (push (substring text start pos) parts))))
        (cond ((cdr parts)
               (apply #'concat (nreverse parts)))
              ((car parts))
              (t ""))))))

(defmacro logcat--with-buffer-modifications (&rest body)
  (declare (indent 0))
  `(let ((inhibit-read-only t))
     ,@body))

(defun logcat-flush-filtered-records ()
  "Delete log records currently hidden by filters."
  (interactive)
  (let ((nr-deleted 0))
    (logcat--with-buffer-modifications
      (save-restriction
        (widen)
        (let ((pos (point-min)))
          (while (not (eq pos (point-max)))
            (let* ((invisible (get-text-property pos 'invisible))
                   (start pos))
              (setf pos (next-single-property-change
                         pos 'invisible nil (point-max)))
              (when (eq invisible 'logcat-filtered)
                (delete-region start pos)
                (setf pos start)
                (cl-incf nr-deleted)))))))
    (message "Deleted %d filtered records" nr-deleted)))

(defun logcat-delete-output ()
  "Delete logcat output from the buffer."
  (interactive)
  (save-excursion
    (save-restriction)
    (widen)
    (logcat--with-buffer-modifications
      (delete-region (point-min) (point-max)))))

(defun logcat--field-menu-label (field)
  (cl-case field
    (pid "process ID")
    (tid "thread ID")
    (t (symbol-name field))))

(defun logcat--inside-record-p (pos)
  (save-restriction
    (widen)
    (and (not (eql pos (point-min)))
         (not (eql pos (point-max)))
         (eq (get-text-property (1- pos) 'logcat-record)
             (get-text-property pos 'logcat-record)))))

(defun logcat--check-region (beg end)
  (when (logcat--inside-record-p beg)
    (error "cannot delete partial region"))

  (when (logcat--inside-record-p end)
    (if (and (eql (char-after end) ?\n)
             (not (logcat--inside-record-p (1+ end))))
        (cl-incf end)
      (error "cannot delete partial region")))
  (cons beg end))

(defun logcat-delete-region (beg end)
  "Delete a region of the logcat buffer."
  (interactive "r")
  (cl-destructuring-bind (beg . end) (logcat--check-region beg end)
    (logcat--with-buffer-modifications
      (delete-region beg end))))

(defun logcat--buffer-substring-wrapper
    (orig-fn beg end &optional delete)
  (if delete
      (cl-destructuring-bind (beg . end) (logcat--check-region beg end)
        (logcat--with-buffer-modifications
          (funcall orig-fn beg end delete)))
    (funcall orig-fn beg end delete)))

(defvar logcat-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [(control c) (control f)] logcat-mode-filter-map)
    (define-key keymap [(control c) (control d)] logcat-mode-display-map)
    (define-key keymap [(control c) (control k)]
      'logcat-flush-filtered-records)
    (define-key keymap [remap delete-region] 'logcat-delete-region)
    (define-key keymap [remap undo] 'logcat-filter-undo)
    (define-key keymap [(control ?.)] 'logcat-log-info)
    (define-key keymap [(control c) (meta o)] 'logcat-delete-output)
    (easy-menu-define nil keymap "Logcat"
      `("Logcat"
        ["Start logcat" logcat-start
         :enable (not (logcat-running-p))]
        ["Stop logcat" logcat-stop
         :enable (logcat-running-p)]
        "----------------"
        ["Edit filters" logcat-edit-filters t]
        ["Delete filtered messages" logcat-flush-filtered-records t]
        ("Include only messages with"
         :enable (logcat-record-at-point)
        ,@(cl-loop
             for field in logcat--filterable-fields
             for label = (logcat--field-menu-label field)
             collect `[,(format "this %s" label)
                        (logcat--handle-filter-command ,field t nil)
                        t]))
        ("Exclude messages with"
         :enable (logcat-record-at-point)
        ,@(cl-loop
             for field in logcat--filterable-fields
             for label = (logcat--field-menu-label field)
             collect `[,(format "this %s" label)
                        (logcat--handle-filter-command ,field t t)
                        t]))
        ["Clear filters" logcat-clear-filters t]
        "----------------"
        ("Display fields"
         ,@(cl-loop
              for field in logcat--fields
              collect
                `[,(logcat--field-menu-label field)
                  ,(intern (format "logcat-display-%s" field))
                  :style toggle
                   :selected (logcat--field-displayed-p ',field)]))
        ("Style"
         ,@(cl-loop
              for (name . fn) in logcat-display-style-alist
              collect
                `[,(symbol-name name)
                  (logcat-set-display-style ',name)
                  :style radio
                   :selected (eq logcat--insert-record-function ',fn)]))
        "----------------"
        ["Settings..." (customize-group 'logcat) t]))
    keymap)
  "Keymap for logcat-mode.")

(define-derived-mode logcat-mode fundamental-mode "Logcat"
  "Major mode for viewing Android logcat logs.
You probably want to use the `logcat' command instead of running
`logcat-mode' directly.

This mode supports viewing and filtering log messages from
Android and Android-ish devices."
  (setf logcat-filters logcat-default-filters)
  (logcat-set-display-style logcat-display-style t)
  (setf logcat--filter-ring (make-ring logcat-filter-ring-size))
  (setf font-lock-defaults logcat-font-lock-defaults)
  (setf buffer-undo-list t)
  (setf truncate-lines t)
  (setq-local inhibit-point-motion-hooks t)
  (logcat--update-filters logcat-filters t)
  (setf buffer-invisibility-spec
        (append (mapcar 'logcat--field-to-ispec
                        (cl-set-difference logcat--fields
                                           logcat-default-fields))
                '(logcat-filtered)))
  (add-function :around (local 'filter-buffer-substring-function)
                'logcat--buffer-substring-wrapper)
  (add-function :filter-return (local 'filter-buffer-substring-function)
                'logcat--strip-invisible-text))

(defun logcat--read-u8 ()
  "Read and advance over a byte.
Return a fixnum."
  (prog1 (char-after)
    (forward-char)))

(defun logcat--read-u16 ()
  "Read and advance over a little-endian uint16_t.
Return a fixnum."
  (logior (lsh (logcat--read-u8) 0)
          (lsh (logcat--read-u8) 8)))

(defun logcat--read-u32 ()
  "Read and advance over a little-endian uint32_t.
Return a fixnum; on overflow, result is undefined."
  (logior (lsh (logcat--read-u8) 0)
          (lsh (logcat--read-u8) 8)
          (lsh (logcat--read-u8) 16)
          (lsh (logcat--read-u8) 24)))

(defun logcat--read-u32-as-float ()
  "Read and advance over a little-endian uint32_t.
Return a float, being careful not to overflow."
  (let* ((low (logcat--read-u16))
         (high (logcat--read-u16)))
    (+ low (* 65536.0 high))))

(defun logcat--read-sz (&optional limit)
  "Read and advance over a NUL-terminated string."
  (decode-coding-region
   (point)
   (or (and (search-forward "\000" limit t)
            (1- (point)))
       (goto-char (or limit (point-max))))
   ;; Use DOS so that we translate any stray CRLFs to LFs
   'utf-8-dos
   t))

(defun logcat--read-timestamp ()
  "Read a timestamp of u32 seconds, u32 nanoseconds.
Return a list (HIGH LOW USEC) as in `current-time'."
  (let* ((sec-low (logcat--read-u16))
         (sec-high (logcat--read-u16))
         (nsec-low (logcat--read-u16))
         (nsec-high (logcat--read-u16))
         ;; nsec is 1e9 seconds, so nsec-high has a maximum value of
         ;; floor(1e9/2^16) = 15258.  65536 / 1000 = 65.536.
         (usec (+ (* nsec-high 65)
                  (/ (* nsec-high 536) 1000)
                  (/ nsec-low 1000))))
    (list sec-high sec-low usec)))

(cl-defstruct logcat-record
  ;; logging process; fixnum
  pid
  ;; logging thread; fixnum
  tid
  ;; time since epoch; list of (HIGH LOW USEC)
  timestamp
  ;; priority; fixnum
  priority
  ;; log tag; string
  tag
  ;; log message; string
  message)

(defvar-local logcat--cached-header-length nil)

(defun logcat--read-packet ()
  (let* ((start (point))
         (payload-length (logcat--read-u16))
         ;; The header-length field was a padding field in old
         ;; versions of the protocol, and newer versions of Android
         ;; correctly zero-fill it when emitting the old protocol.
         ;; Old versions of Android leave this field filled with
         ;; garbage.  If we don't see a value in this field that makes
         ;; sense, assume we're looking at old-protocol records with
         ;; junk lengths and ignore the field for future records.
         (raw-header-length (let ((rv (logcat--read-u16)))
                              (or logcat--cached-header-length
                                  (setf logcat--cached-header-length
                                        (if (<= 20 rv 28) rv 0)))))
         (header-length
          (if (zerop raw-header-length) 20 raw-header-length))
         (payload-start (+ start header-length))
         (payload-end (+ payload-start payload-length)))
    (if (> payload-end (point-max))
        (signal 'end-of-buffer nil)
      (prog1 (make-logcat-record
              :pid (logcat--read-u32)
              :tid (logcat--read-u32)
              :timestamp (logcat--read-timestamp)
              ;; Start of payload portion of packet: skip to end of header if
              ;; we haven't read all of it already.
              :priority (progn
                          (goto-char payload-start)
                          (logcat--read-u8))
              :tag (logcat--read-sz payload-end)
              :message (logcat--read-sz payload-end))
        (goto-char payload-end)))))

(defun logcat--try-read-packet ()
  "Read and return a logcat binary packet at point.
On success, return t and leave point after packet.  If the packet
is incomplete, return nil."
  (let* ((pos (point)))
    (condition-case nil
        (logcat--read-packet)
      (end-of-buffer
       (goto-char pos)
       nil))))

(defun logcat--output-after-change (_beg _end _len)
  (let ((packet nil))
    (goto-char (point-min))
    (while (setf packet (logcat--try-read-packet))
      (funcall logcat--handle-log-record-function packet))
    (delete-region (point-min) (point))
    (goto-char (point-max))))

(defun logcat--extract-output-from-file (filename)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename)
    (logcat--chomp
     (decode-coding-region (point-min) (point-max) 'utf-8-dos t))))

(defun logcat--sentinel (process reason)
  (when (not (process-live-p process))
    (let* ((stderr-file (process-get process 'logcat-stderr-file))
           (stderr (and stderr-file
                        (ignore-errors
                          (logcat--extract-output-from-file stderr-file)))))
      (when stderr-file
        (delete-file stderr-file))
      (let ((buffer (process-buffer process)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when logcat--handle-log-record-function
              (ignore-errors
                (funcall logcat--handle-log-record-function
                         (list :dead
                               (format "%s: %s"
                                       (logcat--chomp reason) stderr))))))
          (kill-buffer buffer))))))

(defun logcat--fb-adb-determine-version ()
  (let* ((fb-adb (executable-find logcat-fb-adb-program))
         (content (shell-command-to-string (format "%s version" fb-adb))))
    (when (string-match "version \\([0-9]*\\).\\([0-9]*\\).\\([0-9]*\\)" content)
      (list (match-string-no-properties 1 content)
            (match-string-no-properties 2 content)
            (match-string-no-properties 3 content)))))

(defun logcat--fb-adb-get-versioned-commandline ()
  (let* ((version-list (logcat--fb-adb-determine-version))
         (adb-major-version (nth-value 0 version-list))
         (adb-minor-version (nth-value 1 version-list))
         (adb-patch-version (nth-value 2 version-list))
         (adb-logcat-base-subarg "/system/bin/logcat -B '*:V'"))
    (if (string-equal adb-major-version "1")
        adb-logcat-base-subarg
      (format "rcmd %s" adb-logcat-base-subarg)
      )
    )
  )

(defun logcat--start-process (exe extra-arguments handle-log-record)
  (let ((cmdline (concat "exec "
                         (shell-quote-argument exe)
                         " "
                         extra-arguments
                         (logcat--fb-adb-get-versioned-commandline)))
        (process nil)
        (buffer nil)
        (stderr-file nil))
    (unwind-protect
         (progn
           (setf stderr-file (make-temp-file "logcat.err."))
           (setf buffer (generate-new-buffer " logcat-process"))
           (setf process
                 (let ((process-connection-type nil))
                   (start-process-shell-command
                    "logcat"
                    nil
                    (format "%s </dev/null 2>%s"
                            cmdline
                            (shell-quote-argument stderr-file)))))
           (with-current-buffer buffer
             (set-buffer-multibyte nil)
             (setf buffer-undo-list t)
             (setq-local after-change-functions
                         (list 'logcat--output-after-change))
             (setf logcat--handle-log-record-function handle-log-record))
           (set-process-buffer process buffer)
           (set-process-coding-system process 'no-conversion 'no-conversion)
           (set-process-query-on-exit-flag process nil)
           (set-process-sentinel process 'logcat--sentinel)
           (process-put process 'logcat-stderr-file stderr-file)
           (setf stderr-file nil) ;; Ownership transferred
           ;; Success.
           (prog1
               process
             (setf buffer nil)
             (setf process nil)))
      (when stderr-file
        (delete-file stderr-file))
      (when buffer
        (kill-buffer buffer))
      (when process
        (kill-process process)))))

(defun logcat--cleanup ()
  (logcat-stop))

(defun logcat-stop ()
  (interactive)
  (when (and logcat--logcat-process
             (process-live-p logcat--logcat-process))
    (kill-process logcat--logcat-process)
    (while logcat--logcat-process
      (accept-process-output)))
  (setf logcat--logcat-process nil)
  (remove-hook 'kill-buffer-hook 'logcat--cleanup t)
  (remove-hook 'change-major-mode-hook 'logcat--cleanup t))

(defun logcat--copy-role-property-to-invisibile-property (start end)
  (while (< start end)
    (let* ((pos (next-single-property-change
                 start 'logcat-role nil end))
           (role (get-text-property start 'logcat-role)))
      (when role
        (put-text-property start pos 'invisible
                           (logcat--field-to-ispec role)))
      (setf start pos))))

(defun logcat--apply-text-properties (filtered start end)
  (put-text-property
   start
   end
   'invisible
   (if filtered 'logcat-filtered nil))
  (unless filtered
    (logcat--copy-role-property-to-invisibile-property start end)))

(defun logcat--query-locate-database (suffix)
  "Return the absolute filenames that end with SUFFIX."
  (with-temp-buffer
    (if (not (zerop (call-process "locate" nil t nil (concat "*/" suffix))))
        nil
      (let ((filenames nil))
        (goto-char (point-min))
        (while (re-search-forward "^/.+$" nil t)
          (push (match-string-no-properties 0) filenames))
        (nreverse filenames)))))

(defun logcat--choose-candidate (prompt candidates)
  (unless candidates
    (error "no candidates available"))
  (minibuffer-with-setup-hook 'minibuffer-complete
    (completing-read prompt
                     candidates
                     nil
                     t
                     (try-completion "" candidates)
                     'logcat-choose-file-history)))

(defconst logcat--package-isolation-regexp
  (rx
   bos
   (group-n
    1
    (: (+ (in "a-zA-Z0-9_$")))
    (* (: ?. (+ (in "a-zA-Z0-9_$")))))
   (: ?. (+ (in "a-zA-Z0-9_$")))
   (: ?. (+ (in "a-zA-Z0-9_<>$")))
   eos)
  "Regular expression that isolates a package name from a full method name.
Match group 1 contains the package.  For example, if we match
com.example.foo.Foo.<init>, match group 1 contains
com.example.foo.")

(defun logcat-follow-link-using-locate (filename &optional line fullname)
  "Follow a link to a file by its basename.
If there is any ambiguity, ask the user."
  (interactive "s")
  (let* ((basename (file-name-nondirectory filename))
         (candidates
          (unwind-protect
               (progn
                 (message "Searching for files...")
                 (logcat--query-locate-database basename))
            (message nil)))
         (candidates
          ;; Try filtering candidates using fullname information
          (or (and fullname
                   (string-match
                    logcat--package-isolation-regexp
                    fullname)
                   (cl-loop
                      with package = (match-string 1 fullname)
                      with prefix = (replace-regexp-in-string
                                     (rx ".") "/" package)
                      with match-re = (concat (regexp-quote prefix)
                                              "/"
                                              (regexp-quote basename)
                                              (rx eos))
                      for candidate in candidates
                      when (string-match match-re candidate)
                      collect candidate))
              ;; That didn't work: just use the original candidates
              ;; list
              candidates
              ;; No completions available
              (error "cannot find file matching %S" filename)))
         (file-to-visit (logcat--choose-candidate
                         "Visit file: "
                         candidates)))
    (with-current-buffer (find-file file-to-visit)
      (when line
        (with-no-warnings
          ;; goto-char is right: we want to treat this move as
          ;; interactive
          (goto-line line))))))

(defun logcat-follow-link (&optional pos)
  (interactive)
  (let* ((pos (or pos (point)))
         (filename (or (get-pos-property pos 'logcat-filename)
                       (error "no link at click location")))
         (line (get-pos-property pos 'logcat-line))
         (fullname (get-pos-property pos 'logcat-fullname)))
    (funcall logcat-follow-link-function filename line fullname)))

(defun logcat-follow-link-click (event)
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event))))
    (unless window
      (error "no window selected"))
    (with-current-buffer (window-buffer window)
      (logcat-follow-link pos))))

(defvar logcat-java-link-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [follow-link] 'logcat-follow-link-click)
    (define-key keymap [mouse-2] 'logcat-follow-link-click)
    (define-key keymap [?\r] 'logcat-follow-link)
    keymap)
  "Keymap used for links to source code.")

(defun logcat--apply-link-overlays (start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward logcat--java-exception-regexp end t)
      (let ((overlay (make-overlay
                      (match-beginning 1)
                      (match-end 1)
                      nil t))
            (filename (match-string-no-properties 2))
            (line (and (match-beginning 3)
                       (string-to-number
                        (match-string-no-properties 3)
                        10)))
            (fullname (match-string-no-properties 5)))
        (overlay-put overlay 'logcat-filename filename)
        (overlay-put overlay 'logcat-line line)
        (overlay-put overlay 'logcat-fullname fullname)
        (overlay-put overlay 'evaporate t)
        (overlay-put overlay 'face 'link)
        (overlay-put overlay 'mouse-face 'highlight)
        (overlay-put overlay 'help-echo "Visit link")
        (overlay-put overlay 'follow-link t)
        (overlay-put overlay 'keymap logcat-java-link-keymap)))))

(defun logcat--do-insert-record (record)
  (let* ((start (point))
         (end (progn (funcall logcat--insert-record-function record)
                     (point))))
    (put-text-property start end 'logcat-record record)
    (logcat--apply-text-properties
     (not (funcall logcat--filter-function record)) start end)
    (logcat--apply-link-overlays start end)))

(defun logcat--delete-whitespace-backward ()
  (when (memq (char-before) '(?\r ?\n))
    (delete-char -1)))

(defun logcat--chomp (string)
  (replace-regexp-in-string
   (rx (* (in " \t\v\r\n")) eos)
   ""
   (replace-regexp-in-string
    (rx bos (* any (in " \t\v\r\n")))
    ""
    string)))

(defconst logcat--scroll-history-interval 1.0
  "Number of seconds over which to maintain scroll history.")

(defconst logcat--scroll-immediate-threshold 10
  "Threshold for scrolling immediately.
Scroll immediately if the number of scroll events in the last
`logcat--scroll-history-interval' seconds is less than or equal
to this number.")

(defconst logcat--scroll-delay-maximum 0.5
  "Maximum number of seconds to delay scrolling.")

(defconst logcat--scroll-delay-increment
  ;; By default, smoothly ramp the delay up to
  ;; logcat--scroll-delay-maximum as we reach 500/entries per second.
  (/ (/ logcat--scroll-delay-maximum
        (max 1 (- 500 logcat--scroll-immediate-threshold)))
     logcat--scroll-history-interval)
  "Scroll delay to add for each recent scroll event.
For each scroll requested in the last
`logcat--scroll-history-interval' seconds in excess of
`logcat--scroll-immediate-threshold', add this many seconds to
the next scroll flush, up to a maximum of
`logcat--scroll-delay-maximum' seconds.")

(cl-defstruct logcat-scroll-record
  timestamp
  prev
  next)

(defvar-local logcat--windows-to-scroll nil
  "List of windows to scroll.")
(defvar-local logcat--scroll-timer nil
  "Timer that will eventually scroll the indicated windows.")
(defvar-local logcat--recent-scroll-times nil
  "List of timestamps of scroll requests.")
(defvar-local logcat--recent-scroll-count 0
  "Number of scroll events.")

(defun logcat--scroll-remove-record (record)
  (let ((prev (logcat-scroll-record-prev record))
        (next (logcat-scroll-record-next record)))
    (setf (logcat-scroll-record-next prev) next)
    (setf (logcat-scroll-record-prev next) prev)))

(defun logcat--scroll-prune-old-records (head bound)
  "Remove from scroll list HEAD all entries older than BOUND.
Return the number of entries removed."
  (let ((nr-pruned 0))
    (while (let ((oldest (logcat-scroll-record-prev head)))
             (when (and (not (eq oldest head))
                        (< (logcat-scroll-record-timestamp oldest) bound))
               (logcat--scroll-remove-record oldest)
               (cl-incf nr-pruned))))
    nr-pruned))

(defun logcat--scroll-insert-record (prev timestamp)
  (let* ((next (logcat-scroll-record-next prev))
         (record (make-logcat-scroll-record
                  :timestamp timestamp
                  :prev prev
                  :next next)))
    (setf (logcat-scroll-record-next prev) record)
    (setf (logcat-scroll-record-prev next) record)
    record))

(defun logcat--scroll-make-head ()
  (let ((head (make-logcat-scroll-record)))
    (setf (logcat-scroll-record-next head) head)
    (setf (logcat-scroll-record-prev head) head)
    head))

(defun logcat--count-recent-scrolls ()
  (let* ((now (float-time))
         (history (or logcat--recent-scroll-times
                      (setf logcat--recent-scroll-times
                            (logcat--scroll-make-head))))
         (nr-pruned (logcat--scroll-prune-old-records
                     history
                     (- now logcat--scroll-history-interval))))
    (logcat--scroll-insert-record history now)
    (setf logcat--recent-scroll-count
          (+ logcat--recent-scroll-count 1 (- nr-pruned)))))

(defun logcat--flush-scroll-queue-1 ()
  (logcat--trace "flushing scroll queue: %s windows" (length logcat--windows-to-scroll))
  (when logcat--scroll-timer
    (cancel-timer logcat--scroll-timer)
    (setf logcat--scroll-timer nil))
  (cl-loop
     with last-visible-position = (logcat--end-of-visible-buffer)
     with window-queue = (prog1 logcat--windows-to-scroll
                           (setf logcat--windows-to-scroll nil))
     for (window . mode) in window-queue
     when (and (window-live-p window)
               (eq (window-buffer window) (current-buffer)))
     do (with-selected-window window
          (goto-char
           (if (eq mode 'last-visible) last-visible-position (point-max)))
          (recenter (- -1 scroll-margin)))))

(defun logcat--flush-scroll-queue (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (derived-mode-p 'logcat-mode)
        (logcat--flush-scroll-queue-1)))))

(defun logcat--scroll-tracking-reset ()
  (kill-local-variable 'logcat--recent-scroll-times)
  (kill-local-variable 'logcat--recent-scroll-count))

(defun logcat--clamp (min val max)
  (max min (min val max)))

(defun logcat--adjust-windows-for-scroll (last-visible-position)
  (dolist (window (if mark-active nil
                    (get-buffer-window-list nil nil t)))
    (let ((wp (window-point window)))
      (when (<= last-visible-position wp)
        (unless (assq window logcat--windows-to-scroll)
          (push (cons window
                      (if (< wp (point-max)) 'last-visible 'point-max))
                logcat--windows-to-scroll)))))
  (when logcat--windows-to-scroll
    (let ((scroll-delay
           (let ((recent-scrolls (logcat--count-recent-scrolls)))
             (logcat--trace "recent-scrolls: %s" recent-scrolls)
             (logcat--clamp
              0
              (* (- recent-scrolls
                    logcat--scroll-immediate-threshold)
                 logcat--scroll-delay-increment)
              logcat--scroll-delay-maximum))))
      (logcat--trace "scroll-delay: %s" scroll-delay)
      (unless logcat--scroll-timer
        (setf logcat--scroll-timer
              (run-at-time scroll-delay
                           nil
                           (let ((buffer (current-buffer)))
                             (lambda ()
                               (logcat--flush-scroll-queue buffer)))))))))

(defun logcat--receive-log-record (record)
  "Insert a newly-available record into a logcat buffer."
  (logcat--with-buffer-modifications
    (let ((move-to-end nil)
          (last-visible-position (logcat--end-of-visible-buffer)))
      (save-excursion
        (save-restriction
          (widen)
          (logcat--adjust-windows-for-scroll last-visible-position)
          ;; If we're not being shown in any window, we still want to
          ;; move point if it's at apparent EOB: this way, when we
          ;; switch back to the buffer, we'll be at the end.
          (setf move-to-end
                (and (not (get-buffer-window-list nil nil t))
                     (cond ((< (point) last-visible-position) nil)
                           ((< (point) (point-max)) 'last-visible)
                           (t 'point-max))))
          (goto-char (point-max))
          (cond ((logcat-record-p record)
                 (logcat--do-insert-record record))
                ((eq (car-safe record) :dead)
                 (setf logcat--logcat-process nil)
                 (logcat--insert-threadtime-timestamp (current-time))
                 (insert (format "logcat died: %s" (cadr record)))
                 (logcat--delete-whitespace-backward)
                 (insert ?\n)))))
      (when move-to-end
        (goto-char
         (if (eq 'move-to-end 'last-visible)
             (logcat--end-of-visible-buffer)
           (point-max)))))))

(defun logcat-insert-as (role string &rest args)
  (insert (propertize (apply #'format string args) 'logcat-role role)))

(defun logcat--insert-threadtime-timestamp (timestamp)
  (cl-destructuring-bind
        (second minute hour day month _year _dow _dst _zone)
      (decode-time timestamp)
    (logcat-insert-as 'date "%02d-%02d " month day)
    (logcat-insert-as 'time "%02d:%02d:%02d.%03d "
                      hour minute second
                      (/ (cl-caddr timestamp) 1000))))

(defun logcat-insert-record-threadtime (record)
  "Format logcat records as MONTH-DAY H:M:S PID TID PRIO TAG: MESSAGE."
  (logcat--insert-threadtime-timestamp (logcat-record-timestamp record))
  (logcat-insert-as 'pid "%d " (logcat-record-pid record))
  (logcat-insert-as 'tid "%d " (logcat-record-tid record))
  (logcat-insert-as 'priority "%c "
                    (aref logcat-priority-letters
                          (logcat-record-priority record)))
  (logcat-insert-as 'tag "%s: " (logcat-record-tag record))
  (logcat-insert-as 'message "%s" (logcat-record-message record))
  (logcat--delete-whitespace-backward)
  (insert ?\n))

(defun logcat-insert-record-brief (record)
  "Format logcat records as PRIO/TAG(PID): message."
  (logcat-insert-as 'priority "%c/"
                    (aref logcat-priority-letters
                          (logcat-record-priority record)))
  (logcat-insert-as 'tag "%s" (logcat-record-tag record))
  (logcat-insert-as 'pid "(%d)" (logcat-record-pid record))
  (insert ": ")
  (logcat-insert-as 'message "%s" (logcat-record-message record))
  (logcat--delete-whitespace-backward)
  (insert ?\n))

(defun logcat--end-of-visible-buffer ()
  "Find the position after the last visible part of the buffer.
Does not widen the buffer."
  (let ((pos (point-max)))
    (while (cond ((eq pos (point-min)) nil)
                 ((invisible-p (1- pos))
                  (setf pos (previous-single-property-change
                             pos
                             'invisible
                             nil
                             (point-min))))))
    pos))

(defun logcat--forward-invisible-character (&optional N)
  "Move forward over most N invisible characters.
Return the number of characters moved.  If N is negative, move
backward.  N defaults to 1."
  (let ((start (point))
        (N (or N 1)))
    (cond ((>= N 0)
           (while (and (not (eobp))
                       (< (- (point) start) N)
                       (invisible-p (point)))
             (goto-char (next-single-property-change
                         (point)
                         'invisible nil (point-max))))
           (when (< N (- (point) start))
             (goto-char (+ start N))))
          (t
           (while (and (not (bobp))
                       (< N (- (point) start))
                       (invisible-p (1- (point))))
             (goto-char (previous-single-property-change
                         (point)
                         'invisible nil (point-min))))
           (when (< (- (point) start) N)
             (goto-char (+ start N)))))
    (- (point) start)))

(defun logcat--move-point-out-of-invisible-region-all-windows ()
  (logcat--map-buffer-windows (current-buffer)
    (lambda (window)
      (with-selected-window window
        (logcat--forward-invisible-character most-negative-fixnum)
        (when (bobp)
          (logcat--forward-invisible-character most-positive-fixnum))))))

(defun logcat--refresh-records (&optional reformat)
  (logcat--with-buffer-modifications
    (save-restriction
      (widen)
      (let ((pos (point-min)))
        (while (not (eq pos (point-max)))
          (let* ((record (get-text-property pos 'logcat-record))
                 (start pos))
            (setf pos (next-single-property-change
                       pos 'logcat-record nil (point-max)))
            (when record
              (cond (reformat
                     (let ((inhibit-quit t))
                       (delete-region start pos)
                       (save-excursion
                         (goto-char start)
                         (logcat--do-insert-record record)
                         (setf pos (point)))))
                    (t
                     (let ((should-filter
                            (not (funcall logcat--filter-function record)))
                           (is-filtered
                            (eq (get-text-property start 'invisible)
                                'logcat-filtered)))
                       (unless (eq should-filter is-filtered)
                         (let ((inhibit-quit t))
                           (logcat--apply-text-properties
                            should-filter start pos)))))))))))))

(defun logcat--maybe-read-extra-arguments ()
  (when current-prefix-arg
    (read-string "Extra arguments for fb-adb: "
                 logcat-fb-adb-arguments
                 'logcat-fb-adb-arguments-history)))

(defun logcat-start (&optional extra-arguments)
  "Start a logcat process in the current buffer.
If a logcat process is running, kill it first.  With prefix, ask
for extra logcat command line arguments."
  (interactive (list (logcat--maybe-read-extra-arguments)))
  (unless (derived-mode-p 'logcat-mode)
    (error "logcat-start called in non-logcat buffer"))
  (logcat-stop)

  (let ((fb-adb (executable-find logcat-fb-adb-program)))
    (unless fb-adb
      (error "fb-adb program %S not found; you may need to install it"
             logcat-fb-adb-program))
    (add-hook 'change-major-mode-hook 'logcat--cleanup t t)
    (add-hook 'kill-buffer-hook 'logcat--cleanup t t)
    (setf logcat--logcat-process
          (logcat--start-process
           fb-adb
           (or extra-arguments logcat-fb-adb-arguments)
           (let ((buffer (current-buffer)))
             (lambda (record)
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (logcat--receive-log-record record)))))))))

(defun logcat-running-p ()
  "Return whether there is a live logcat attached to this buffer."
  (and logcat--logcat-process
       (process-live-p logcat--logcat-process)))

(defvar-local logcat--edit-filters-target-buffer nil
  "In logcat-edit-filters-mode, the buffer to which we apply filters.")

(defvar-local logcat--edit-filters-widget nil
  "In logcat-edit-filters-mode, the main editing widget.")

(defun logcat--compile-filter-term (term)
  (cond ((stringp term) (cons 'string term))
        ((integerp term) (cons 'integer term))
        ((memq term '(priority pid tid))
         (cl-pushnew term logcat--used-fields)
         (cons 'integer term))
        ((memq term '(tag message))
         (cl-pushnew term logcat--used-fields)
         (cons 'string term))
        ((memq term '(V D I W E F))
         (cons 'integer
               (cl-position (aref (symbol-name term) 0)
                            logcat-priority-letters)))
        (t (error "invalid filter term %S" term))))

(defun logcat--check-regexp (regexp)
  "Check that REGEXP is a valid regular expression.
Signal an error if it is not."
  (string-match regexp ""))

(defun logcat--compile-filter-condition (filter-condition)
  "Make a Lisp expression that evaluates FILTER-CONDITION."
  (pcase filter-condition
    (`(and . ,conditions)
      `(and ,@(mapcar #'logcat--compile-filter-condition conditions)))
    (`(or . ,conditions)
      `(or ,@(mapcar #'logcat--compile-filter-condition conditions)))
    (`(not ,condition)
      `(not ,(logcat--compile-filter-condition condition)))
    (`(=~ ,record-field ,regexp)
      (unless (memq record-field '(tag message))
        (error "cannot regexp-match field %S" record-field))
      (logcat--check-regexp regexp)
      (cl-pushnew record-field logcat--used-fields)
      `(string-match ,regexp ,record-field))
    (`(,(and comparison (or `< `> `<= `>= `=))
        ,term1 ,term2)
      (let* ((pt1 (logcat--compile-filter-term term1))
             (t1-type (car pt1))
             (t1-value (cdr pt1))
             (pt2 (logcat--compile-filter-term term2))
             (t2-type (car pt2))
             (t2-value (cdr pt2)))
        (unless (eq t1-type t2-type)
          (error (concat "invalid filter condition: "
                         "%S has type %S, but %S has type %S")
                 term1 t1-type
                 term2 t2-type))
        (cond ((memq comparison '(< > <= >=))
               (unless (eq t1-type 'integer)
                 (error
                  (concat "invalid filter condition %S: "
                          "only integers can be used with `<` or `>`")
                  filter-condition))
               `(,comparison ,t1-value ,t2-value))
              (t
               `(equal ,t1-value ,t2-value)))))
    (`(,(or `funcall `pred `predicate) ,function)
      `(funcall (function ,function)))
    (_ (error "invalid filter condition %S" filter-condition))))

(defun logcat--compile-filter (filter-form)
  "Build a Lisp expression for a single FILTER-FORM.
The generated expression assumes it is being evaluated inside the
function that `logcat--compile-filters' builds."
  (pcase filter-form
    (`(+ ,condition)
      `(when ,(logcat--compile-filter-condition condition)
         (setf result t)))
    (`(- ,condition)
      `(when ,(logcat--compile-filter-condition condition)
         (setf result nil)))
    (`(! ,condition)
      `(unless ,(logcat--compile-filter-condition condition)
         (setf result nil)))
    (_ (error "unrecognized filter %S" filter-form))))

(defun logcat--compile-filters (filter-forms)
  "Build a Lisp function that evaluates FILTER-FORMS for a record."
  (let* ((logcat--used-fields nil)
         (body (mapcar 'logcat--compile-filter filter-forms))
         (lexical-binding t))
    (byte-compile
     `(lambda (record)
        (identity record) ;; Shut up byte compiler
        (let ((result t)
              ,@(cl-loop
                   for field in logcat--used-fields
                   for getter = (intern (format "logcat-record-%s" field))
                   collect `(,field (,getter record))))
          ,@body
          result)))))

(defun logcat--update-filters (filter-forms &optional noadd)
  (cl-assert (derived-mode-p 'logcat-mode))
  (let ((filter-function (logcat--compile-filters filter-forms)))
    (setf logcat--filter-function filter-function)
    (unless noadd
      (ring-insert logcat--filter-ring logcat-filters))
    (setf logcat-filters filter-forms)
    (logcat--refresh-records)
    (logcat--move-point-out-of-invisible-region-all-windows)
    (setf mode-line-process
          (if logcat-filters
              ":filtered"
            nil))))

(defun logcat-edit-filters-apply ()
  "Apply the logcat filters in this buffer."
  (interactive)
  (unless (derived-mode-p 'logcat-edit-filters-mode)
    "Not in a logcat filter editing buffer")
  (unless (and logcat--edit-filters-target-buffer
               (buffer-live-p logcat--edit-filters-target-buffer))
    (error "Original logcat buffer is dead"))
  (let ((value (widget-value logcat--edit-filters-widget)))
    (with-current-buffer logcat--edit-filters-target-buffer
      (logcat--update-filters value))))

(defvar logcat-edit-filters-mode-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap widget-keymap)
    (define-key keymap [(control c) (control c)] 'logcat-edit-filters-apply)
    keymap)
  "Keymap for `logcat-edit-filters-mode`.")

(defun logcat--edit-filters-revert-buffer
    (&optional _ignore-auto _noconfirm)
  (let ((logcat-buffer logcat--edit-filters-target-buffer))
    (unless (buffer-live-p logcat-buffer)
      (error "Original logcat buffer is dead"))
    (logcat--with-buffer-modifications
      (remove-overlays)
      (delete-region (point-min) (point-max))
      (insert logcat-edit-filters-intro-text)
      (fill-region (point-min) (point-max))
      (insert "\n\n"))
    (setf logcat--edit-filters-widget (widget-create 'logcat-filter-list))
    (widget-value-set logcat--edit-filters-widget
                      (with-current-buffer logcat-buffer logcat-filters))
    (insert "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest _ign) (logcat-edit-filters-apply))
     "Apply")
    (insert "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest _ign)
               (customize-variable 'logcat-default-filters))
     "Edit defaults")
    (insert "\n")
    (widget-create
     'push-button
     :notify (lambda (&rest _ign)
               (widget-value-set
                logcat--edit-filters-widget
                logcat-default-filters))
     "Load defaults")

    (widget-setup)))

(define-derived-mode logcat-edit-filters-mode
    fundamental-mode
    "logedit-edit-filters"
  "Major mode for editing logcat filters."
  (setq-local revert-buffer-function 'logcat--edit-filters-revert-buffer))

(defun logcat--make-edit-filters-buffer ()
  (let* ((logcat-buffer (current-buffer))
         (edit-buffer (get-buffer logcat--edit-filters-buffer-name)))
    (unless (and edit-buffer
                 (with-current-buffer edit-buffer
                   (eq logcat--edit-filters-target-buffer logcat-buffer)))
      (when edit-buffer
        (kill-buffer edit-buffer))
      (setf edit-buffer
            (get-buffer-create
             logcat--edit-filters-buffer-name))
      (with-current-buffer edit-buffer
        (logcat-edit-filters-mode)
        (setf logcat--edit-filters-target-buffer logcat-buffer)
        (revert-buffer)
        (setf buffer-undo-list nil)))
    edit-buffer))

(defun logcat-edit-filters ()
  "Edit logcat filters in a separate buffer."
  (interactive)
  (unless (derived-mode-p 'logcat-mode)
    (error "not in logcat mode"))
  (pop-to-buffer (logcat--make-edit-filters-buffer)))

(defun logcat-filter-undo ()
  "Restore the previous filter configuration."
  (interactive)
  (logcat--call-with-preserved-apparent-positions
   (lambda ()
     (logcat--update-filters
      (ring-remove logcat--filter-ring 0)
      t)))
  (when (called-interactively-p 'interactive)
    (message "Restored previous filter configuration")))

(defun logcat--map-buffer-windows (buffer function)
  "Call FUNCTION in each window displaying BUFFER."
  (declare (indent 1))
  (mapc function (get-buffer-window-list buffer nil t)))

;;;###autoload
(defun logcat (&optional extra-arguments)
  "Run logcat.
With prefix, prompt for extra arguments to give to `fb-adb'."
  (interactive (list (logcat--maybe-read-extra-arguments)))
  (let* ((buffer (get-buffer "*logcat*"))
         (created nil))
    (unless buffer
      (setf created t)
      (setf buffer (get-buffer-create "*logcat*")))
    (unwind-protect
         (with-current-buffer buffer
           (unless (derived-mode-p 'logcat-mode)
             (logcat-mode))
           (unless (logcat-running-p)
             (logcat-start extra-arguments))
           (read-only-mode 1)
           (goto-char (point-max))
           (display-buffer buffer)
           ;; (pop-to-buffer buffer)
           (setf buffer nil))
      (when (and created buffer)
        (kill-buffer buffer)))))

(push 'logcat-role yank-excluded-properties)
(push 'logcat-record yank-excluded-properties)

(provide 'logcat)

;; logcat.el ends here
