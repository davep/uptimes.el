;;; uptimes.el --- Track and display Emacs session uptimes.  -*- lexical-binding: t; -*-
;; Copyright 1999-2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 3.8
;; Keywords: processes, uptime
;; URL: https://github.com/davep/uptimes.el
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; uptimes.el provides a simple system for tracking and displaying the
;; uptimes of your Emacs sessions. Simply loading uptimes.el from your
;; ~/.emacs file will start the tracking of any session.
;;
;; The latest version of uptimes.el can be found at:
;;
;;   <URL:https://github.com/davep/uptimes.el>

;;; Thanks:
;;
;; Istvan Marko <imarko@pacificnet.net> for pointing out that a default of
;; one second for `uptimes-auto-save-interval' was probably a little OTT.
;;
;; Doug Elias <dme7@cornell.edu> for pointing out that midnight.el is a
;; recent addition to emacs.
;;
;; Nix <nix@esperi.demon.co.uk> for pointing out that some XEmacs users
;; might need `inhibit-clash-detection' set to t at points in this code.

;;; Code:

;; Bits that we need.

(eval-when-compile
  (require 'cl-lib))
(require 'pp)
(require 'timer)

;; Customize options.

(defgroup uptimes nil
  "Track Emacs session uptimes."
  :group 'games
  :prefix "uptimes-")

(defcustom uptimes-database (locate-user-emacs-file ".uptimes.el" "~/.emacs-uptimes")
  "Database of uptimes."
  :type  'file
  :group 'uptimes)

(defcustom uptimes-keep-count 10
  "Number of uptimes to keep."
  :type  'integer
  :group 'uptimes)

(defcustom uptimes-auto-save t
  "Should we auto-save our uptime data?"
  :type  '(choice (const :tag "Yes, auto-save uptime details" t)
                  (const :tag "No, don't auto-save details" nil))
  :group 'uptimes)

(defcustom uptimes-auto-save-interval 300
  "How often, in seconds, should we auto-save the data?"
  :type  'integer
  :group 'uptimes)

(defface uptimes-header-face
  '((default :inherit default))
  "Face for header (above a list of uptimes) in the `uptimes' buffer."
  :group 'uptimes)

(defface uptimes-headings-face
  '((default :inherit font-lock-comment-face))
  "Face for headings (after the header and before the uptimes) in the `uptimes' buffer."
  :group 'uptimes)

(defface uptimes-current-uptime-face
  '((default :inherit font-lock-variable-name-face))
  "Face used when the uptime of the current Emacs is shown in the `uptimes' buffer."
  :group 'uptimes)

(defface uptimes-other-uptime-face
  '((default :inherit font-lock-constant-face))
  "Face used when an uptime (other than for the current Emacs) is shown in the `uptimes' buffer."
  :group 'uptimes)

(defvar uptimes-headings-string
  (concat "\n\nBoot                Endtime             Uptime       This emacs\n"
          "=================== =================== ============ ==========\n")
  "String of column headings in an `uptimes' buffer.
This is shown after the header, and before the list of uptimes above each list.
The face `uptimes-headings-face' is used.")

(defvar uptimes-current-uptime-indicator "<--"
  "String to indicate the uptime of the current Emacs in the `'uptimes' buffer.")

(defvar uptimes-show-last-uptimes-first t
  "Whether to show the list of recent `uptimes' before top uptimes.
Set to false to put the list of top uptimes first.")

(defvar uptimes-show-booted-first t
  "Whether to show uptimes' boot and end timestamps before their duration.
Set to false to show the duration of the uptime first.")

(defun uptimes-default-duration-formatter (days hours mins secs)
  "Default way of formating the duration of an uptime.
DAYS HOURS MINS and SECS are duration to be formatted.
Rather than changing this function, set
`uptimes-duration-formatter' to a function of your choice."
  (format "%12s " (format "%d.%02d:%02d:%02d" days hours mins secs)))

(defvar uptimes-duration-formatter #'uptimes-default-duration-formatter
  "Function used by `uptimes' to format the duration of an uptime.
The value should be a function that takes four arguments DAYS
HOURS MINS and SECS and returns a string.")

(defun uptimes-default-timestamp-formatter (time)
  "Default way of formatting an uptimes timestamp TIME.
TIME is a float.
Rather than changing this function, set
`uptimes-timestamp-formatter' to a function of your choice."
  (format "%19s " (format-time-string "%Y-%m-%d %T" (uptimes-time-float time))))

(defvar uptimes-timestamp-formatter #'uptimes-default-timestamp-formatter
  "Function used by `uptimes' to format booted or ended timestamps.
The value should be a function that takes a FLOAT and returns a string")


;; The following functions are borrowed from midnight.el. I've made copies
;; here for two reasons. First, older versions of emacs don't have
;; midnight.el, second, (require 'midnight) has side-effects that some
;; people might not want. Somehow, this "cut-n-paste" method of coding
;; doesn't quite seem right, perhaps I'm missing something?

(defun uptimes-float-time (&optional tm)
  "Convert `current-time' (or TM) to a float number of seconds."
  (cl-multiple-value-bind (s0 s1 s2) (or tm (current-time))
    (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2))))

(defun uptimes-time-float (num)
  "Convert NUM (float seconds since epoch) to a list of 3 integers."
  (let* ((div (ash 1 16))
         (1st (floor num div)))
    (list 1st (floor (- num (* (float div) 1st)))
          (round (* 10000000 (mod num 1))))))

;; Non-customize variables.

(defvar uptimes-boottime (uptimes-float-time before-init-time)
  "The time that uptimes.el came into existence.

Normaly populated from `before-init-time'.")

(defvar uptimes-last-n nil
  "Last `uptimes-keep-count' uptimes.")

(defvar uptimes-top-n nil
  "Top `uptimes-keep-count' uptimes.")

(defvar uptimes-auto-save-timer nil
  "Timer object for the auto-saver.

Note that the timer object isn't used in the uptime code but this variable
is probided so that you can kill/restart the timer in your own code.")

;; Main code.

(cl-defun uptimes-key (&optional (boottime uptimes-boottime))
  "Return an `assoc' key for the given BOOTTIME.

If not supplied BOOTTIME defaults to `uptimes-boottime'."
  (format "%.7f" boottime))

(cl-defun uptimes-uptime (&optional (boottime uptimes-boottime)
                                    (endtime (uptimes-float-time)))
  "Return the uptime of BOOTTIME at ENDTIME."
  (- endtime boottime))

(cl-defun uptimes-uptime-values (&optional (boottime uptimes-boottime)
                                           (endtime (uptimes-float-time)))
  "Get the different parts of an uptime.

BOOTTIME is an optional boot-time for an Emacs process, if not supplied the
default is the boot-time of the current process. ENDTIME is the optional
time at which the Emacs process closed down, if not supplied the default is
the current time.

The result is returned as the following `list':

  (DAYS HOURS MINS SECS)"
  (let* ((now   (uptimes-uptime boottime endtime))
         (days  (floor (/ now 86400)))
         (hours (progn (cl-decf now (* days  86400)) (floor (/ now 3600))))
         (mins  (progn (cl-decf now (* hours 3600))  (floor (/ now 60))))
         (secs  (progn (cl-decf now (* mins  60))    (floor now))))
    (list days hours mins secs)))

(defun uptimes-with-face (face string)
  "Return STRING decorated with face FACE."
  (propertize string 'face face))

(defun uptimes-read-uptimes ()
  "Read the uptimes database into `uptimes-last-n' and `uptimes-top-n'."
  (when (file-exists-p uptimes-database)
    (with-temp-buffer
      (let ((create-lockfiles nil))      ; For the benefit of GNU emacs.
        (insert-file-contents uptimes-database t))
      (setq uptimes-last-n (read (current-buffer)))
      (setq uptimes-top-n  (read (current-buffer))))))

(defun uptimes-update ()
  "Update `uptimes-last-n' and `uptimes-top-n'."
  (uptimes-read-uptimes)
  ;; Yes, I know cl-flet* would make more sense here; this is what I used to
  ;; use here. However: https://github.com/davep/uptimes.el/issues/2
  (cl-flet ((trunc (list &optional (where uptimes-keep-count))
                    (let ((trunc-point (nthcdr (1- where) list)))
                      (when (consp trunc-point)
                        (setf (cdr trunc-point) nil)))
                    list))
    (cl-flet ((update (list now sort-pred)
                (let* ((key  (uptimes-key))
                       (this (cdr (assoc key list))))
                  (unless this
                    (setq this (cons uptimes-boottime nil))
                    (push (cons key this) list))
                       (setf (cdr this) now)
                       (trunc (sort list sort-pred)))))
      (let ((now (uptimes-float-time)))
        (setq uptimes-last-n
              (update uptimes-last-n now
                      (lambda (x y) (> (cddr x) (cddr y)))))
        (setq uptimes-top-n
              (update uptimes-top-n now
                      (lambda (x y)
                        (> (uptimes-uptime (cadr x) (cddr x))
                           (uptimes-uptime (cadr y) (cddr y))))))))))

;;;###autoload
(defun uptimes-save ()
  "Write the uptimes to `uptimes-database'."
  (interactive)
  (unless noninteractive
    (uptimes-update)
    (with-temp-buffer
      (let ((standard-output (current-buffer))
            (print-length nil))
        (pp uptimes-last-n)
        (pp uptimes-top-n)
        ;; TODO: What is the correct method of ignoring a lock error (IOW,
        ;; don't bother trying to write if there is a locking problem)?
        (let ((create-lockfiles nil))     ; For the benefit of GNU emacs.
          (write-region (point-min) (point-max) uptimes-database nil 0))))))

(defun uptimes-print-uptimes (header list)
  "Print HEADER and then all uptimes in list LIST to the current buffer.
The format is as follows:

First HEADER is shown in face `uptimes-header-face'.
HEADER is a format string, such as \"Top %d uptimes\" in which %d
which will be replaced by the number of uptimes in LIST.

Then the string `uptimes-headings-string' is shown in
`uptimes-headings-face'.

Then each uptime is shown on a single line as: BOOTED ENDED DURATION
- if `uptimes-show-booted-first' is nil then the duration
   is shown before the booted/ended
- The current uptime has `uptimes-current-uptime-indicator' appended.
- The whole line is shown using `uptimes-current-uptime-face' and
   `uptimes-other-uptimes-face'.

The booted/ended timestamps are formatted using the function set
in `uptimes-timestamp-formatter' and the duration is formatted using
the function set in `uptimes-duration-formatter'."
  (insert
   (uptimes-with-face
    'uptimes-header-face
    (format header (length list)))
   (uptimes-with-face 'uptimes-headings-face
                      uptimes-headings-string))
   (cl-loop for uptime in list
            for bootsig  = (car  uptime)
            for booted   = (cadr uptime)
            for ended    = (cddr uptime)
            for current-uptime = (string= bootsig (uptimes-key))
            for booted-and-ended-string = (concat (funcall uptimes-timestamp-formatter booted)
                                                  (funcall uptimes-timestamp-formatter ended))
            for duration-string = (cl-multiple-value-bind (days hours mins secs)
                                      (uptimes-uptime-values booted ended)
                                    (funcall uptimes-duration-formatter days hours mins secs))
            do
            (insert
             (uptimes-with-face
              (if current-uptime 'uptimes-current-uptime-face 'uptimes-other-uptime-face)
              (concat (when uptimes-show-booted-first booted-and-ended-string)
                      duration-string
                      (unless uptimes-show-booted-first booted-and-ended-string)
                      (if current-uptime uptimes-current-uptime-indicator "")
                      "\n"))))
   (insert "\n"))

;;;###autoload
(defun uptimes ()
  "Display the last and top `uptimes-keep-count' uptimes.
A buffer *uptimes* is created - pressing g will refresh the list.
The uptimes are also saved using `uptimes-save'.

Set `uptimes-show-last-uptimes-first' to nil to swap the order of the lists.
See `uptimes-print-uptime' for other ways to customize the output."
  (interactive)
  (uptimes-save)
  (with-output-to-temp-buffer "*uptimes*"
    (with-current-buffer "*uptimes*"
      (when uptimes-show-last-uptimes-first
        (uptimes-print-uptimes "Last %d uptimes" uptimes-last-n))
      (uptimes-print-uptimes "Top %d uptimes" uptimes-top-n)
      (unless uptimes-show-last-uptimes-first
        (uptimes-print-uptimes "Last %d uptimes" uptimes-last-n))
      (setq-local revert-buffer-function (lambda (&rest _ignored) (uptimes)))))) ; g refreshes buffer

;;;###autoload
(defun uptimes-current ()
  "Display the uptime for the current Emacs session."
  (interactive)
  (uptimes-save)
  (message "Emacs has been up and running for %s"
           (format-seconds
            "%Y, %D, %H, %M and %z%S"
            (- (uptimes-float-time) uptimes-boottime))))

;; Register our presence and, if `uptimes-auto-save' is true, kick off the
;; auto-save process.
(progn
  (when uptimes-auto-save
    (setq uptimes-auto-save-timer
          (run-at-time nil uptimes-auto-save-interval #'uptimes-save)))
  (add-hook 'kill-emacs-hook #'uptimes-save))

(provide 'uptimes)

;;; uptimes.el ends here
