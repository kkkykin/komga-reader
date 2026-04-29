;;; komga-reader-backend.el --- Backend abstraction layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Backend abstraction for komga-reader.
;; Implementations must register themselves by setting `komga-reader-backend-impl'.
;; All backend functions are asynchronous and accept a callback.

;;; Code:

(defcustom komga-reader-debug nil
  "When non-nil, enable debug logging for komga-reader."
  :type 'boolean
  :group 'komga-reader)

(defcustom komga-reader-curl-extra-args nil
  "Extra arguments passed to curl for every HTTP request.
Each element should be a string.  For example, to use a proxy:
  (\"-x\" \"http://127.0.0.1:8080\")
Or to ignore SSL certificate errors:
  (\"--insecure\")"
  :type '(repeat string)
  :group 'komga-reader)

(defvar komga-reader-backend-impl nil
  "Plist registering backend functions.
Keys: :list-books :get-manifest :get-chapter
:get-progression :update-progression.
Each value is a function.")

(defun komga-reader--debug-log (format-string &rest args)
  "Log a debug message when `komga-reader-debug' is non-nil.
FORMAT-STRING and ARGS are passed to `message'."
  (when komga-reader-debug
    (apply #'message (concat "[komga-reader] " format-string) args)))

(defun komga-reader--curl (method url callback &optional headers body)
  "Call curl with METHOD, URL, optional HEADERS alist and BODY string.
When finished, call CALLBACK with (STATUS-CODE BODY-STRING)."
  (let* ((args (append (list "-s" "-w" "\nHTTP_CODE:%{http_code}" "-X" method)
                       komga-reader-curl-extra-args
                       (list url)))
         (coding-system-for-read 'utf-8)
         (output ""))
    (dolist (h headers)
      (setq args (append args (list "-H" (concat (car h) ": " (cdr h))))))
    (when body
      (setq args (append args (list "-H" "Content-Type: application/json" "-d" body))))
    (komga-reader--debug-log "curl %s %s (extra-args: %S)" method url komga-reader-curl-extra-args)
    (make-process
     :name "komga-reader-curl"
     :command (cons "curl" args)
     :connection-type 'pipe
     :filter (lambda (_proc chunk)
               (setq output (concat output chunk)))
     :sentinel (lambda (proc _event)
                 (when (eq (process-status proc) 'exit)
                   (if (/= (process-exit-status proc) 0)
                       (progn
                         (komga-reader--debug-log "curl failed: exit %d" (process-exit-status proc))
                         (funcall callback 0 ""))
                     (let* ((code-match (string-match "\nHTTP_CODE:\\([0-9]+\\)\n?$" output))
                            (code (if code-match
                                      (string-to-number (match-string 1 output))
                                    0))
                            (body (if code-match
                                      (substring output 0 (match-beginning 0))
                                    output)))
                       (komga-reader--debug-log "curl response: HTTP %d (body %d bytes)" code (length body))
                       (funcall callback code body))))))))

(defun komga-reader-list-books (callback &optional query page size)
  (funcall (or (plist-get komga-reader-backend-impl :list-books)
               (error "No backend registered"))
           callback query page size))

(defun komga-reader-get-manifest (book-id callback)
  (funcall (or (plist-get komga-reader-backend-impl :get-manifest)
               (error "No backend registered"))
           book-id callback))

(defun komga-reader-get-chapter (book-id resource-url callback)
  (funcall (or (plist-get komga-reader-backend-impl :get-chapter)
               (error "No backend registered"))
           book-id resource-url callback))

(defun komga-reader-get-progression (book-id callback)
  (funcall (or (plist-get komga-reader-backend-impl :get-progression)
               (error "No backend registered"))
           book-id callback))

(defun komga-reader-update-progression (book-id position href callback)
  (funcall (or (plist-get komga-reader-backend-impl :update-progression)
               (error "No backend registered"))
           book-id position href callback))

(provide 'komga-reader-backend)
;;; komga-reader-backend.el ends here
