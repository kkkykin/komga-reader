;;; komga-reader-backend.el --- Backend abstraction layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Backend abstraction for komga-reader.
;; Implementations must register themselves by setting `komga-reader-backend-impl'.
;; All backend functions are asynchronous and accept a callback.

;;; Code:

(defvar komga-reader-backend-impl nil
  "Plist registering backend functions.
Keys: :list-books :get-manifest :get-chapter
:get-progression :update-progression.
Each value is a function.")

(defun komga-reader--curl (method url callback &optional headers body)
  "Call curl with METHOD, URL, optional HEADERS alist and BODY string.
When finished, call CALLBACK with (STATUS-CODE BODY-STRING)."
  (let* ((args (list "-s" "-w" "\nHTTP_CODE:%{http_code}" "-X" method url))
         (coding-system-for-read 'utf-8)
         (output ""))
    (dolist (h headers)
      (setq args (append args (list "-H" (concat (car h) ": " (cdr h))))))
    (when body
      (setq args (append args (list "-H" "Content-Type: application/json" "-d" body))))
    (make-process
     :name "komga-reader-curl"
     :command (cons "curl" args)
     :connection-type 'pipe
     :filter (lambda (_proc chunk)
               (setq output (concat output chunk)))
     :sentinel (lambda (proc _event)
                 (when (eq (process-status proc) 'exit)
                   (if (/= (process-exit-status proc) 0)
                       (funcall callback 0 "")
                     (let* ((code-match (string-match "\nHTTP_CODE:\\([0-9]+\\)\n?$" output))
                            (code (if code-match
                                      (string-to-number (match-string 1 output))
                                    0))
                            (body (if code-match
                                      (substring output 0 (match-beginning 0))
                                    output)))
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
