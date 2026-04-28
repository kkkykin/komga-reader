;;; komga-reader-backend.el --- Backend abstraction layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Backend abstraction for komga-reader.
;; Implementations must register themselves by setting `komga-reader-backend-impl'.

;;; Code:

(require 'json)

(defvar komga-reader-backend-impl nil
  "Plist with keys :list-books :get-manifest :get-chapter :get-progression :update-progression.
Each value is a function.")

(defun komga-reader--curl (method url &optional headers body)
  "Call curl with METHOD, URL, optional HEADERS alist and BODY string.
Return (STATUS-CODE . BODY-STRING)."
  (let* ((args (list "-s" "-w" "\nHTTP_CODE:%{http_code}" "-X" method url))
         (coding-system-for-read 'utf-8))
    (dolist (h headers)
      (setq args (append args (list "-H" (concat (car h) ": " (cdr h))))))
    (when body
      (setq args (append args (list "-H" "Content-Type: application/json" "-d" body))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process "curl" nil t nil args)))
        (if (/= exit-code 0)
            (error "curl failed: exit code %d" exit-code)
          (let* ((output (buffer-string))
                 (code-match (string-match "\nHTTP_CODE:\\([0-9]+\\)\n?$" output))
                 (code (string-to-number (match-string 1 output)))
                 (body (substring output 0 (match-beginning 0))))
            (cons code body)))))))

(defun komga-reader-list-books (&optional query page size)
  (funcall (or (plist-get komga-reader-backend-impl :list-books)
               (error "No backend registered"))
           query page size))

(defun komga-reader-get-manifest (book-id)
  (funcall (or (plist-get komga-reader-backend-impl :get-manifest)
               (error "No backend registered"))
           book-id))

(defun komga-reader-get-chapter (book-id resource-url)
  (funcall (or (plist-get komga-reader-backend-impl :get-chapter)
               (error "No backend registered"))
           book-id resource-url))

(defun komga-reader-get-progression (book-id)
  (funcall (or (plist-get komga-reader-backend-impl :get-progression)
               (error "No backend registered"))
           book-id))

(defun komga-reader-update-progression (book-id position href)
  (funcall (or (plist-get komga-reader-backend-impl :update-progression)
               (error "No backend registered"))
           book-id position href))

(provide 'komga-reader-backend)
;;; komga-reader-backend.el ends here
