;;; komga-reader-komga.el --- Komga backend implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Komga-specific backend for komga-reader.

;;; Code:

(require 'komga-reader-backend)
(require 'json)

(defun komga-reader-komga--url ()
  (let ((url (getenv "KOMGA_URL")))
    (unless url (error "KOMGA_URL not set"))
    url))

(defun komga-reader-komga--api-key ()
  (let ((key (getenv "KOMGA_API_KEY")))
    (unless key (error "KOMGA_API_KEY not set"))
    key))

(defun komga-reader-komga-list-books (&optional query page size)
  (let* ((url (format "%s/api/v1/books/list?page=%d&size=%d"
                       (komga-reader-komga--url)
                       (or page 0) (or size 20)))
         (body (if query
                   (json-encode `((fullTextSearch . ,query)))
                 "{}"))
         (result (komga-reader--curl "POST" url
                                     `(("X-API-Key" . ,(komga-reader-komga--api-key))
                                       ("Content-Type" . "application/json"))
                                     body))
         (code (car result))
         (json (cdr result)))
    (unless (= code 200)
      (error "Failed to list books: HTTP %d" code))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-from-string json))))

(defun komga-reader-komga-get-manifest (book-id)
  (let* ((url (format "%s/api/v1/books/%s/manifest/epub"
                       (komga-reader-komga--url) book-id))
         (result (komga-reader--curl "GET" url
                                     `(("X-API-Key" . ,(komga-reader-komga--api-key))
                                       ("Accept" . "application/webpub+json"))))
         (code (car result))
         (json (cdr result)))
    (unless (= code 200)
      (error "Failed to get manifest: HTTP %d" code))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-key-type 'symbol))
      (json-read-from-string json))))

(defun komga-reader-komga-get-chapter (_book-id resource-url)
  (let* ((result (komga-reader--curl "GET" resource-url
                                     `(("X-API-Key" . ,(komga-reader-komga--api-key)))))
         (code (car result))
         (body (cdr result)))
    (unless (= code 200)
      (error "Failed to get chapter: HTTP %d" code))
    body))

(defun komga-reader-komga-get-progression (book-id)
  (let* ((url (format "%s/api/v1/books/%s/progression"
                       (komga-reader-komga--url) book-id))
         (result (komga-reader--curl "GET" url
                                     `(("X-API-Key" . ,(komga-reader-komga--api-key)))))
         (code (car result))
         (json (cdr result)))
    (if (= code 200)
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol))
          (json-read-from-string json))
      nil)))

(defun komga-reader-komga-update-progression (book-id position href)
  (let* ((url (format "%s/api/v1/books/%s/progression"
                       (komga-reader-komga--url) book-id))
         (relative-href (replace-regexp-in-string
                         (format "^%s/api/v1/books/%s/resource/"
                                 (regexp-quote (komga-reader-komga--url))
                                 book-id)
                         "" href))
         (body (json-encode
                `((device . ((id . "emacs-komga-reader")
                             (name . "Emacs Komga Reader")))
                  (locator . ((href . ,relative-href)
                              (type . "application/xhtml+xml")
                              (locations . ((position . ,position)
                                            (progression . 0.0)
                                            (totalProgression . 0.0)
                                            (fragments . [])))))
                  (modified . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
         (result (komga-reader--curl "PUT" url
                                     `(("X-API-Key" . ,(komga-reader-komga--api-key))
                                       ("Content-Type" . "application/json"))
                                     body))
         (code (car result)))
    (unless (= code 204)
      (message "Warning: failed to update progression: HTTP %d" code))))

;;;###autoload
(defun komga-reader-komga-init ()
  (setq komga-reader-backend-impl
        (list :list-books #'komga-reader-komga-list-books
              :get-manifest #'komga-reader-komga-get-manifest
              :get-chapter #'komga-reader-komga-get-chapter
              :get-progression #'komga-reader-komga-get-progression
              :update-progression #'komga-reader-komga-update-progression)))

(provide 'komga-reader-komga)
;;; komga-reader-komga.el ends here
