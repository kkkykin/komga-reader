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

(defun komga-reader-komga-list-books (callback &optional query page size)
  (let* ((url (format "%s/api/v1/books/list?page=%d&size=%d"
                       (komga-reader-komga--url)
                       (or page 0) (or size 20)))
         (body (if query
                   (json-encode `((fullTextSearch . ,query)))
                 "{}")))
    (komga-reader--curl
     "POST" url
     (lambda (code json)
       (if (= code 200)
           (let ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol))
             (funcall callback (json-read-from-string json)))
         (error "Failed to list books: HTTP %d" code)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Content-Type" . "application/json"))
     body)))

(defun komga-reader-komga-get-manifest (book-id callback)
  (let* ((url (format "%s/api/v1/books/%s/manifest/epub"
                       (komga-reader-komga--url) book-id)))
    (komga-reader--curl
     "GET" url
     (lambda (code json)
       (if (= code 200)
           (let ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol))
             (funcall callback (json-read-from-string json)))
         (error "Failed to get manifest: HTTP %d" code)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Accept" . "application/webpub+json")))))

(defun komga-reader-komga-get-chapter (_book-id resource-url callback)
  (komga-reader--curl
   "GET" resource-url
   (lambda (code body)
     (if (= code 200)
         (funcall callback body)
       (error "Failed to get chapter: HTTP %d" code)))
   `(("X-API-Key" . ,(komga-reader-komga--api-key)))))

(defun komga-reader-komga-get-progression (book-id callback)
  (let ((url (format "%s/api/v1/books/%s/progression"
                     (komga-reader-komga--url) book-id)))
    (komga-reader--curl
     "GET" url
     (lambda (code json)
       (if (= code 200)
           (let ((json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'symbol))
             (funcall callback (json-read-from-string json)))
         (funcall callback nil)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Accept" . "application/vnd.readium.progression+json")))))

(defun komga-reader-komga-update-progression (book-id position href callback)
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
                  (modified . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t))))))
    (komga-reader--curl
     "PUT" url
     (lambda (code body)
       (unless (= code 204)
         (message "Warning: failed to update progression: HTTP %d" code))
       (when callback
         (funcall callback code body)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Content-Type" . "application/json"))
     body)))

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
