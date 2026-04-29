;;; komga-reader-komga.el --- Komga backend implementation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Komga-specific backend for komga-reader.

;;; Code:

(require 'komga-reader-backend)

(defcustom komga-reader-komga-url nil
  "Base URL of the Komga server."
  :type '(choice (string :tag "URL")
                 (const :tag "Unset" nil))
  :group 'komga-reader)

(defcustom komga-reader-komga-api-key nil
  "API key for authenticating with the Komga server."
  :type '(choice (string :tag "API Key")
                 (const :tag "Unset" nil))
  :group 'komga-reader)

(defcustom komga-reader-komga-device-id nil
  "Device ID for syncing reading progress with Komga.
Defaults to emacs-(system-name) if nil."
  :type '(choice (string :tag "Device ID")
                 (const :tag "Default (emacs-(system-name))" nil))
  :group 'komga-reader)

(defcustom komga-reader-komga-device-name nil
  "Device name for syncing reading progress with Komga.
Defaults to \"Emacs (system-name)\" if nil."
  :type '(choice (string :tag "Device Name")
                 (const :tag "Default (Emacs (system-name))" nil))
  :group 'komga-reader)

(defun komga-reader-komga--device-id ()
  "Return the device ID for syncing progress."
  (or komga-reader-komga-device-id
      (format "emacs-%s" (system-name))))

(defun komga-reader-komga--device-name ()
  "Return the device name for syncing progress."
  (or komga-reader-komga-device-name
      (format "Emacs %s" (system-name))))

(defun komga-reader-komga--url ()
  "Return the Komga server URL.
Prefers `komga-reader-komga-url', falling back to KOMGA_URL env var."
  (or komga-reader-komga-url
      (let ((url (getenv "KOMGA_URL")))
        (unless url (error "KOMGA_URL not set and `komga-reader-komga-url' is nil"))
        url)))

(defun komga-reader-komga--api-key ()
  "Return the Komga API key.
Prefers `komga-reader-komga-api-key', falling back to KOMGA_API_KEY env var."
  (or komga-reader-komga-api-key
      (let ((key (getenv "KOMGA_API_KEY")))
        (unless key (error "KOMGA_API_KEY not set and `komga-reader-komga-api-key' is nil"))
        key)))

(defun komga-reader-komga-list-books (callback &optional query page size)
  (komga-reader--debug-log "list-books query=%s page=%s size=%s" query page size)
  (let* ((url (format "%s/api/v1/books/list?page=%d&size=%d&sort=readProgress.readDate,desc"
                       (komga-reader-komga--url)
                       (or page 0) (or size 1000)))
         (body (if query
                   (json-serialize (list :fullTextSearch query))
                 "{}")))
    (komga-reader--curl
     "POST" url
     (lambda (code json)
       (komga-reader--debug-log "list-books response: HTTP %d" code)
       (if (= code 200)
           (funcall callback (json-parse-string json :object-type 'plist :array-type 'list))
         (error "Failed to list books: HTTP %d" code)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Content-Type" . "application/json"))
     body)))

(defun komga-reader-komga-get-manifest (book-id callback)
  (komga-reader--debug-log "get-manifest book-id=%s" book-id)
  (let* ((url (format "%s/api/v1/books/%s/manifest/epub"
                       (komga-reader-komga--url) book-id)))
    (komga-reader--curl
     "GET" url
     (lambda (code json)
       (komga-reader--debug-log "get-manifest response: HTTP %d" code)
       (if (= code 200)
           (funcall callback (json-parse-string json :object-type 'plist :array-type 'list))
         (error "Failed to get manifest: HTTP %d" code)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Accept" . "application/webpub+json")))))

(defun komga-reader-komga-get-chapter (_book-id resource-url callback)
  (komga-reader--debug-log "get-chapter resource-url=%s" resource-url)
  (komga-reader--curl
   "GET" resource-url
   (lambda (code body)
     (komga-reader--debug-log "get-chapter response: HTTP %d (body %d bytes)" code (length body))
     (if (= code 200)
         (funcall callback body)
       (error "Failed to get chapter: HTTP %d" code)))
   `(("X-API-Key" . ,(komga-reader-komga--api-key)))))

(defun komga-reader-komga-get-progression (book-id callback)
  (komga-reader--debug-log "get-progression book-id=%s" book-id)
  (let ((url (format "%s/api/v1/books/%s/progression"
                     (komga-reader-komga--url) book-id)))
    (komga-reader--curl
     "GET" url
     (lambda (code json)
       (komga-reader--debug-log "get-progression response: HTTP %d" code)
       (if (= code 200)
           (funcall callback (json-parse-string json :object-type 'plist :array-type 'list))
         (funcall callback nil)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Accept" . "application/vnd.readium.progression+json")))))

(defun komga-reader-komga-update-progression (book-id position href callback)
  (komga-reader--debug-log "update-progression book-id=%s position=%s href=%s" book-id position href)
  (let* ((url (format "%s/api/v1/books/%s/progression"
                      (komga-reader-komga--url) book-id))
         (relative-href (replace-regexp-in-string
                         (format "^%s/api/v1/books/%s/resource/"
                                 (regexp-quote (komga-reader-komga--url))
                                 book-id)
                         "" href))
         (body (json-serialize
                (list :device (list :id (komga-reader-komga--device-id)
                                    :name (komga-reader-komga--device-name))
                      :locator (list :href relative-href
                                     :type "application/xhtml+xml"
                                     :locations (list :position position
                                                      :progression 0.0
                                                      :totalProgression 0.0
                                                      :fragments []))
                      :modified (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
    (komga-reader--curl
     "PUT" url
     (lambda (code body)
       (komga-reader--debug-log "update-progression response: HTTP %d" code)
       (unless (= code 204)
         (message "Warning: failed to update progression: HTTP %d" code))
       (when callback
         (funcall callback code body)))
     `(("X-API-Key" . ,(komga-reader-komga--api-key))
       ("Content-Type" . "application/json"))
     body)))

;;;###autoload
(defun komga-reader-komga-init ()
  (komga-reader--debug-log "komga-init: registering backend")
  (setq komga-reader-backend-impl
        (list :list-books #'komga-reader-komga-list-books
              :get-manifest #'komga-reader-komga-get-manifest
              :get-chapter #'komga-reader-komga-get-chapter
              :get-progression #'komga-reader-komga-get-progression
              :update-progression #'komga-reader-komga-update-progression)))

(provide 'komga-reader-komga)
;;; komga-reader-komga.el ends here
