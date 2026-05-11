;;; komga-reader.el --- Komga reader for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Browse and read books from a Komga server.

;;; Code:

(require 'tabulated-list)
(require 'cl-lib)
(require 'komga-reader-backend)
(require 'komga-reader-komga)

(when (version<= "29.1" emacs-version)
  (require 'multisession))

(declare-function komga-reader-reader-open "komga-reader-reader"
                  (book-id manifest &optional chapter-index))

(defvar-local komga-reader--toc-book-id nil)
(defvar-local komga-reader--toc-reading-order nil)
(defvar-local komga-reader--toc-manifest nil)
(defvar-local komga-reader--booklist-from-cache nil)

(defcustom komga-reader-booklist-cache-ttl 300
  "Time-to-live in seconds for the book list multisession cache.
Set to 0 to disable caching."
  :type 'integer
  :group 'komga-reader)

(when (featurep 'multisession)
  (define-multisession-variable komga-reader--booklist-cache nil
    "Cached book list entries for komga-reader.")
  (define-multisession-variable komga-reader--last-read-book nil
    "Last read book id for komga-reader."))

(defun komga-reader--record-last-read-book (book-id)
  "Record BOOK-ID as the most recently read book."
  (when (and (featurep 'multisession) (boundp 'komga-reader--last-read-book))
    (setf (multisession-value komga-reader--last-read-book) book-id)))

;;;###autoload
(defun komga-reader ()
  "Open the Komga reader book list."
  (interactive)
  (komga-reader-komga-init)
  (komga-reader--debug-log "opening book list")
  (let ((buf (pop-to-buffer "*Komga Books*")))
    (komga-reader-booklist-mode)
    (setq tabulated-list-entries nil)
    (tabulated-list-print t)
    ;; Try to load from multisession cache first
    (let ((cached (komga-reader--booklist-get-cache)))
      (when cached
        (setq-local komga-reader--booklist-from-cache t)
        (setq tabulated-list-entries cached)
        (tabulated-list-print t)
        (message "Showing cached book list (press g to refresh)")))
    (message "Loading books...")
    (komga-reader--fetch-book-entries
     (lambda (entries)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (when (eq major-mode 'komga-reader-booklist-mode)
             (setq-local komga-reader--booklist-from-cache nil)
             (setq tabulated-list-entries entries)
             (tabulated-list-print t))))))))

(defun komga-reader--fetch-book-entries (callback)
  (komga-reader--debug-log "fetch-book-entries")
  (komga-reader-list-books
   (lambda (result)
     (komga-reader--debug-log "fetch-book-entries: received %d books" (length (plist-get result :content)))
     (let* ((content (plist-get result :content))
            (entries nil))
       (dolist (book content)
         (let* ((id (plist-get book :id))
                (metadata (plist-get book :metadata))
                (title (or (plist-get metadata :title) "Unknown"))
                (authors (plist-get metadata :authors))
                (author (or (car authors) "Unknown"))
                (media (plist-get book :media))
                (pages (or (plist-get media :pagesCount) 0))
                (read-progress (plist-get book :readProgress))
                (progress-str
                 (let ((page (and read-progress (plist-get read-progress :page))))
                   (if (and page (> pages 0))
                       (let ((pct (min 100 (round (* 100.0 (/ (float (1+ page)) pages))))))
                         (if (>= pct 100) "Done" (format "%d%%" pct)))
                     "-")))
                (last-modified (and read-progress (plist-get read-progress :lastModified)))
                (last-read-str
                 (if last-modified
                     (format-time-string "%Y-%m-%d %H:%M" (date-to-time last-modified))
                   "-")))
           (push (list id (vector title (format "%s" author) (format "%d" pages) progress-str last-read-str)) entries)))
       (let ((entries (nreverse entries)))
         (komga-reader--booklist-put-cache entries)
         (funcall callback entries))))))

(defun komga-reader--booklist-get-cache ()
  "Return cached book list entries if not expired, else nil."
  (when (and (featurep 'multisession) (boundp 'komga-reader--booklist-cache)
             (> komga-reader-booklist-cache-ttl 0))
    (let* ((data (multisession-value komga-reader--booklist-cache))
           (timestamp (plist-get data :timestamp))
           (entries (plist-get data :entries)))
      (when (and timestamp entries
                 (< (float-time (time-subtract (current-time) timestamp))
                    komga-reader-booklist-cache-ttl))
        entries))))

(defun komga-reader--booklist-put-cache (entries)
  "Store book list ENTRIES in multisession cache."
  (when (and (featurep 'multisession) (boundp 'komga-reader--booklist-cache)
             (> komga-reader-booklist-cache-ttl 0))
    (setf (multisession-value komga-reader--booklist-cache)
          (list :timestamp (current-time) :entries entries))))

(defvar komga-reader-booklist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'komga-reader--booklist-select)
    (define-key map (kbd "t") #'komga-reader--booklist-open-toc)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "q") #'quit-window)
    map))

(defun komga-reader--booklist-open-toc ()
  "Open the table of contents for the selected book."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (komga-reader--open-toc id))))

(define-derived-mode komga-reader-booklist-mode tabulated-list-mode "Komga-Books"
  "Major mode for listing Komga books."
  (setq tabulated-list-format [("Title" 35 t)
                               ("Author" 18 t)
                               ("Pages" 7 t)
                               ("Progress" 10 t)
                               ("Last Read" 20 t)])
  (setq tabulated-list-sort-key (cons "Last Read" t))
  (setq-local revert-buffer-function #'komga-reader--booklist-refresh)
  (tabulated-list-init-header))

(defun komga-reader--booklist-refresh (_ignore-auto _noconfirm)
  "Refresh the Komga book list."
  (message "Refreshing books...")
  (komga-reader--fetch-book-entries
   (lambda (entries)
     (when (eq major-mode 'komga-reader-booklist-mode)
       (setq tabulated-list-entries entries)
       (tabulated-list-print t)))))

(defun komga-reader--booklist-select ()
  "Open the selected book at its saved progress, or chapter 0."
  (interactive)
  (let ((id (tabulated-list-get-id))
        (buf (current-buffer)))
    (when id
      (komga-reader--debug-log "booklist-select: book-id=%s" id)
      (when (buffer-local-value 'komga-reader--booklist-from-cache buf)
        (message "Note: book list is from cache, progress may not be up-to-date"))
      (message "Loading manifest...")
      (komga-reader-get-manifest
       id
       (lambda (manifest)
         (when (buffer-live-p buf)
           (require 'komga-reader-reader)
           (komga-reader-reader-open id manifest)))))))

(defun komga-reader--open-toc (book-id &optional chapter-index)
  (komga-reader--debug-log "open-toc: book-id=%s chapter-index=%s" book-id chapter-index)
  (message "Loading manifest...")
  (komga-reader-get-manifest
   book-id
   (lambda (manifest)
     (let* ((reading-order (plist-get manifest :readingOrder))
            (toc (plist-get manifest :toc))
            (metadata (plist-get manifest :metadata))
            (title (or (plist-get metadata :title) "Unknown")))
       (pop-to-buffer (format "*Komga: %s*" title))
       (komga-reader-toc-mode)
       (setq-local komga-reader--toc-book-id book-id)
       (setq-local komga-reader--toc-reading-order reading-order)
       (setq-local komga-reader--toc-manifest manifest)
       (let ((inhibit-read-only t))
         (erase-buffer)
         (insert (propertize (format "Table of Contents: %s\n\n" title)
                             'face 'bold))
         (dotimes (i (length reading-order))
           (let* ((chapter (nth i reading-order))
                  (href (plist-get chapter :href))
                  (toc-entry (cl-find-if (lambda (e) (string= href (plist-get e :href))) toc))
                  (chapter-title (or (and toc-entry (plist-get toc-entry :title))
                                     (format "Chapter %d" (1+ i)))))
             (insert-text-button chapter-title
                                 'action #'komga-reader--toc-select
                                 'book-id book-id
                                 'chapter-index i
                                 'href href
                                 'follow-link t)
             (insert "\n")))
         (goto-char (point-min))
         (when chapter-index
           (komga-reader--debug-log "open-toc: jumping to chapter %d" chapter-index)
           (komga-reader--toc-goto-chapter chapter-index)))))))

(defun komga-reader--toc-goto-chapter (chapter-index)
  "Move point to the button for CHAPTER-INDEX in the TOC buffer."
  (goto-char (point-min))
  (let ((btn (next-button (point-min))))
    (while (and btn (/= (button-get btn 'chapter-index) chapter-index))
      (setq btn (next-button (button-end btn))))
    (when btn
      (goto-char (button-start btn)))))

(defvar komga-reader-toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'komga-reader--toc-select-button)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode komga-reader-toc-mode special-mode "Komga-TOC"
  "Major mode for book table of contents."
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defun komga-reader--toc-select-button ()
  (interactive)
  (let ((button (button-at (point))))
    (when button
      (komga-reader--toc-select button))))

(defun komga-reader--toc-select (button)
  (let* ((book-id (button-get button 'book-id))
         (chapter-index (button-get button 'chapter-index))
         (manifest (buffer-local-value 'komga-reader--toc-manifest (current-buffer)))
         (existing-buf nil))
    (require 'komga-reader-reader)
    ;; Try to find an existing reader buffer for this book
    (dolist (buf (buffer-list))
      (when (and (eq (buffer-local-value 'major-mode buf) 'komga-reader-reader-mode)
                 (equal (buffer-local-value 'komga-reader-reader--book-id buf) book-id))
        (setq existing-buf buf)))
    (if existing-buf
        (progn
          (pop-to-buffer existing-buf)
          (komga-reader-reader--load-chapter chapter-index))
      (komga-reader-reader-open book-id manifest chapter-index))))

;;;###autoload
(defun komga-reader-resume ()
  "Resume reading the last book from the server progress."
  (interactive)
  (if (and (featurep 'multisession) (boundp 'komga-reader--last-read-book))
      (let ((book-id (multisession-value komga-reader--last-read-book)))
        (if book-id
            (progn
              (komga-reader-komga-init)
              (komga-reader--debug-log "resume: book-id=%s" book-id)
              (message "Resuming last book...")
              (komga-reader-get-manifest
               book-id
               (lambda (manifest)
                 (require 'komga-reader-reader)
                 (komga-reader-reader-open book-id manifest))))
          (message "No last read book found")))
    (message "Resume requires Emacs 29+ multisession support")))

(provide 'komga-reader)
;;; komga-reader.el ends here
