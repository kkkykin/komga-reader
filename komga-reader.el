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

(defvar-local komga-reader--toc-book-id nil)
(defvar-local komga-reader--toc-reading-order nil)
(defvar-local komga-reader--toc-manifest nil)

;;;###autoload
(defun komga-reader ()
  "Open the Komga reader book list."
  (interactive)
  (komga-reader-komga-init)
  (pop-to-buffer "*Komga Books*")
  (komga-reader-booklist-mode)
  (setq tabulated-list-entries (komga-reader--fetch-book-entries))
  (tabulated-list-print t))

(defun komga-reader--fetch-book-entries ()
  (let* ((result (komga-reader-list-books))
         (content (cdr (assoc 'content result)))
         (entries nil))
    (dolist (book content)
      (let* ((id (cdr (assoc 'id book)))
             (metadata (cdr (assoc 'metadata book)))
             (title (or (cdr (assoc 'title metadata)) "Unknown"))
             (authors (cdr (assoc 'authors metadata)))
             (author (or (car authors) "Unknown"))
             (media (cdr (assoc 'media book)))
             (pages (or (cdr (assoc 'pagesCount media)) 0))
             (read-progress (cdr (assoc 'readProgress book)))
             (progress-str
              (let ((page (and read-progress (cdr (assoc 'page read-progress)))))
                (if (and page (> pages 0))
                    (let ((pct (min 100 (round (* 100.0 (/ (float (1+ page)) pages))))))
                      (if (>= pct 100) "Done" (format "%d%%" pct)))
                  "-")))
             (last-modified (and read-progress (cdr (assoc 'lastModified read-progress))))
             (last-read-str
              (if last-modified
                  (format-time-string "%Y-%m-%d %H:%M" (date-to-time last-modified))
                "-")))
        (push (list id (vector title (format "%s" author) (format "%d" pages) progress-str last-read-str)) entries)))
    (nreverse entries)))

(defvar komga-reader-booklist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'komga-reader--booklist-select)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode komga-reader-booklist-mode tabulated-list-mode "Komga-Books"
  "Major mode for listing Komga books."
  (setq tabulated-list-format [("Title" 35 t)
                               ("Author" 18 t)
                               ("Pages" 7 t)
                               ("Progress" 10 t)
                               ("Last Read" 20 t)])
  (setq tabulated-list-sort-key (cons "Last Read" t))
  (tabulated-list-init-header))

(defun komga-reader--booklist-select ()
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (when id
      (komga-reader--open-toc id))))

(defun komga-reader--open-toc (book-id)
  (let* ((manifest (komga-reader-get-manifest book-id))
         (reading-order (cdr (assoc 'readingOrder manifest)))
         (toc (cdr (assoc 'toc manifest)))
         (metadata (cdr (assoc 'metadata manifest)))
         (title (or (cdr (assoc 'title metadata)) "Unknown")))
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
               (href (cdr (assoc 'href chapter)))
               (toc-entry (cl-find-if (lambda (e) (string= href (cdr (assoc 'href e)))) toc))
               (chapter-title (or (and toc-entry (cdr (assoc 'title toc-entry)))
                                  (format "Chapter %d" (1+ i)))))
          (insert-text-button chapter-title
                              'action #'komga-reader--toc-select
                              'book-id book-id
                              'chapter-index i
                              'href href
                              'follow-link t)
          (insert "\n")))
      (goto-char (point-min)))))

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
         (manifest (buffer-local-value 'komga-reader--toc-manifest (current-buffer))))
    (require 'komga-reader-reader)
    (komga-reader-reader-open book-id manifest chapter-index)))

(provide 'komga-reader)
;;; komga-reader.el ends here
