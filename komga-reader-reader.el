;;; komga-reader-reader.el --- Reader mode for komga-reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Render and read book chapters.

;;; Code:

(require 'shr)
(require 'komga-reader-backend)

(defvar-local komga-reader-reader--book-id nil)
(defvar-local komga-reader-reader--manifest nil)
(defvar-local komga-reader-reader--chapter-index 0)
(defvar-local komga-reader-reader--total-chapters 0)
(defvar-local komga-reader-reader--next-html nil)
(defvar-local komga-reader-reader--reading-order nil)

(defvar komga-reader-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "p") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "SPC") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "DEL") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "j") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "k") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "<right>") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "<left>") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "q") #'komga-reader-reader-quit)
    map))

(define-derived-mode komga-reader-reader-mode special-mode "Komga-Reader"
  "Major mode for reading books."
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defun komga-reader-reader--put-image (_spec _alt _flags)
  "Ignore images.")

(defun komga-reader-reader--render-html (html)
  "Render HTML string in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert html)
    (let ((shr-put-image-function #'komga-reader-reader--put-image)
          (shr-inhibit-images t)
          (shr-blocked-images "."))
      (shr-render-region (point-min) (point-max)))
    (goto-char (point-min))))

(defun komga-reader-reader-open (book-id manifest &optional chapter-index)
  "Open BOOK-ID at CHAPTER-INDEX (default 0).
If a progression is saved on the server, resume from that position."
  (setq chapter-index (or chapter-index 0))
  (let* ((reading-order (cdr (assoc 'readingOrder manifest)))
         (total (length reading-order))
         (title (or (cdr (assoc 'title (cdr (assoc 'metadata manifest)))) "Unknown"))
         (progression (komga-reader-get-progression book-id))
         (saved-pos (and progression
                         (let ((locator (cdr (assoc 'locator progression))))
                           (let ((locations (cdr (assoc 'locations locator))))
                             (cdr (assoc 'position locations))))))
         (saved-index (when (numberp saved-pos)
                        (truncate saved-pos))))
    (when (and saved-index (>= saved-index 0) (< saved-index total))
      (setq chapter-index saved-index))
    (pop-to-buffer (format "*Reading: %s*" title))
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--book-id book-id)
    (setq-local komga-reader-reader--manifest manifest)
    (setq-local komga-reader-reader--reading-order reading-order)
    (setq-local komga-reader-reader--total-chapters total)
    (komga-reader-reader--load-chapter chapter-index)))

(defun komga-reader-reader--load-chapter (index)
  "Load chapter at INDEX."
  (when (and (>= index 0) (< index komga-reader-reader--total-chapters))
    (setq-local komga-reader-reader--chapter-index index)
    (let* ((chapter (nth index komga-reader-reader--reading-order))
           (href (cdr (assoc 'href chapter)))
           (html (komga-reader-get-chapter komga-reader-reader--book-id href)))
      (komga-reader-reader--render-html html)
      (komga-reader-reader--sync-progression)
      ;; Preload next chapter
      (when (< (1+ index) komga-reader-reader--total-chapters)
        (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload (1+ index))))))

(defun komga-reader-reader--preload (index)
  "Preload chapter INDEX in background."
  (when (and komga-reader-reader--book-id
             (< index komga-reader-reader--total-chapters))
    (let* ((chapter (nth index komga-reader-reader--reading-order))
           (href (cdr (assoc 'href chapter))))
      (condition-case nil
          (let ((html (komga-reader-get-chapter komga-reader-reader--book-id href)))
            (setq-local komga-reader-reader--next-html html))
        (error nil)))))

(defun komga-reader-reader-next-chapter ()
  "Go to next chapter."
  (interactive)
  (let ((next (1+ komga-reader-reader--chapter-index)))
    (if (>= next komga-reader-reader--total-chapters)
        (message "Last chapter")
      (if komga-reader-reader--next-html
          (progn
            (setq-local komga-reader-reader--chapter-index next)
            (komga-reader-reader--render-html komga-reader-reader--next-html)
            (setq-local komga-reader-reader--next-html nil)
            (komga-reader-reader--sync-progression)
            (when (< (1+ next) komga-reader-reader--total-chapters)
              (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload (1+ next))))
        (komga-reader-reader--load-chapter next)))))

(defun komga-reader-reader-prev-chapter ()
  "Go to previous chapter."
  (interactive)
  (let ((prev (1- komga-reader-reader--chapter-index)))
    (if (< prev 0)
        (message "First chapter")
      (setq-local komga-reader-reader--next-html nil)
      (komga-reader-reader--load-chapter prev))))

(defun komga-reader-reader--sync-progression ()
  "Save current chapter index to server."
  (when komga-reader-reader--book-id
    (let* ((chapter (nth komga-reader-reader--chapter-index
                         komga-reader-reader--reading-order))
           (href (cdr (assoc 'href chapter))))
      (komga-reader-update-progression komga-reader-reader--book-id
                                       komga-reader-reader--chapter-index
                                       href))))

(defun komga-reader-reader-quit ()
  "Quit reader and sync progression."
  (interactive)
  (komga-reader-reader--sync-progression)
  (quit-window))

(provide 'komga-reader-reader)
;;; komga-reader-reader.el ends here
