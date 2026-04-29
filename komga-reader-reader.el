;;; komga-reader-reader.el --- Reader mode for komga-reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; Render and read book chapters.

;;; Code:

(require 'shr)
(require 'komga-reader-backend)

(declare-function komga-reader--open-toc "komga-reader"
                  (book-id &optional chapter-index))

(defgroup komga-reader nil
  "Komga reader for Emacs."
  :group 'comm)

(defcustom komga-reader-preload-chapters-count 3
  "Number of upcoming chapters to preload in background.
Set to 0 to disable preloading."
  :type 'integer
  :group 'komga-reader)

(defvar-local komga-reader-reader--book-id nil)
(defvar-local komga-reader-reader--manifest nil)
(defvar-local komga-reader-reader--chapter-index 0)
(defvar-local komga-reader-reader--total-chapters 0)
(defvar-local komga-reader-reader--chapter-cache nil)
(defvar-local komga-reader-reader--reading-order nil)

(defun komga-reader-reader--cache-get (index)
  "Get cached HTML for chapter INDEX, or nil."
  (cdr (assoc index komga-reader-reader--chapter-cache)))

(defun komga-reader-reader--cache-put (index html)
  "Cache HTML for chapter INDEX."
  (setq-local komga-reader-reader--chapter-cache
              (cons (cons index html)
                    (assoc-delete-all index komga-reader-reader--chapter-cache))))

(defun komga-reader-reader--cache-clear ()
  "Clear the chapter cache."
  (setq-local komga-reader-reader--chapter-cache nil))

(defvar komga-reader-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "p") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map (kbd "<right>") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "<left>") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "t") #'komga-reader-reader-open-toc)
    (define-key map (kbd "q") #'komga-reader-reader-quit)
    map))

(define-derived-mode komga-reader-reader-mode special-mode "Komga-Reader"
  "Major mode for reading books."
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'komga-reader-reader--refresh))

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
  (let* ((reading-order (plist-get manifest :readingOrder))
         (total (length reading-order))
         (title (or (plist-get (plist-get manifest :metadata) :title) "Unknown"))
         (buf (pop-to-buffer (format "*Reading: %s*" title))))
    (komga-reader-reader-mode)
    (komga-reader-reader--cache-clear)
    (setq-local komga-reader-reader--book-id book-id)
    (setq-local komga-reader-reader--manifest manifest)
    (setq-local komga-reader-reader--reading-order reading-order)
    (setq-local komga-reader-reader--total-chapters total)
    (komga-reader-get-progression
     book-id
     (lambda (progression)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let* ((saved-pos (and progression
                                  (let ((locator (plist-get progression :locator)))
                                    (let ((locations (plist-get locator :locations)))
                                      (plist-get locations :position)))))
                  (saved-index (when (numberp saved-pos)
                                 (truncate saved-pos))))
             (when (and saved-index (>= saved-index 0) (< saved-index total))
               (setq chapter-index saved-index))
             (komga-reader-reader--load-chapter chapter-index))))))))

(defun komga-reader-reader--load-chapter (index)
  "Load chapter at INDEX."
  (when (and (>= index 0) (< index komga-reader-reader--total-chapters))
    (setq-local komga-reader-reader--chapter-index index)
    (let* ((chapter (nth index komga-reader-reader--reading-order))
           (href (plist-get chapter :href))
           (buf (current-buffer))
           (cached (komga-reader-reader--cache-get index)))
      (if cached
          (progn
            (komga-reader-reader--render-html cached)
            (komga-reader-reader--sync-progression)
            (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload-ahead))
        (message "Loading chapter %d..." (1+ index))
        (komga-reader-get-chapter
         komga-reader-reader--book-id href
         (lambda (html)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (komga-reader-reader--render-html html)
               (komga-reader-reader--sync-progression)
               (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload-ahead)))))))))

(defun komga-reader-reader--preload-ahead ()
  "Preload upcoming chapters in background."
  (when (and komga-reader-reader--book-id
             (> komga-reader-preload-chapters-count 0))
    (let ((current komga-reader-reader--chapter-index)
          (total komga-reader-reader--total-chapters)
          (buf (current-buffer)))
      (dotimes (i komga-reader-preload-chapters-count)
        (let ((idx (+ current 1 i)))
          (when (< idx total)
            (unless (komga-reader-reader--cache-get idx)
              (condition-case nil
                  (let ((chapter (nth idx komga-reader-reader--reading-order)))
                    (komga-reader-get-chapter
                     komga-reader-reader--book-id
                     (plist-get chapter :href)
                     (lambda (html)
                       (when (buffer-live-p buf)
                         (with-current-buffer buf
                           (komga-reader-reader--cache-put idx html))))))
                (error nil)))))))))

(defun komga-reader-reader-next-chapter ()
  "Go to next chapter."
  (interactive)
  (let ((next (1+ komga-reader-reader--chapter-index)))
    (if (>= next komga-reader-reader--total-chapters)
        (message "Last chapter")
      (let ((cached (komga-reader-reader--cache-get next)))
        (if cached
            (progn
              (setq-local komga-reader-reader--chapter-index next)
              (komga-reader-reader--render-html cached)
              (komga-reader-reader--cache-put next nil)
              (komga-reader-reader--sync-progression)
              (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload-ahead))
          (komga-reader-reader--load-chapter next))))))

(defun komga-reader-reader-prev-chapter ()
  "Go to previous chapter."
  (interactive)
  (let ((prev (1- komga-reader-reader--chapter-index)))
    (if (< prev 0)
        (message "First chapter")
      (komga-reader-reader--cache-clear)
      (komga-reader-reader--load-chapter prev))))

(defun komga-reader-reader--sync-progression ()
  "Save current chapter index to server."
  (when komga-reader-reader--book-id
    (let* ((chapter (nth komga-reader-reader--chapter-index
                         komga-reader-reader--reading-order))
           (href (plist-get chapter :href)))
      (komga-reader-update-progression
       komga-reader-reader--book-id
       komga-reader-reader--chapter-index
       href
       (lambda (_status _body) nil)))))

(defun komga-reader-reader-quit ()
  "Quit reader and sync progression."
  (interactive)
  (komga-reader-reader--sync-progression)
  (quit-window))

(defun komga-reader-reader--refresh (_ignore-auto _noconfirm)
  "Refresh the reader by fetching the latest progression from server.
If the server's saved chapter differs from the current one, jump to it.
Otherwise, reload the current chapter."
  (when komga-reader-reader--book-id
    (message "Fetching latest progress...")
    (let ((buf (current-buffer)))
      (komga-reader-get-progression
       komga-reader-reader--book-id
       (lambda (progression)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((saved-pos (and progression
                                    (let ((locator (plist-get progression :locator)))
                                      (let ((locations (plist-get locator :locations)))
                                        (plist-get locations :position)))))
                    (saved-index (when (numberp saved-pos)
                                   (truncate saved-pos)))
                    (total komga-reader-reader--total-chapters)
                    (current komga-reader-reader--chapter-index))
               (cond
                ((and saved-index (>= saved-index 0) (< saved-index total)
                      (/= saved-index current))
                 (message "Server progress: chapter %d" (1+ saved-index))
                 (komga-reader-reader--load-chapter saved-index))
                ((and saved-index (>= saved-index 0) (< saved-index total))
                 (message "Already at latest chapter, reloading...")
                 (komga-reader-reader--cache-put current nil)
                 (komga-reader-reader--load-chapter current))
                (t
                 (message "No server progress found, reloading current chapter...")
                 (komga-reader-reader--cache-put current nil)
                 (komga-reader-reader--load-chapter current)))))))))))

(defun komga-reader-reader-open-toc ()
  "Open the table of contents for the current book, jumping to current chapter."
  (interactive)
  (when komga-reader-reader--book-id
    (komga-reader--open-toc komga-reader-reader--book-id
                            komga-reader-reader--chapter-index)))

(provide 'komga-reader-reader)
;;; komga-reader-reader.el ends here
