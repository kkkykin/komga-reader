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
(declare-function komga-reader--record-last-read-book "komga-reader"
                  (book-id))

(defgroup komga-reader nil
  "Komga reader for Emacs."
  :group 'comm)

(defcustom komga-reader-preload-chapters-count 3
  "Number of upcoming chapters to preload in background.
Set to 0 to disable preloading."
  :type 'integer
  :group 'komga-reader)

(defcustom komga-reader-render-images nil
  "When non-nil, render images in chapter HTML.
When nil (default), images are suppressed for a clean reading experience."
  :type 'boolean
  :group 'komga-reader)

(defvar-local komga-reader-reader--book-id nil)
(defvar-local komga-reader-reader--manifest nil)
(defvar-local komga-reader-reader--chapter-index 0)
(defvar-local komga-reader-reader--total-chapters 0)
(defvar-local komga-reader-reader--chapter-cache nil)
(defvar-local komga-reader-reader--reading-order nil)

(defvar-local komga-reader-reader--history-back nil
  "Stack of previously visited chapter indices for history-back navigation.")

(defvar-local komga-reader-reader--history-forward nil
  "Stack of chapter indices for history-forward navigation.")

(defvar-local komga-reader-reader--history-navigating nil
  "Non-nil when navigating via history to suppress history push.")

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

(defun komga-reader-reader--chapter-index-from-locator (locator reading-order)
  "Find chapter index in READING-ORDER matching LOCATOR's href.
Returns nil if no match found."
  (when locator
    (let ((href (plist-get locator :href))
          (index 0)
          (found nil))
      (while (and (not found) (< index (length reading-order)))
        (let ((chapter-href (plist-get (nth index reading-order) :href)))
          (when (and href chapter-href (string-suffix-p href chapter-href))
            (setq found index)))
        (setq index (1+ index)))
      found)))

(defvar komga-reader-reader-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "p") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map (kbd "<right>") #'komga-reader-reader-next-chapter)
    (define-key map (kbd "<left>") #'komga-reader-reader-prev-chapter)
    (define-key map (kbd "t") #'komga-reader-reader-open-toc)
    (define-key map (kbd "l") #'komga-reader-reader-history-back)
    (define-key map (kbd "r") #'komga-reader-reader-history-forward)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode komga-reader-reader-mode special-mode "Komga-Reader"
  "Major mode for reading books."
  (setq truncate-lines t)
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq-local revert-buffer-function #'komga-reader-reader--refresh)
  (add-hook 'quit-window-hook #'komga-reader-reader--sync-progression nil t))

(defun komga-reader-reader--put-image (_spec _alt _flags)
  "Ignore images.")

(defun komga-reader-reader--render-html (html)
  "Render HTML string in current buffer.
When `komga-reader-render-images' is nil, images are suppressed."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert html)
    (if komga-reader-render-images
        (shr-render-region (point-min) (point-max))
      (let ((shr-put-image-function #'komga-reader-reader--put-image)
            (shr-inhibit-images t)
            (shr-blocked-images "."))
        (shr-render-region (point-min) (point-max))))
    (goto-char (point-min))))

(defun komga-reader-reader-open (book-id manifest &optional chapter-index)
  "Open BOOK-ID at CHAPTER-INDEX (default 0).
If CHAPTER-INDEX is nil and a progression is saved on
the server, resume from that position."
  (let ((resume-from-server (null chapter-index)))
    (setq chapter-index (or chapter-index 0))
  (komga-reader--debug-log "reader-open book-id=%s chapter-index=%s" book-id chapter-index)
  (komga-reader--record-last-read-book book-id)
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
           (let* ((saved-index (and progression
                                    (komga-reader-reader--chapter-index-from-locator
                                     (plist-get progression :locator)
                                     reading-order))))
             (when (and resume-from-server
                        saved-index (>= saved-index 0) (< saved-index total))
               (komga-reader--debug-log "reader-open: resuming from server progress chapter %d" saved-index)
               (setq chapter-index saved-index))
             (komga-reader-reader--load-chapter chapter-index)))))))))

(defun komga-reader-reader--history-push ()
  "Push current chapter onto the back-history stack.
Clears forward-history.  Skipped when navigating."
  (unless komga-reader-reader--history-navigating
    (when (and komga-reader-reader--chapter-index
               (> komga-reader-reader--total-chapters 0))
      (push komga-reader-reader--chapter-index
            komga-reader-reader--history-back)
      (setq komga-reader-reader--history-forward nil))))

(defun komga-reader-reader--load-chapter (index)
  "Load chapter at INDEX."
  (when (and (>= index 0) (< index komga-reader-reader--total-chapters))
    (komga-reader--debug-log "load-chapter index=%d (total=%d)" index komga-reader-reader--total-chapters)
    (komga-reader-reader--history-push)
    (setq-local komga-reader-reader--chapter-index index)
    (let* ((chapter (nth index komga-reader-reader--reading-order))
           (href (plist-get chapter :href))
           (buf (current-buffer))
           (cached (komga-reader-reader--cache-get index)))
      (if cached
          (progn
            (komga-reader--debug-log "load-chapter: using cached chapter %d" index)
            (komga-reader-reader--render-html cached)
            (komga-reader-reader--sync-progression)
            (komga-reader--debug-log "load-chapter: scheduling preload timer for %s" (buffer-name buf))
            (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload-ahead buf))
        (message "Loading chapter %d..." (1+ index))
        (komga-reader-get-chapter
         komga-reader-reader--book-id href
         (lambda (html)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (komga-reader--debug-log "load-chapter: fetched chapter %d (%d bytes)" index (length html))
               (komga-reader-reader--render-html html)
               (komga-reader--debug-log "load-chapter: scheduling preload timer for %s" (buffer-name buf))
               (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload-ahead buf)))))))))

(defun komga-reader-reader--preload-ahead (target-buf)
  "Preload upcoming chapters in background for buffer TARGET-BUF."
  (komga-reader--debug-log "preload-ahead: invoked for buffer %s" (buffer-name target-buf))
  (when (and (buffer-live-p target-buf)
             (with-current-buffer target-buf
               (and komga-reader-reader--book-id
                    (> komga-reader-preload-chapters-count 0))))
    (with-current-buffer target-buf
       (let ((current komga-reader-reader--chapter-index)
             (total komga-reader-reader--total-chapters))
         (komga-reader--debug-log "preload-ahead: current=%d count=%d" current komga-reader-preload-chapters-count)
         (dotimes (i komga-reader-preload-chapters-count)
           (let ((idx (+ current 1 i)))
             (when (< idx total)
               (unless (komga-reader-reader--cache-get idx)
                 (condition-case nil
                     (let ((chapter (nth idx komga-reader-reader--reading-order)))
                       (komga-reader--debug-log "preload-ahead: fetching chapter %d" idx)
                       (komga-reader-get-chapter
                        komga-reader-reader--book-id
                        (plist-get chapter :href)
                        (lambda (html)
                          (when (buffer-live-p target-buf)
                            (with-current-buffer target-buf
                              (komga-reader--debug-log "preload-ahead: cached chapter %d (%d bytes)" idx (length html))
                              (komga-reader-reader--cache-put idx html))))))
                   (error nil))))))))))

(defun komga-reader-reader-next-chapter ()
  "Go to next chapter."
  (interactive)
  (let ((next (1+ komga-reader-reader--chapter-index)))
    (if (>= next komga-reader-reader--total-chapters)
        (message "Last chapter")
      (komga-reader--debug-log "next-chapter: %d -> %d" komga-reader-reader--chapter-index next)
      (let ((cached (komga-reader-reader--cache-get next))
            (buf (current-buffer)))
        (if cached
            (progn
              (komga-reader-reader--history-push)
              (setq-local komga-reader-reader--chapter-index next)
              (komga-reader-reader--render-html cached)
              (komga-reader-reader--cache-put next nil)
              (komga-reader-reader--sync-progression)
              (run-with-idle-timer 0.5 nil #'komga-reader-reader--preload-ahead buf))
          (komga-reader-reader--load-chapter next))))))

(defun komga-reader-reader-prev-chapter ()
  "Go to previous chapter."
  (interactive)
  (let ((prev (1- komga-reader-reader--chapter-index)))
    (if (< prev 0)
        (message "First chapter")
      (komga-reader--debug-log "prev-chapter: %d -> %d" komga-reader-reader--chapter-index prev)
      (komga-reader-reader--cache-clear)
      (komga-reader-reader--load-chapter prev))))

(defun komga-reader-reader--sync-progression ()
  "Save current chapter index to server."
  (when komga-reader-reader--book-id
    (let* ((chapter (nth komga-reader-reader--chapter-index
                         komga-reader-reader--reading-order))
           (href (plist-get chapter :href)))
      (komga-reader--debug-log "sync-progression: book=%s chapter=%d" komga-reader-reader--book-id komga-reader-reader--chapter-index)
      (komga-reader-update-progression
       komga-reader-reader--book-id
       komga-reader-reader--chapter-index
       href
       (lambda (_status _body) nil)))))

(defun komga-reader-reader--refresh (_ignore-auto _noconfirm)
  "Refresh the reader by fetching the latest progression from server.
If the server's saved chapter differs from the current one, jump to it.
Otherwise, reload the current chapter."
  (when komga-reader-reader--book-id
    (komga-reader--debug-log "refresh: book=%s current=%d" komga-reader-reader--book-id komga-reader-reader--chapter-index)
    (message "Fetching latest progress...")
    (let ((buf (current-buffer)))
      (komga-reader-get-progression
       komga-reader-reader--book-id
       (lambda (progression)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((saved-index (and progression
                                      (komga-reader-reader--chapter-index-from-locator
                                       (plist-get progression :locator)
                                       komga-reader-reader--reading-order)))
                    (total komga-reader-reader--total-chapters)
                    (current komga-reader-reader--chapter-index))
               (cond
                ((and saved-index (>= saved-index 0) (< saved-index total)
                      (/= saved-index current))
                 (komga-reader--debug-log "refresh: jumping to server chapter %d" saved-index)
                 (message "Server progress: chapter %d" (1+ saved-index))
                 (komga-reader-reader--load-chapter saved-index))
                ((and saved-index (>= saved-index 0) (< saved-index total))
                 (komga-reader--debug-log "refresh: reloading current chapter")
                 (message "Already at latest chapter, reloading...")
                 (komga-reader-reader--cache-put current nil)
                 (komga-reader-reader--load-chapter current))
                (t
                 (komga-reader--debug-log "refresh: no server progress, reloading current")
                 (message "No server progress found, reloading current chapter...")
                 (komga-reader-reader--cache-put current nil)
                 (komga-reader-reader--load-chapter current)))))))))))

(defun komga-reader-reader-open-toc ()
  "Open the table of contents for the current book, jumping to current chapter."
  (interactive)
  (when komga-reader-reader--book-id
    (komga-reader--open-toc komga-reader-reader--book-id
                            komga-reader-reader--chapter-index)))

(defun komga-reader-reader-history-back ()
  "Go back to the previous chapter in reading history."
  (interactive)
  (if (null komga-reader-reader--history-back)
      (message "No more history")
    (push komga-reader-reader--chapter-index
          komga-reader-reader--history-forward)
    (let ((prev (pop komga-reader-reader--history-back)))
      (setq komga-reader-reader--history-navigating t)
      (unwind-protect
          (komga-reader-reader--load-chapter prev)
        (setq komga-reader-reader--history-navigating nil)))))

(defun komga-reader-reader-history-forward ()
  "Go forward to the next chapter in reading history."
  (interactive)
  (if (null komga-reader-reader--history-forward)
      (message "No forward history")
    (push komga-reader-reader--chapter-index
          komga-reader-reader--history-back)
    (let ((next (pop komga-reader-reader--history-forward)))
      (setq komga-reader-reader--history-navigating t)
      (unwind-protect
          (komga-reader-reader--load-chapter next)
        (setq komga-reader-reader--history-navigating nil)))))

(provide 'komga-reader-reader)
;;; komga-reader-reader.el ends here
