;;; komga-reader-test.el --- Tests for komga-reader  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author:
;; Keywords: comm

;;; Commentary:

;; ERT tests for komga-reader async refactor.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'komga-reader-backend)
(require 'komga-reader-komga)
(require 'komga-reader-reader)
(require 'komga-reader)

;; ---------------------------------------------------------------------------
;; Debug logging tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-debug-log-nil ()
  "Test that debug log produces no output when debug is nil."
  (let ((komga-reader-debug nil)
        (komga-reader-debug-buffer "*komga-reader-test-debug-nil*"))
    (komga-reader--debug-log "test %s" "message")
    (should (not (get-buffer komga-reader-debug-buffer)))))

(ert-deftest komga-reader-test-debug-log-non-nil ()
  "Test that debug log produces output to debug buffer when debug is enabled."
  (let ((komga-reader-debug t)
        (komga-reader-debug-buffer "*komga-reader-test-debug-nonnil*"))
    (unwind-protect
        (progn
          (komga-reader--debug-log "test %s" "message")
          (let ((buf (get-buffer komga-reader-debug-buffer)))
            (should buf)
            (should (string= (with-current-buffer buf (buffer-string))
                             "[komga-reader] test message\n"))))
      (when (get-buffer komga-reader-debug-buffer)
        (kill-buffer komga-reader-debug-buffer)))))

(ert-deftest komga-reader-test-debug-custom-variable ()
  "Test that komga-reader-debug is a boolean custom variable."
  (should (booleanp komga-reader-debug)))

;; ---------------------------------------------------------------------------
;; curl extra-args tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-curl-extra-args-default ()
  "Test that curl extra args default to nil."
  (should (null komga-reader-curl-extra-args)))

(ert-deftest komga-reader-test-curl-extra-args-injected ()
  "Test that curl extra args are injected into the curl command."
  (let ((captured-command nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq captured-command (plist-get args :command)))))
      (let ((komga-reader-curl-extra-args '("--proxy" "http://127.0.0.1:8080")))
        (komga-reader--curl "GET" "http://example.com" (lambda (_ _)) nil nil)
        (should (member "--proxy" captured-command))
        (should (member "http://127.0.0.1:8080" captured-command))
        (should (member "http://example.com" captured-command))
        ;; extra args should appear before the URL
        (should (< (seq-position captured-command "--proxy")
                      (seq-position captured-command "http://example.com")))))))

;; ---------------------------------------------------------------------------
;; Low-level async process tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-make-process-stdout-collection ()
  "Test that make-process collects stdout via :filter and invokes :sentinel."
  (let ((output "")
        (done nil))
    (make-process
     :name "test-echo"
     :command '("echo" "-n" "hello\nHTTP_CODE:200")
     :filter (lambda (_proc string)
               (setq output (concat output string)))
     :sentinel (lambda (proc _event)
                 (when (eq (process-status proc) 'exit)
                   (setq done t))))
    (with-timeout (5 (ert-fail "Process timed out"))
      (while (not done)
        (sleep-for 0.05)))
    (should (string= output "hello\nHTTP_CODE:200"))))

(ert-deftest komga-reader-test-make-process-exit-code ()
  "Test that non-zero exit code is captured."
  (let ((exit-code nil)
        (done nil))
    (make-process
     :name "test-fail"
     :command '("sh" "-c" "exit 42")
     :filter (lambda (_proc _string) nil)
     :sentinel (lambda (proc _event)
                 (when (eq (process-status proc) 'exit)
                   (setq exit-code (process-exit-status proc))
                   (setq done t))))
    (with-timeout (5 (ert-fail "Process timed out"))
      (while (not done)
        (sleep-for 0.05)))
    (should (= exit-code 42))))

;; ---------------------------------------------------------------------------
;; komga-reader--curl async callback tests (mocked)
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-curl-callback-chain ()
  "Test that komga-reader--curl invokes callback with parsed status and body."
  (komga-reader-komga-init)
  (let ((called nil)
        (received-body nil))
    ;; Mock curl to simulate immediate async callback
    (cl-letf (((symbol-function 'komga-reader--curl)
               (lambda (_method _url callback &optional _headers _body)
                 (run-with-timer 0.05 nil
                                 (lambda ()
                                   (funcall callback 200 "test-body"))))))
      (komga-reader-get-chapter "book1" "http://example.com/ch1"
                                (lambda (body)
                                  (setq called t)
                                  (setq received-body body)))
      (with-timeout (5 (ert-fail "Callback timed out"))
        (while (not called)
          (sleep-for 0.05)))
      (should (string= received-body "test-body")))))

(ert-deftest komga-reader-test-list-books-json-parse ()
  "Test JSON parsing callback in komga-reader-komga-list-books."
  (komga-reader-komga-init)
  (let ((result nil))
    (cl-letf (((symbol-function 'komga-reader--curl)
               (lambda (_method _url callback &optional _headers _body)
                 (run-with-timer 0.05 nil
                                 (lambda ()
                                   (funcall callback 200
                                            "{\"content\":[{\"id\":\"42\",\"metadata\":{\"title\":\"Test\"}}]}"))))))
      (komga-reader-list-books
       (lambda (data)
         (setq result data)))
      (with-timeout (5 (ert-fail "Callback timed out"))
        (while (not result)
          (sleep-for 0.05)))
      (let* ((content (plist-get result :content))
             (first-book (car content))
             (id (plist-get first-book :id))
             (metadata (plist-get first-book :metadata))
             (title (plist-get metadata :title)))
        (should (string= id "42"))
        (should (string= title "Test"))))))

(ert-deftest komga-reader-test-get-progression-nil-on-404 ()
  "Test that get-progression returns nil on non-200."
  (komga-reader-komga-init)
  (let ((result 'uninitialized))
    (cl-letf (((symbol-function 'komga-reader--curl)
               (lambda (_method _url callback &optional _headers _body)
                 (run-with-timer 0.05 nil
                                 (lambda ()
                                   (funcall callback 404 "not found"))))))
      (komga-reader-get-progression "book1"
                                    (lambda (data)
                                      (setq result data)))
      (with-timeout (5 (ert-fail "Callback timed out"))
        (while (eq result 'uninitialized)
          (sleep-for 0.05)))
      (should (null result)))))

;; ---------------------------------------------------------------------------
;; Reader cache / preload tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-chapter-cache ()
  "Test that chapter cache stores and retrieves HTML correctly."
  (with-temp-buffer
    (setq-local komga-reader-reader--chapter-cache nil)
    (komga-reader-reader--cache-put 5 "<html>cached</html>")
    (should (string= (komga-reader-reader--cache-get 5) "<html>cached</html>"))
    (komga-reader-reader--cache-put 5 nil)
    (should (null (komga-reader-reader--cache-get 5)))))

(ert-deftest komga-reader-test-preload-ahead-target-buf ()
  "Test that preload-ahead uses passed target-buf rather than current-buffer."
  (let ((reading-buf (generate-new-buffer " *test-reading*"))
        (other-buf (generate-new-buffer " *test-other*")))
    (unwind-protect
        (progn
          (with-current-buffer reading-buf
            (komga-reader-reader-mode)
            (setq-local komga-reader-reader--book-id "book1")
            (setq-local komga-reader-reader--chapter-index 0)
            (setq-local komga-reader-reader--total-chapters 5)
            (setq-local komga-reader-reader--reading-order
                        '((:href "ch0") (:href "ch1") (:href "ch2") (:href "ch3") (:href "ch4")))
            (setq-local komga-reader-reader--chapter-cache nil))
          ;; Switch to another buffer and call preload-ahead with reading-buf
          (with-current-buffer other-buf
            (should (null komga-reader-reader--book-id))
            (let ((fetched-chapters nil))
              (cl-letf (((symbol-function 'komga-reader-get-chapter)
                         (lambda (_book-id href callback)
                           (push href fetched-chapters)
                           (funcall callback "html"))))
                (komga-reader-reader--preload-ahead reading-buf))
              ;; Should have fetched chapters 1, 2, 3 (preload count = 3)
              (should (= (length fetched-chapters) 3)))))
      (when (buffer-live-p reading-buf) (kill-buffer reading-buf))
      (when (buffer-live-p other-buf) (kill-buffer other-buf)))))

(ert-deftest komga-reader-test-preload-ahead-dead-buffer ()
  "Test that preload-ahead silently returns when target buffer is dead."
  (let ((dead-buf (generate-new-buffer " *test-dead*")))
    (kill-buffer dead-buf)
    ;; Should not signal an error
    (should (null (komga-reader-reader--preload-ahead dead-buf)))))

(ert-deftest komga-reader-test-preload-ahead-no-book-id ()
  "Test that preload-ahead returns when target buffer has no book-id."
  (let ((buf (generate-new-buffer " *test-no-book*"))
        (other-buf (generate-new-buffer " *test-other*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (komga-reader-reader-mode)
            ;; intentionally leave book-id nil
            (setq-local komga-reader-reader--chapter-index 0)
            (setq-local komga-reader-reader--total-chapters 5))
          (with-current-buffer other-buf
            (let ((fetched nil))
              (cl-letf (((symbol-function 'komga-reader-get-chapter)
                         (lambda (_book-id _href callback)
                           (setq fetched t)
                           (funcall callback "html"))))
                (komga-reader-reader--preload-ahead buf)
                (should (null fetched))))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (buffer-live-p other-buf) (kill-buffer other-buf)))))

(ert-deftest komga-reader-test-load-chapter-schedules-preload-with-buf ()
  "Test that load-chapter schedules preload timer with buffer argument."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--book-id "book1")
    (setq-local komga-reader-reader--chapter-index 0)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--reading-order
                '((:href "ch0") (:href "ch1")))
    (setq-local komga-reader-reader--chapter-cache nil)
    (let ((timer-args nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat fn &rest args)
                   (setq timer-args (cons fn args))
                   nil))
                ((symbol-function 'komga-reader-reader--render-html)
                 (lambda (_html) nil))
                ((symbol-function 'komga-reader-reader--sync-progression)
                 (lambda () nil))
                ((symbol-function 'komga-reader-get-chapter)
                 (lambda (_book-id _href callback)
                   (funcall callback "<html>test</html>"))))
        (komga-reader-reader--load-chapter 0)
        (should timer-args)
        (should (eq (car timer-args) #'komga-reader-reader--preload-ahead))
        ;; The second element should be the buffer
        (should (bufferp (cadr timer-args)))
        (should (eq (cadr timer-args) (current-buffer)))))))

(ert-deftest komga-reader-test-next-chapter-schedules-preload-with-buf ()
  "Test that next-chapter schedules preload timer with buffer argument."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--book-id "book1")
    (setq-local komga-reader-reader--chapter-index 0)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--reading-order
                '((:href "ch0") (:href "ch1")))
    (setq-local komga-reader-reader--chapter-cache nil)
    (komga-reader-reader--cache-put 1 "<html>cached</html>")
    (let ((timer-args nil))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_secs _repeat fn &rest args)
                   (setq timer-args (cons fn args))
                   nil))
                ((symbol-function 'komga-reader-reader--render-html)
                 (lambda (_html) nil))
                ((symbol-function 'komga-reader-reader--sync-progression)
                 (lambda () nil)))
        (komga-reader-reader-next-chapter)
        (should timer-args)
        (should (eq (car timer-args) #'komga-reader-reader--preload-ahead))
        (should (bufferp (cadr timer-args)))
        (should (eq (cadr timer-args) (current-buffer)))))))

;; ---------------------------------------------------------------------------
;; Device ID / Device Name tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-device-id-default ()
  "Test that device-id defaults to emacs-(system-name)."
  (let ((komga-reader-komga-device-id nil))
    (should (string= (komga-reader-komga--device-id)
                     (format "emacs-%s" (system-name))))))

(ert-deftest komga-reader-test-device-name-default ()
  "Test that device-name defaults to Emacs (system-name)."
  (let ((komga-reader-komga-device-name nil))
    (should (string= (komga-reader-komga--device-name)
                     (format "Emacs %s" (system-name))))))

(ert-deftest komga-reader-test-device-id-custom ()
  "Test that custom device-id is respected."
  (let ((komga-reader-komga-device-id "my-custom-device"))
    (should (string= (komga-reader-komga--device-id) "my-custom-device"))))

(ert-deftest komga-reader-test-device-name-custom ()
  "Test that custom device-name is respected."
  (let ((komga-reader-komga-device-name "My Reader"))
    (should (string= (komga-reader-komga--device-name) "My Reader"))))

;; ---------------------------------------------------------------------------
;; Keymap tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-reader-keymap-spc-scroll-up ()
  "Test that SPC is bound to scroll-up-command in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "SPC"))
              #'scroll-up-command)))

(ert-deftest komga-reader-test-reader-keymap-del-scroll-down ()
  "Test that DEL is bound to scroll-down-command in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "DEL"))
              #'scroll-down-command)))

(ert-deftest komga-reader-test-reader-keymap-j-unbound ()
  "Test that j is no longer bound in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "j"))
              nil)))

(ert-deftest komga-reader-test-reader-keymap-k-unbound ()
  "Test that k is no longer bound in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "k"))
              nil)))

(ert-deftest komga-reader-test-booklist-keymap-g-revert ()
  "Test that g is bound to revert-buffer in booklist mode."
  (should (eq (lookup-key komga-reader-booklist-mode-map (kbd "g"))
              #'revert-buffer)))

;; ---------------------------------------------------------------------------
;; Multisession cache tests (Emacs 29+ only)
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-cache-ttl-zero-disabled ()
  "Test that cache is disabled when TTL is 0."
  (let ((komga-reader-booklist-cache-ttl 0))
    (should (null (komga-reader--booklist-get-cache)))))

(ert-deftest komga-reader-test-cache-put-and-get ()
  "Test that cache stores and retrieves entries."
  (skip-unless (featurep 'multisession))
  (let ((komga-reader-booklist-cache-ttl 300)
        (test-entries '(("id1" ["Title1" "Author1" "100" "50%" "2024-01-01"]))))
    (komga-reader--booklist-put-cache test-entries)
    (let ((cached (komga-reader--booklist-get-cache)))
      (should (equal cached test-entries)))))

(ert-deftest komga-reader-test-cache-expired ()
  "Test that expired cache returns nil."
  (skip-unless (featurep 'multisession))
  (let ((komga-reader-booklist-cache-ttl 1)
        (test-entries '(("id1" ["Title1" "Author1" "100" "50%" "2024-01-01"]))))
    (komga-reader--booklist-put-cache test-entries)
    ;; Wait for cache to expire
    (sleep-for 1.5)
    (should (null (komga-reader--booklist-get-cache)))))

;; ---------------------------------------------------------------------------
;; Reader keymap / revert-buffer / toc tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-reader-keymap-t-open-toc ()
  "Test that t is bound to komga-reader-reader-open-toc in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "t"))
              #'komga-reader-reader-open-toc)))

(ert-deftest komga-reader-test-reader-revert-buffer-function ()
  "Test that revert-buffer-function is set in reader mode."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (should (eq revert-buffer-function #'komga-reader-reader--refresh))))

;; ---------------------------------------------------------------------------
;; Revert-buffer hook test
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-booklist-revert-buffer-function ()
  "Test that revert-buffer-function is set in booklist mode."
  (with-temp-buffer
    (komga-reader-booklist-mode)
    (should (eq revert-buffer-function #'komga-reader--booklist-refresh))))

;; ---------------------------------------------------------------------------
;; Reader open chapter-index tests
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-reader-open-with-chapter-index-ignores-server ()
  "When chapter-index is explicitly given, do not override with server progress."
  (with-temp-buffer
    (let ((loaded-index nil)
          (manifest '(:readingOrder ((:href "ch0") (:href "ch1") (:href "ch2"))
                      :metadata (:title "Test"))))
      (cl-letf (((symbol-function 'komga-reader-get-progression)
                 (lambda (_book-id callback)
                   ;; Server says progress is at chapter 2
                   (run-with-timer 0.01 nil
                                   (lambda ()
                                     (funcall callback
                                              '(:locator (:href "ch2")))))))
                ((symbol-function 'komga-reader-reader--load-chapter)
                 (lambda (index)
                   (setq loaded-index index)))
                ((symbol-function 'komga-reader--record-last-read-book)
                 (lambda (_) nil))
                ((symbol-function 'pop-to-buffer)
                 (lambda (_) (current-buffer))))
        (komga-reader-reader-open "book1" manifest 1)
        (with-timeout (5 (ert-fail "Async timeout"))
          (while (null loaded-index)
            (sleep-for 0.05)))
        (should (= loaded-index 1))))))

(ert-deftest komga-reader-test-reader-open-without-chapter-index-uses-server ()
  "When chapter-index is nil, resume from server progress."
  (with-temp-buffer
    (let ((loaded-index nil)
          (manifest '(:readingOrder ((:href "ch0") (:href "ch1") (:href "ch2"))
                      :metadata (:title "Test"))))
      (cl-letf (((symbol-function 'komga-reader-get-progression)
                 (lambda (_book-id callback)
                   ;; Server says chapter 2
                   (run-with-timer 0.01 nil
                                   (lambda ()
                                     (funcall callback
                                              '(:locator (:href "ch2")))))))
                ((symbol-function 'komga-reader-reader--load-chapter)
                 (lambda (index)
                   (setq loaded-index index)))
                ((symbol-function 'komga-reader--record-last-read-book)
                 (lambda (_) nil))
                ((symbol-function 'pop-to-buffer)
                 (lambda (_) (current-buffer))))
        (komga-reader-reader-open "book1" manifest)
        (with-timeout (5 (ert-fail "Async timeout"))
          (while (null loaded-index)
            (sleep-for 0.05)))
        (should (= loaded-index 2))))))

(ert-deftest komga-reader-test-reader-open-without-progress-defaults-to-zero ()
  "When chapter-index is nil and server has no progress, default to chapter 0."
  (with-temp-buffer
    (let ((loaded-index nil)
          (manifest '(:readingOrder ((:href "ch0") (:href "ch1") (:href "ch2"))
                      :metadata (:title "Test"))))
      (cl-letf (((symbol-function 'komga-reader-get-progression)
                 (lambda (_book-id callback)
                   ;; Server has no progress
                   (run-with-timer 0.01 nil
                                   (lambda ()
                                     (funcall callback nil)))))
                ((symbol-function 'komga-reader-reader--load-chapter)
                 (lambda (index)
                   (setq loaded-index index)))
                ((symbol-function 'komga-reader--record-last-read-book)
                 (lambda (_) nil))
                ((symbol-function 'pop-to-buffer)
                 (lambda (_) (current-buffer))))
        (komga-reader-reader-open "book1" manifest)
        (with-timeout (5 (ert-fail "Async timeout"))
          (while (null loaded-index)
            (sleep-for 0.05)))
        (should (= loaded-index 0))))))

;; ---------------------------------------------------------------------------
;; quit-window-hook tests (Phase 1)
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-reader-q-bound-to-quit-window ()
  "Test that q is bound to quit-window in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "q"))
              #'quit-window)))

(ert-deftest komga-reader-test-reader-quit-removed ()
  "Test that komga-reader-reader-quit no longer exists."
  (should-not (fboundp 'komga-reader-reader-quit)))

(ert-deftest komga-reader-test-reader-quit-window-hook-has-sync ()
  "Test that quit-window-hook contains sync-progression in reader buffer."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (should (memq #'komga-reader-reader--sync-progression quit-window-hook))))

(ert-deftest komga-reader-test-reader-quit-window-hook-is-buffer-local ()
  "Test that quit-window-hook sync entry is buffer-local."
  (let ((global-hook (default-value 'quit-window-hook)))
    (with-temp-buffer
      (komga-reader-reader-mode)
      (should-not (memq #'komga-reader-reader--sync-progression global-hook))
      (should (memq #'komga-reader-reader--sync-progression quit-window-hook)))))

;; ---------------------------------------------------------------------------
;; Reading history tests (Phase 2)
;; ---------------------------------------------------------------------------

(ert-deftest komga-reader-test-reader-l-bound-to-history-back ()
  "Test that l is bound to history-back in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "l"))
              #'komga-reader-reader-history-back)))

(ert-deftest komga-reader-test-reader-r-bound-to-history-forward ()
  "Test that r is bound to history-forward in reader mode."
  (should (eq (lookup-key komga-reader-reader-mode-map (kbd "r"))
              #'komga-reader-reader-history-forward)))

(ert-deftest komga-reader-test-history-push-basic ()
  "Test that history push adds current chapter to back stack."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--chapter-index 0)
    (setq-local komga-reader-reader--total-chapters 5)
    (komga-reader-reader--history-push)
    (should (equal komga-reader-reader--history-back '(0)))
    (should (null komga-reader-reader--history-forward))))

(ert-deftest komga-reader-test-history-push-clears-forward ()
  "Test that history push clears forward stack."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--chapter-index 2)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--history-forward '(4 5))
    (komga-reader-reader--history-push)
    (should (equal komga-reader-reader--history-back '(2)))
    (should (null komga-reader-reader--history-forward))))

(ert-deftest komga-reader-test-history-push-skipped-when-navigating ()
  "Test that history push is skipped when navigating flag is set."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--chapter-index 3)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--history-navigating t)
    (komga-reader-reader--history-push)
    (should (null komga-reader-reader--history-back))))

(ert-deftest komga-reader-test-history-push-skipped-when-no-chapter ()
  "Test that history push is skipped when total-chapters is 0."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--chapter-index 0)
    (setq-local komga-reader-reader--total-chapters 0)
    (komga-reader-reader--history-push)
    (should (null komga-reader-reader--history-back))))

(ert-deftest komga-reader-test-history-back-navigates ()
  "Test that history-back loads the previous chapter."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--book-id "book1")
    (setq-local komga-reader-reader--chapter-index 3)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--reading-order
                '((:href "ch0") (:href "ch1") (:href "ch2") (:href "ch3") (:href "ch4")))
    (setq-local komga-reader-reader--history-back '(1 2))
    (setq-local komga-reader-reader--history-forward nil)
    (let ((loaded-index nil))
      (cl-letf (((symbol-function 'komga-reader-reader--load-chapter)
                 (lambda (index)
                   (setq loaded-index index))))
        (komga-reader-reader-history-back)
        (should (= loaded-index 1))
        (should (equal komga-reader-reader--history-back '(2)))
        (should (equal komga-reader-reader--history-forward '(3)))))))

(ert-deftest komga-reader-test-history-back-empty ()
  "Test that history-back shows message when stack is empty."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--chapter-index 0)
    (setq-local komga-reader-reader--history-back nil)
    ;; Should not error, just message
    (komga-reader-reader-history-back)))

(ert-deftest komga-reader-test-history-forward-navigates ()
  "Test that history-forward loads the next chapter."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--book-id "book1")
    (setq-local komga-reader-reader--chapter-index 1)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--reading-order
                '((:href "ch0") (:href "ch1") (:href "ch2") (:href "ch3") (:href "ch4")))
    (setq-local komga-reader-reader--history-back '(0))
    (setq-local komga-reader-reader--history-forward '(3))
    (let ((loaded-index nil))
      (cl-letf (((symbol-function 'komga-reader-reader--load-chapter)
                 (lambda (index)
                   (setq loaded-index index))))
        (komga-reader-reader-history-forward)
        (should (= loaded-index 3))
        (should (equal komga-reader-reader--history-back '(1 0)))
        (should (null komga-reader-reader--history-forward))))))

(ert-deftest komga-reader-test-history-forward-empty ()
  "Test that history-forward shows message when stack is empty."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--chapter-index 0)
    (setq-local komga-reader-reader--history-forward nil)
    ;; Should not error, just message
    (komga-reader-reader-history-forward)))

(ert-deftest komga-reader-test-history-navigating-flag-cleared-after-error ()
  "Test that navigating flag is cleared even if load-chapter errors."
  (with-temp-buffer
    (komga-reader-reader-mode)
    (setq-local komga-reader-reader--book-id "book1")
    (setq-local komga-reader-reader--chapter-index 2)
    (setq-local komga-reader-reader--total-chapters 5)
    (setq-local komga-reader-reader--history-back '(0))
    (cl-letf (((symbol-function 'komga-reader-reader--load-chapter)
               (lambda (_index) (error "simulated error"))))
      (should-error (komga-reader-reader-history-back))
      (should (null komga-reader-reader--history-navigating)))))

(provide 'komga-reader-test)
;;; komga-reader-test.el ends here
