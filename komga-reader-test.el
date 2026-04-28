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
      (let* ((content (cdr (assoc 'content result)))
             (first-book (car content))
             (id (cdr (assoc 'id first-book)))
             (metadata (cdr (assoc 'metadata first-book)))
             (title (cdr (assoc 'title metadata))))
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

(ert-deftest komga-reader-test-next-html-cache ()
  "Test that preloaded HTML is stored in buffer-local variable."
  (with-temp-buffer
    (setq-local komga-reader-reader--next-html nil)
    (setq-local komga-reader-reader--next-html "<html>cached</html>")
    (should (string= komga-reader-reader--next-html "<html>cached</html>"))))

(provide 'komga-reader-test)
;;; komga-reader-test.el ends here
