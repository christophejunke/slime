;;; swank-prompt.lisp --- Simple prompt object for REPL
;;;
;;; Authors: Christophe Junke  <deftransform@proton.me>
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(defpackage :swank-prompt
  (:use :cl)
  (:use :swank)
  (:import-from #:swank
                swank::send-to-emacs)
  (:export #:render-prompt
           #:prompt-command
           #:prompt-command-function
           #:command
           #:on-prompt-event
           #:simple-prompt
           #:simple-prompt-plist
           #:simple-prompt-value
           #:simple-prompt-stack
           #:*prompt*
           #:render-current-prompt
           #:notify-repl-prompt-changed
           #:prompt-table
           #:current-package
           #:current-time))

(in-package :swank-prompt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (swank-require :swank-repl))

(defun notify-repl-prompt-changed ()
  (send-to-emacs `(:repl-request-prompt-refresh)))

(defgeneric render-prompt (value stream &key &allow-other-keys)
  (:documentation
   "Print VALUE to STREAM as part of a REPL prompt")
  (:method (o stream &key &allow-other-keys)
    "Default method that write the object to stream"
    (write o :stream stream :circle t :escape nil))
  (:method ((o function) stream &key &allow-other-keys)
    "Functions are called to produce a value to render"
    (render-prompt (funcall o) stream))
  (:method ((o symbol) stream &key style &allow-other-keys)
    "An FBOUND symbol is called to produce a value to render"
    (case style
      (:key (write o :stream stream :escape t))
      (t (if (fboundp o)
             (render-prompt (funcall o) stream :style style)
             (case style
               (:non-nil (and o (call-next-method)))))))))

(defstruct prompt-command name function)

(defmacro command (function &optional (name function))
  `(make-prompt-command :name ,name :function ,function))

(defgeneric on-prompt-event (prompt event &key &allow-other-keys)
  (:documentation "Messaging with the prompt")
  (:method (p e &key &allow-other-keys)
    (warn "ignored event ~s for prompt ~s" e p)))

(defclass simple-prompt ()
  ((stack
    :documentation "A stack of values (e.g. your environment)"
    :accessor simple-prompt-stack
    :initform nil
    :initarg :stack)
   (plist
    :documentation "A property list of values (e.g. your inventory)"
    :accessor simple-prompt-plist
    :initform nil
    :initarg :plist)))

(defun current-package (&aux (p *package*))
  (or (first (package-nicknames p))
      (package-name p)))

(defun current-time ()
  (multiple-value-bind (s m h) (get-decoded-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" h m s)))

(defvar *prompt*
  (make-instance 'simple-prompt
                 :plist '()
                 :stack '(current-time current-package)))

(defun simple-prompt-value (key &optional (prompt *prompt*))
  (getf (simple-prompt-plist prompt) key))

(defun (setf simple-prompt-value) (val key &optional (prompt *prompt*))
  (with-slots (plist) prompt
    (prog1 val
      (if val
          (setf (getf plist key) val)
          (remf plist key)))))

(defmethod render-prompt ((p simple-prompt) stream &key &allow-other-keys)
  (with-slots (stack plist) p
    (pprint-logical-block (stream plist)
      (let ((stack (reverse stack)))
        (loop (unless stack (return))
              (render-prompt (pop stack) stream)
              (when stack (write-string " " stream))))
      (pprint-exit-if-list-exhausted)
      (pprint-newline :mandatory stream)
      (loop (write (pprint-pop) :stream stream)
            (write-string ": " stream)
            (render-prompt (pprint-pop) stream)
            (pprint-newline :mandatory stream)
            (pprint-exit-if-list-exhausted))))
  (format stream "> "))

(defun render-current-prompt (package)
  (handler-case (with-output-to-string (out)
                  (let ((*package* (or (find-package package)
                                       (find-package :cl-user))))
                    (render-prompt *prompt* out)))
    (error (e) (format nil "ERROR ~a >" e))))

;; UTILS

(defstruct prompt-table plist)

(defun prompt-table (&rest plist)
  (make-prompt-table :plist plist))

(defmethod render-prompt ((o prompt-table) stream &key &allow-other-keys)
  (format stream
          "~<~@{~a ~s~^~:@_~}~:>"
          (prompt-table-plist o)))

(defmethod render-prompt ((o prompt-command) stream &key &allow-other-keys)
  (with-slots (name function) o
    (let ((title (format nil "{command ~(~a~)}" name))
          (doc (documentation function 'function)))
      (with-input-from-string (in (or doc title))
        (write-string (read-line in) stream)))))

(provide :swank-prompt)

;; it is possible to have automatic retention of *,**,*** values in the prompt
;; based on the type of values
