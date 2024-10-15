;;; slime-prompt.el --- Slime REPL prompt function that calls backend
;;;
;;; Authors: Christophe Junke  <deftransform@proton.me>
;;;
;;; License: This code has been placed in the Public Domain.  All warranties
;;;          are disclaimed.

(require 'slime)

(define-slime-contrib slime-prompt
  "Call Lisp backend to compute prompt string"
  (:slime-dependencies slime-repl)
  (:swank-dependencies swank-prompt)
  (:on-load (slime-prompt--enable))
  (:on-unload (slime-prompt--disable)))

(defun slime-prompt--enable ()
  (setf slime-repl-prompt-function 'slime-prompt--render))

(defun slime-prompt--disable ()
  (setf slime-repl-prompt-function nil))

(defun slime-prompt--render (package)
  (slime-eval `(swank-prompt:render-current-prompt ,package)))

(provide 'slime-prompt)
