;;
;;  Triangle
;;
;;  Copyright 2012,2013 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(defpackage :RoL-template
  (:nicknames :L>template :lowh.triangle.template)
  (:use :cl :str)
  (:export
   #:*template-output*
   #:*template-vars-package*
   #:define-template-var
   #:template-let
   #:template-var
   #:template-var-key
   #:read-template
   #:compile-template
   #:compile-template-from-file
   #:clear-template-cache
   #:print-template))

(in-package :RoL-template)

;;  Environment

(defparameter *template-vars-package*
  (defpackage :RoL-template.vars
    (:nicknames :L>template.vars)))

(defun template-var-key (var)
  (intern (symbol-name var) *template-vars-package*))

(defmacro template-var (name)
  (template-var-key name))

(defun template-var-p (var)
  (and (symbolp var)
       (eq (symbol-package var)
           (find-package *template-vars-package*))))

(defmacro template-let (bindings &body body)
  "By default bind to an existing variable."
  (let ((bindings (mapcar (lambda (b)
                            (if (consp b)
                                b
                                (list b b)))
                          bindings)))
    `(let ,(mapcar (lambda (b) `(,(template-var-key (car b)) ,(cadr b)))
                   bindings)
       (declare (special ,@(mapcar (lambda (b) (template-var-key (car b)))
                                   bindings)))
       (symbol-macrolet ,(mapcar (lambda (b)
                                   `(,(car b) ,(template-var-key (car b))))
                                 bindings)
         ,@body))))

(defmacro define-template-var (name &optional (value nil value-p) doc)
  (let ((var (template-var-key name)))
    (if value-p
        `(defparameter ,var ,value ,@(when doc `(,doc)))
        (if doc
            `(progn (defvar ,var)
                    (setf (documentation ',var t) ,doc))
            `(defvar ,var)))))

;;  Template reader

(defvar *template-output* *standard-output*)
(defvar *template-vars*)
(defvar *nested*)
(defvar *writing*)

(flet ((read-plain (stream &optional char)
         (declare (ignore char))
         (let ((plain (with-output-to-string (out)
                        (loop for c = (peek-char nil stream nil #\« t)
                           until (char= #\« c)
                           do (write-char (read-char stream) out)))))
           (when (and *nested*
                      (char= #\« (peek-char nil stream nil #\Z t)))
             (read-char stream t nil t)
             (unless (char= #\Space (peek-char nil stream t nil t))
               (unread-char #\« stream)))
           (unless (= 0 (length plain))
             (if *writing*
                 plain
                 `(write-string ,plain *template-output*)))))
       (read-var (stream char)
         (let ((var (let ((*package* *template-vars-package*))
                      (read stream))))
           (assert (eq *template-vars-package* (symbol-package var)))
           (ecase char
             ( #\$ (car (push (the symbol var) *template-vars*))))))
       (dispatch-template-eval (stream c n)
         (declare (ignore c n))
         (assert (not *nested*) nil "Template parse error: Nested « ")
         (let ((*nested* t)) (read stream t nil t)))
       (dispatch-template-eval-print (stream c n)
         (declare (ignore c n))
         (assert (not *writing*) () "Template parse error: Nested «=")
         (let ((*nested* t)
               (*writing* t))
           `(write-string (str ,(read stream t nil t)) *template-output*))))
  (let ((rt (copy-readtable nil)))
    (make-dispatch-macro-character #\« t rt)
    (set-dispatch-macro-character #\« #\Space #'dispatch-template-eval rt)
    (set-dispatch-macro-character #\« #\= #'dispatch-template-eval-print rt)
    (set-macro-character #\» #'read-plain t rt)
    (set-macro-character #\$ #'read-var t rt)

    (defun read-template (stream)
      (let ((*readtable* (copy-readtable rt))
            (*nested* nil)
            (*writing* nil)
            (*template-vars* nil))
        (loop while (peek-char nil stream nil)
           for item = (read-plain stream) then (read stream)
           when item collect item)))))

(defun read-template-from-string (string)
  (with-input-from-string (s string)
    (read-template s)))

(defun read-template-from-file (path)
  (with-open-file (stream path
                          :element-type 'character
                          :external-format :utf-8)
    (read-template stream)))

;;  Compilation

(defmacro do-tree ((var tree) &body body)
  (let ((walk (gensym "WALK-")))
    `(labels ((,walk (,var)
                (if (consp ,var)
                    (progn (,walk (car ,var))
                           (,walk (cdr ,var)))
                    (progn ,@body))))
       (,walk ,tree))))

(defun collect-vars (sexp)
  (let (vars)
    (do-tree (x sexp)
      (when (and (symbolp x) (eq *template-vars-package* (symbol-package x)))
        (pushnew x vars :test #'eq)))
    (nreverse vars)))

(defun template-printer-code (template)
  (let ((vars))
    (labels ((walk (x)
               (if (consp x)
                   (cons (walk (car x)) (walk (cdr x)))
                   (if (template-var-p x)
                       (let ((g (assoc x vars)))
                         (if g
                             (cdr g)
                             (cdar (push (cons x (gensym
                                                  (symbol-name x)))
                                         vars))))
                       x))))
      (let ((body (walk template)))
        `(lambda ()
           ,@(if vars
                 `((let ,(mapcar (lambda (var)
                                   `(,(cdr var) (when (boundp
                                                       ',(car var))
                                                  (symbol-value
                                                   ',(car var)))))
                                 vars)
                     ,@body))
                 body))))))

#+nil
(template-printer-code (read-template-from-string "plop«= $abc »"))

(defun compile-template (template)
  (compile nil (template-printer-code template)))

;;  File-level cache

(defvar *template-pathname-defaults* *default-pathname-defaults*)

(defvar *template-cache*)

(defun clear-template-cache ()
  (setf *template-cache* (make-hash-table :test 'equal)))

(clear-template-cache)

(defun compile-template-from-file (path)
  (let ((found #1=(gethash path *template-cache*)))
    (if (and found
             (= (car found) #2=(file-write-date path)))
        (cdr found)
        (let ((template (compile-template (read-template-from-file
                                           path))))
          (format t "~&;; Compiling template ~S~%" path)
          (if found
              (setf (car found) #2#
                    (cdr found) template)
              (setf #1# (cons #2# template)))
          template))))

;;  Print template

(defun print-template (template)
  (funcall (etypecase template
             (pathname (compile-template-from-file template))
             (string (compile-template (read-template-from-string
                                        template))))))

(define-compiler-macro print-template (&whole form template)
  (if (stringp template)
      `(,(compile-template (read-template-from-string template)))
      form))
