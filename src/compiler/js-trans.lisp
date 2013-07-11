;;;; Perform various transformations on the generated JS AST

;;; JSCL is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; JSCL is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with JSCL.  If not, see <http://www.gnu.org/licenses/>.

;; List of the functions that can be applied to transform the AST
(defparameter *js-transforms* ())

;; Special value for a transform to return to indicate that the AST supplied
;; should be removed entirely
(defvar *empty* (gensym))

(defstruct js-transform
  appliesp ; Predicate checking if the transform applies
  func)    ; The transform function itself

(defmacro define-js-transform (pattern ast-name &body body)
  "Define a new transformation which matches against PATTERN, and returns
   the value of BODY when it applies"
  `(push (make-js-transform
           :appliesp (parse-pattern ',pattern)
           :func (lambda (,ast-name)
                   ,@body))
         *js-transforms*))

(defun parse-pattern (pattern)
  "Parse PATTERN, returning a predicate which matches against it"
  (let ((ast (gensym)))
    (eval `(lambda (,ast)
             (and (not (atom ,ast))
                  ,@(loop for item in pattern
                          for i from 0 collecting
                          (ecase (car item)
                            (quote ; Checks for a literal symbol
                              `(equal (elt ,ast ,i) ,item))))
                  (= (length ,ast) ,(length pattern)))))))

;;; TODO: Return a value indicating whether a transformation took place
;;; so that the caller doesn't have to do an O(n) EQUAL check between the
;;; old one and the new one
(defun apply-js-transform (ast js-transform)
  "Apply TRANSFORM to AST, or any of its subforms, returning the new form
   or a copy of the form if no transformation took place."
  (with-slots (appliesp func) js-transform
    (labels ((%apply-js-transform (ast)
               (cond
                 ((atom ast)
                  ast)
                 ((funcall appliesp (car ast))
                  ;; TODO: Shared structure matters?
                  (let ((new-ast (funcall func (car ast))))
                    (if (eql new-ast *empty*)
                      (%apply-js-transform (cdr ast))
                      (cons new-ast (%apply-js-transform (cdr ast))))))
                 (t
                  (cons (car ast) (%apply-js-transform (cdr ast)))))))
      (if (funcall appliesp ast)
        (funcall func ast)
        (%apply-js-transform ast)))))

(defun apply-all-js-transforms (ast)
  (loop for trans in *js-transforms*
        for new-ast = (apply-js-transform ast trans)
        when (not (equal new-ast ast))
          return (apply-all-js-transforms new-ast)
        finally (return new-ast)))

(define-js-transform ('progn) ast
  (declare (ignore ast))
  *empty*)
