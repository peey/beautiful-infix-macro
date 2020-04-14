(in-package :beautiful-infix-macro)

(defparameter *and-reducable-operators-list*
  '(< <= > >= = /= eq eql equal))

;; source: freenode user pjb in lisp channel
(defun possibly-valid-form-p (object) 
  (or (atom object)
      (and (symbolp (car object)) ;; checks for functions, macros & symbol operators
	   (or (fboundp (car object))
	       (macro-function (car object))
	       (special-operator-p (car object))))
      (and (consp (car object)) ;; checks for lambda expressions
	   (eq 'lambda (caar object))
	   (listp (cadar object)))))

(defmacro $$ (&rest list-of-expressions) ; walks through list tree and converts lists which aren't a suspected form from (...) to ($ ...)
  "Use parantheses for grouping, but with much much more complexity, confusion, and caveats"
  (if (reduce #'(lambda (accu element) (and accu (possibly-valid-form-p element))) list-of-expressions :initial-value t)
      ;; if free of nested infix forms
      (push 'ugly-tiny-infix-macro:$ list-of-expressions)
      ;; else
      (append '(ugly-tiny-infix-macro:$) (map 'list #'(lambda (element)
			      (if (possibly-valid-form-p element)
				  element
				  (push '$$ element))) list-of-expressions))))
