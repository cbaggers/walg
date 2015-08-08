(in-package #:walg)

(defmacro deferror (name parent-condition (&rest args) error-string &body body)
  (unless (every #'symbolp args) (error "can only take simple args"))
  (labels ((kwd (x) (intern (format nil "~a" x) :keyword)))
    (let ((parent-condition (or parent-condition 'error)))
      (loop :for arg :in args :do
         (setf body (subst `(,arg condition) arg body :test #'eq)))
      `(define-condition ,name (,parent-condition)
         (,@(loop :for arg :in args :collect
               `(,arg :initarg ,(kwd arg) :reader ,arg)))
         (:report (lambda (condition stream)
                    (declare (ignorable condition))
                    (format stream ,error-string ,@body)))))))

(defun group-by (source n)
  "This takes a  flat list and emit a list of lists, each n long
   containing the elements of the original list"
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n)
				   acc))
		   (nreverse (cons source acc))))))
    (if source
	(rec source nil)
	nil)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun symb-package (package &rest args)
  (values (intern (apply #'mkstr args) package)))
