(in-package #:walg)

;;-------------------------------------------------------------
;; Definitions

;; abstract syntax tree nodes
(defclass-triv ~lambda () v body)
(defclass-triv ~ident () name)
(defclass-triv ~apply () fn arg)
(defclass-triv ~let () v defn body)
(defclass-triv ~let-rec () v defn body)


(defun ~lambda (v body) (make-instance '~lambda :v v :body body))
(defun ~ident (name) (make-instance '~ident :name name))
(defun ~apply (fn arg) (make-instance '~apply :fn fn :arg arg))
(defun ~let (v defn body) (make-instance '~let :v v :defn defn :body body))
(defun ~let-rec (v defn body) (make-instance '~let-rec :v v :defn defn :body body))


(defmethod print-object ((x ~let-rec) stream)
  (format stream "(~~let-rec ~s ~s ~s)" (v x) (defn x) (body x)))

(defmethod print-object ((x ~let) stream)
  (format stream "(~~let ~s ~s ~s)" (v x) (defn x) (body x)))

(defmethod print-object ((x ~lambda) stream)
  (format stream "(~~lambda ~s ~s)" (v x) (body x)))

(defmethod print-object ((x ~apply) stream)
  (format stream "(~~apply ~s ~s)" (fn x) (arg x)))

(defmethod print-object ((x ~ident) stream)
  (format stream "(~~ident ~s)" (name x)))


;; exception types
(deferror ~type-error () (reason)
    "Type inference algorithm cannot infer type.~%Reason: ~a" reason)

(deferror ~parse-error () (reason)
    "Type environment supplied for is incomplete.~%Reason: ~a" reason)


;; types and type constructors
(let ((next-variable-id -1))
  (defun get-next-variable-id ()
    (incf next-variable-id)))

(let ((next-variable-code (1- (char-code #\a))))
  (defun get-next-variable-name ()
    (format nil "~a" (code-char (incf next-variable-code)))))

;; type-variable
(defclass-triv ~type-variable () id name instance)

(defmethod initialize-instance :after ((x ~type-variable) &key)
  (setf (name x) nil
        (instance x) nil
        (id x) (get-next-variable-id)))

(defun ~type-variable ()
  (make-instance '~type-variable))

(defmethod get-name (x ~type-variable)
  (if (name x)
      (name x)
      (setf (id x) (get-next-variable-name))))

(defmethod print-object ((obj ~type-variable) stream)
  (format stream "#<type-variable :id ~a~@[ :instance ~a~]~@[ :name ~a~]>"
          (id obj) (instance obj) (name obj)))

;; type-operator
(defclass-triv ~type-operator () name types)

(defun ~type-operator (name &rest types)
  (make-instance '~type-operator :name name :types types))

(defmethod print-object ((obj ~type-operator) stream)
  (format stream "#<type-operator :name ~a~@[ :types ~a~]>"
          (name obj) (types obj)))

;; function
(defclass-triv ~function (~type-operator))

(defun ~function (from-type to-type)
  (make-instance '~function :from-type from-type :to-type to-type))

(defmethod initialize-instance ((obj ~function) &key from-type to-type)
  (setf (name obj) :->
        (types obj) `(,from-type ,to-type)))

;; basic types are constructed with a nullary type constructor

(defvar ~integer (make-instance '~type-operator :name "int" :types nil))
(defvar ~bool (make-instance '~type-operator :name "bool" :types nil))

;; environment
(defclass-triv ~env () (internal :initform (make-hash-table)))

(defun %n-add-elements-to-env (env elements)
  (loop :for (k v) :in elements :do (setf (gethash k (internal env)) v))
  env)

(defmethod make-env (&rest initial-elements)
  (%n-add-elements-to-env (make-instance '~env) (group-by initial-elements 2)))

(defmethod env-get (env key)
  (gethash key (internal env)))

(defmethod (setf env-get) (value env key)
  (setf (gethash key (internal env)) value))

(defmethod copy-env (env &rest additions)
  (let ((new-env (make-instance '~env))
        (additions (group-by additions 2)))
    (maphash (lambda (k v) (setf (gethash k (internal new-env)) v))
             (internal env))
    (%n-add-elements-to-env new-env additions)
    new-env))

;;-------------------------------------------------------------
;; Type inference machinery

(defun ~analyse (node env &optional non-generic)
  "Computes the type of the expression given by node.

    The type of the node is computed in the context of the  supplied type
    environment env. Data types can be introduced into the  language simply by
    having a predefined set of identifiers in the initial environment.
    environment; this way there is no need to change the syntax or,
    more importantly, the type-checking program when extending the language.

    Args:
        node: The root of the abstract syntax tree.
        env: The type environment is a mapping of expression identifier names
            to type assignments.
            to type assignments.
        non_generic: A set of non-generic variables, or None

    Returns:
        The computed type of the expression.

    Raises:
        TypeError: The type of the expression could not be inferred, for example
            if it is not possible to unify two types such as Integer and Bool
        ParseError: The abstract syntax tree rooted at node could not be parsed"
  (typecase node
    (~ident (~get-type (name node) env non-generic))
    (~apply (let ((result-type (make-instance '~type-variable)))
              (~unify (make-instance
                       '~function
                       :from-type (~analyse (arg node) env non-generic)
                       :to-type result-type)
                      (~analyse (fn node) env non-generic))
              result-type))
    (~lambda (let* ((arg-type (make-instance '~type-variable)))
               (make-instance
                '~function
                :from-type arg-type
                :to-type (~analyse (body node)
                                   (copy-env env (v node) arg-type)
                                   (union (list arg-type)
                                          (copy-seq non-generic))))))
    (~let (let ((defn-type (~analyse (defn node) env non-generic)))
            (~analyse (body node)
                      (copy-env env (v node) defn-type)
                      non-generic)))
    (~let-rec (let* ((new-type (~type-variable))
                     (new-env (copy-env env (v node) new-type))
                     (new-non-generic (union (list new-type)
                                             (copy-seq non-generic)))
                     (defn-type (~analyse (defn node)
                                          new-env
                                          new-non-generic)))
                (~unify new-type defn-type)
                (~analyse (body node) new-env non-generic)))
    (otherwise (error "Unhandled syntax node ~s" node))))

(defun ~get-type (name env non-generic)
  "Get the type of identifier name from the type environment env.

    Args:
        name: The identifier name
        env: The type environment mapping from identifier names to types
        non_generic: A set of non-generic TypeVariables

    Raises:
        ParseError: Raised if name is an undefined symbol in the type
            environment."
  (cond
    ((env-get env name) (~fresh (env-get env name) non-generic))
    ((~is-integer-literal name) ~integer)
    (t (error '~parse-error :reason (format nil "undefined symbol ~s" name)))))

(defun ~fresh (type non-generic)
  "Makes a copy of a type expression.

    type is copied. The the generic variables are duplicated and the
    non_generic variables are shared.

    Args:
        type: A type to be copied.
        non_generic: A set of non-generic TypeVariables"
  (let ((mappings (make-hash-table)))
    (labels ((fresh-rec (tp)
               (let* ((p (~prune tp)))
                 (typecase p
                   (~type-variable
                    (if (~is-generic p non-generic)
                        (or (gethash p mappings)
                            (setf (gethash p mappings)
                                  (make-instance '~type-variable)))
                        p))
                   (~type-operator
                    (make-instance '~type-operator
                                   :name (name p)
                                   :types (mapcar #'fresh-rec (types p))))
                   (otherwise nil))))) ;; assumed. maybe should be error?
      (fresh-rec type))))

;; UGH the side effects...cant we clean this up a bit?
(defun ~unify (type-a type-b)
  "Unify the two types type-a and type-b.

    Makes the types type-a and type-b the same.

    Args:
        type-a: The first type to be made equivalent
        type-b: The second type to be be equivalent

    Returns:
        None

    Raises:
        TypeError: Raised if the types cannot be unified."
  (let ((a (~prune type-a))
        (b (~prune type-b)))
    (cond
      ((typep a '~type-variable)
       (when (not (equal a b))
         (when (~occurs-in-type a b)
           (error '~type-error :reason "recursive unification"))
         (setf (instance a) b)))
      ((and (typep a '~type-operator) (typep b '~type-variable))
       (~unify b a))
      ((and (typep a '~type-operator) (typep b '~type-operator))
       (when (or (not (equal (name a) (name b)))
                 (not (= (length (types a)) (length (types b)))))
         (error '~type-error
                :reason (format nil "type mismatch (type-equal ~s ~s" a b)))
       (mapcar #'~unify (types a) (types b)))
      (t (error "Could not unify types ~a ~a (~a ~a)"
                type-a type-b a b)))))

(defun ~prune (type)
  "Returns the currently defining instance of type.

    As a side effect, collapses the list of type instances. The function Prune
    is used whenever a type expression has to be inspected: it will always
    return a type expression which is either an uninstantiated type variable or
    a type operator; i.e. it will skip instantiated variables, and will
    actually prune them from expressions to remove long chains of instantiated
    variables.

    Args:
        type: The type to be pruned

    Returns:
        An uninstantiated TypeVariable or a TypeOperator"
  (if (and (typep type '~type-variable)
           (instance type))
      (setf (instance type) (~prune (instance type)))
      type))

(defun ~is-generic (v non-generic)
  "Checks whether a given variable occurs in a list of non-generic variables

    Note that a variables in such a list may be instantiated to a type term,
    in which case the variables contained in the type term are considered
    non-generic.

    Note: Must be called with v pre-pruned

    Args:
        v: The TypeVariable to be tested for genericity
        non_generic: A set of non-generic TypeVariables

    Returns:
        True if v is a generic variable, otherwise False"
  (not (~occurs-in v non-generic)))

(defun ~occurs-in-type (v type-2)
  "Checks whether a type variable occurs in a type expression.

    Note: Must be called with v pre-pruned

    Args:
        v:  The TypeVariable to be tested for
        type-2: The type in which to search

    Returns:
        True if v occurs in type-2, otherwise False"
  (let ((pruned-type-2 (~prune type-2)))
    (cond ((equal pruned-type-2 v) t)
          ((typep pruned-type-2 '~type-operator)
           (~occurs-in v (types pruned-type-2))))))

(defun ~occurs-in (type types)
  "Checks whether a types variable occurs in any other types.

    Args:
        type:  The TypeVariable to be tested for
        types: The sequence of types in which to search

    Returns:
        True if type occurs in any of types, otherwise False"
  (some #'(lambda (x) (~occurs-in-type type x)) types))

(defun ~is-integer-literal (name)
  "Checks whether name is an integer literal string.

    Args:
        name: The identifier to check

    Returns:
        True if name is an integer literal, otherwise False"
  (handler-case (and (parse-integer name) t)
    (parse-error () nil)))

;;-------------------------------------------------------------
;; Examples

(let* ((var1 (~type-variable))
             (var2 (~type-variable))
             (pair-type (~type-operator "*" var1 var2))
             (var3 (~type-variable))
             (my-env (make-env "pair" (~function var1 (~function var2 pair-type))
                               "true" ~bool
                               "cond" (~function ~bool (~function var3 (~function var3 var3)))
                               "zero" (~function ~integer ~bool)
                               "pred" (~function ~integer ~integer)
                               "times" (~function ~integer (~function ~integer ~integer)))))
  (let (;; (pair (~apply (~apply (~ident "pair")
        ;;                       (~apply (~ident "f") (~ident "4")))
        ;;               (~apply (~ident "f") (~ident "true"))))
        (example
         (~let-rec "factorial"
                   (~lambda "n"
                            (~apply
                             (~apply
                              (~apply (~ident "cond")
                                      (~apply (~ident "zero") (~ident "n")))
                              (~ident "1"))
                             (~apply
                              (~apply (~ident "times") (~ident "n"))
                              (~apply (~ident "factorial")
                                      (~apply (~ident "pred") (~ident "n"))))))
                   (~apply (~ident "factorial") (~ident "5")))))

    (defun test () (try-exp my-env example))))

(defun try-exp (env node)
  "Try to evaluate a type printing the result or reporting errors.

    Args:
        env: The type environment in which to evaluate the expression.
        node: The root node of the abstract syntax tree of the expression.

    Returns:
        None"
  (~analyse node env))
