(in-package #:message-oo)

(push :message-oo *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +mangle+ (find-package "MESSAGE-OO.MANGLE"))

  (defun method-name (message-list)
    "(:add item) -> message-oo.mangle:|ADD:|
   (:add item :at pos) -> message-oo.mangle:|ADD:AT:|
   :add -> message-oo.mangle:add
   (:add item &rest rest) -> message-oo.mangle:|ADD::|
   (:add item :at pos &rest rest) -> message-oo.mangle:|ADD:AT::|"
    (labels ((collect-string (list)
               (unless list
                 (return-from collect-string ""))
               (concatenate 'string (if (eq (car list) '&rest) 
                                        "" (string (car list))) 
                            ":" (collect-string (cddr list)))))                 
      (intern (if (consp message-list)
                  (collect-string message-list)
                  (string message-list))
              +mangle+)))

  (defun method-params (message-list)
    (when (consp message-list)
      (if (eq (car message-list) '&rest) message-list
          (cons (second message-list) (method-params (cddr message-list)))))))

(defmacro defmessage (class-list message-list &body body)
   "It is smalltalk-like:
 (defmessage container (:add item :after old-item)
   (container-add-after container item old-item))"
   (destructuring-bind (class-name class-type) 
       (if (listp class-list) class-list (list class-list class-list))
   `(cl:defmethod 
        ,(method-name message-list) ((,class-name ,class-type) 
                                     . ,(method-params message-list))
      . ,body)))

(define-condition bad-message (error)
  ((message :initarg message)))

(defun find-message (message)
  (labels ((collect-string (list)
             (unless list
               (return-from collect-string ""))
             (unless (symbolp (car list))
               (return-from collect-string ":"))
             (concatenate 'string (string (car list))
                          ":" (collect-string (cddr list))))

           (next-name (name)
             "add:to: -> add:to::
              add:to:: -> add::
              add: -> error"
             (let* ((i (1- (length name)))
                    (end i))
               (unless (char= (char name i) #\:)
                 (error 'bad-message :message message))
               (decf i)
               (unless (char= (char name i) #\:)
                 (return-from next-name (concatenate 'string name ":")))
               (do () ((char= (char name i) #\:))
                 (decf i)
                 (when (< 0 i)
                   (error 'bad-message :message message)))
               (remove-if (constantly t) name :start (1+ i) :end end)))

           (collect-params (message name)
             (do ((i 0 (1+ i)) res was-colon)
                 ((= i (length name)) (nreverse res))
               (if (char= (char name i) #\:)
                   (progn
                     (when was-colon
                       (return-from collect-params 
                         (append (nreverse res) message)))
                     (push (second message) res)
                     (setf message (cddr message)
                           was-colon t))
                   (setf was-colon nil))))

           (try-find (name)
             (let ((generic (find-symbol name +mangle+)))
               (when generic
                 (return-from try-find 
                   (values generic (collect-params message name)))))
             (try-find (next-name name))))
    (try-find 
     (if (consp message)
         (collect-string message)
         (string message)))))


(defmacro @ (object message &rest more-messages)
    "Enables message pipelines
 (@ l (:map (lambda (x) (process x)))) == (mapcar ...)
 (@ l (:map func) (:reduce #'+)) == (reduce (mapcar ..))"
  (multiple-value-bind (generic params) (find-message message)
    (let ((res `(,generic ,object . ,params)))
      (if more-messages
          `(@ ,res . ,more-messages)
          res))))

;;; For first-class function likers

(defmessage list :generic (find-message list))
(defmessage list :fn 
  "Returns function that can be applied to object
Example (mapcar (@ '(:+ 3) :fn) (list 1 2 3 4)) => '(4 5 6 7)"
  (multiple-value-bind (generic params) (find-message list)
    (lambda (x) (apply generic x params))))
(defmessage (any t) (:tap func) 
  "For debugging like in 
 (@ obj (:meth1 ...) (:meth2 ...) (:tap (lambda (x) (print x))) (:meth3 ...))"
  (funcall func) any)

#|

 (defmessage container (:add item)
  {here you have vars container, item})

 (defmessage container (:add item :after old-item)
  ...)

May use another name

 (defmessage (container cl-container:ordered-list) (:add item)
  {here you have vars container, item})

May have &rest

 (defmessage (container cl-container:ordered-list) (:add item &rest rest)
  {here you have vars container, item, rest})

|#