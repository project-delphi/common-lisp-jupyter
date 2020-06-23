(in-package #:jupyter-widgets)

(defparameter +protocol-version+ "2.0.0")
(defparameter +base-module+ "@jupyter-widgets/base")
(defparameter +base-module-version+ "1.2.0")
(defparameter +controls-module+ "@jupyter-widgets/controls")
(defparameter +controls-module-version+ "1.5.0")
(defparameter +output-module+ "@jupyter-widgets/output")
(defparameter +output-module-version+ "1.0.0")
(defparameter +sidecar-module+ "@jupyter-widgets/jupyterlab-sidecar")
(defparameter +sidecar-module-version+ "1.0.0")

(defparameter +target-name+ "jupyter.widget")

(defvar *state-lock* nil)
(defvar *widgets* (make-hash-table :test 'equal))

(defun extract-major-version (semver)
  (let* ((v (string-left-trim '(#\0) semver)))
    (subseq v 0 (position #\. v :start (if (equal (char v 0) #\.) 1 0)))))

(defun widget-registry-name (model-module model-module-version model-name
                             view-module view-module-version view-name)
  (when (and model-module model-module-version model-name view-module
             view-module-version view-name)
    (format nil "~A+~A+~A+~A+~A+~A"
      model-module
      (extract-major-version model-module-version)
      model-name
      view-module
      (extract-major-version view-module-version)
      view-name)))

(defmacro register-widget (name)
  `(let ((class (find-class (quote ,name))))
    (closer-mop:finalize-inheritance class)
    (let ((initargs (closer-mop:compute-default-initargs class)))
      (flet ((def-initarg (slot-name)
                ; CMUCL appears to have the default initarg list in a different order.
                (eval (#+cmucl third #-cmucl second (assoc slot-name initargs)))))
        (when-let ((name (widget-registry-name (def-initarg :%model-module)
                                               (def-initarg :%model-module-version)
                                               (def-initarg :%model-name)
                                               (def-initarg :%view-module)
                                               (def-initarg :%view-module-version)
                                               (def-initarg :%view-name))))
          (setf (gethash name *widgets*) (quote ,name)))))))

(defclass widget (has-traits jupyter:comm jupyter:result)
  ((%model-name
     :initarg :%model-name
     :reader widget-%module-name
     :documentation "Name of the model."
     :trait :unicode)
   (%model-module
     :initarg :%model-module
     :reader widget-%module-module
     :documentation "The namespace for the model."
     :trait :unicode)
   (%model-module-version
     :initarg :%model-module-version
     :reader widget-%module-module-version
     :documentation "A semver requirement for namespace version containing the model."
     :trait :unicode)
   (%view-name
     :initarg :%view-name
     :reader widget-%view-name
     :documentation "Name of the view."
     :trait :unicode)
   (%view-module
     :initarg :%view-module
     :reader widget-%view-module
     :documentation "The namespace for the view."
     :trait :unicode)
   (%view-module-version
     :initarg :%view-module-version
     :reader widget-%view-module-version
     :documentation "A semver requirement for namespace version containing the view."
     :trait :unicode))
  (:metaclass trait-metaclass)
  (:default-initargs
    :display-data t
    :target-name +target-name+)
  (:documentation "Base class for all Jupyter widgets."))

(defmethod jupyter:render ((w widget))
  (jupyter:json-new-obj
    ("text/plain" "A Jupyter Widget")
    ("application/vnd.jupyter.widget-view+json"
      (jupyter:json-new-obj
        ("version_major" 2)
        ("version_minor" 0)
        ("model_id" (jupyter:comm-id w))))))

(defmethod to-json-state (w &optional nm)
  (iter
    (with state = (jupyter:json-empty-obj))
    (for def in (closer-mop:class-slots (class-of w)))
    (for name next (closer-mop:slot-definition-name def))
    (for trait-name next (trait-name name))
    (for type next (trait-type def))
    (when (and (or (not nm) (eql trait-name nm))
               (slot-boundp w name)
               type)
      (jupyter:json-extend-obj state
        ((symbol-to-snake-case name)
          (serialize-trait w type trait-name (slot-value w name)))))
    (finally (return state))))


(defun extract-buffers (state &optional path)
  (let (buffer-paths buffers)
    (cond
      ((hash-table-p state)
        (maphash (lambda (k v)
                   (cond
                     ((and (vectorp v) (equal (array-element-type v) '(unsigned-byte 8)))
                       (setq buffer-paths (append buffer-paths (list (append path (list k)))))
                       (setq buffers (append buffers (list v)))
                       (remhash k state))
                     (t
                       (multiple-value-bind (sub-buffer-paths sub-buffers) (extract-buffers v (append path (list k)))
                         (setq buffer-paths (append buffer-paths sub-buffer-paths))
                         (setq buffers (append buffers sub-buffers))))))
                 state))
      ((listp state)
        (do* ((tail state (cdr state))
              (i 0 (1+ i))
              (v (car tail)))
             ((null tail))
          (cond
            ((and (vectorp v) (equal (array-element-type v) '(unsigned-byte 8)))
              (setq buffer-paths (append buffer-paths (list (append path (list i)))))
              (setq buffers (append buffers (list v)))
              (rplaca tail :null))
            (t
              (multiple-value-bind (sub-buffer-paths sub-buffers) (extract-buffers v (append path (list i)))
                (setq buffer-paths (append buffer-paths sub-buffer-paths))
                (setq buffers (append buffers sub-buffers))))))))
    (values buffer-paths buffers)))

(defun inject-buffer (state buffer-path buffer)
  (let ((node (car buffer-path))
        (rest (cdr buffer-path)))
    (if rest
      (inject-buffer (if (stringp node)
                       (gethash node state)
                       (elt state node))
                     rest buffer)
      (if (stringp node)
        (setf (gethash node state) buffer)
        (setf (elt state node) buffer)))))

(defun inject-buffers (state buffer-paths buffers)
  (iter
    (for buffer-path in buffer-paths)
    (for buffer in buffers)
    (inject-buffer state buffer-path buffer)))

(defun send-state (w &optional name)
  (let ((state (to-json-state w name)))
    (multiple-value-bind (buffer-paths buffers) (values nil nil);(extract-buffers state)
      (jupyter:send-comm-message w
        (jupyter:json-new-obj ("method" "update")
                      ("state" state)
                      ("buffer_paths" (or buffer-paths :empty-array)))
        (jupyter:json-new-obj ("version" +protocol-version+))
        buffers))))

(defun update-state (w data buffers)
  (let ((*trait-source* nil)
        (state (gethash "state" data (make-hash-table :test #'equal)))
        (buffer-paths (gethash "buffer_paths" data)))
    (inject-buffers state buffer-paths buffers)
    (dolist (def (closer-mop:class-slots (class-of w)))
      (let ((name (closer-mop:slot-definition-name def)))
        (multiple-value-bind (value present-p)
                             (gethash (symbol-to-snake-case name) state)
          (when present-p
            (setf (slot-value w name)
                  (deserialize-trait w (trait-type def) (trait-name name) value))))))))

(defun send-custom (widget content &optional buffers)
  (jupyter:send-comm-message widget
    (jupyter:json-new-obj ("method" "custom")
                  ("content" content))
    (jupyter:json-new-obj ("version" +protocol-version+))
    buffers))

(defgeneric on-custom-message (widget content buffers))

(defmethod on-custom-message (widget content buffers))

(defmethod jupyter:on-comm-message ((w widget) data metadata buffers)
  (declare (ignore metadata))
  (switch ((gethash "method" data) :test #'equal)
    ("update"
      (update-state w data buffers))
    ("request_state"
      (send-state w))
    ("custom"
      (on-custom-message w (gethash "content" data) buffers))
    (otherwise
      (call-next-method))))

(defmethod on-trait-change :after ((w widget) type name old-value new-value source)
  (when source
    (send-state w name)))

(defmethod initialize-instance :around ((instance widget) &rest rest &key &allow-other-keys)
  (with-trait-silence
    (prog1
      (call-next-method)
      (unless (getf rest :create-comm)
        (let ((state (to-json-state instance)))
          (multiple-value-bind (buffer-paths buffers) (extract-buffers state)
            (jupyter:send-comm-open instance
              `(("state" . ,state)
                ("buffer_paths" . ,buffer-paths))
              `(("version" . ,+protocol-version+))
              buffers)))))))

(defmethod jupyter:create-comm ((target-name (eql :|jupyter.widget|)) id data metadata buffers)
  (let* ((state (gethash "state" data))
         (model-name (gethash "_model_name" state))
         (model-module (gethash "_model_module" state))
         (model-module-version (gethash "_model_module_version" state))
         (view-name (gethash "_view_name" state))
         (view-module (gethash "_view_module" state))
         (view-module-version (gethash "_view_module_version" state))
         (name (widget-registry-name model-module model-module-version
                                     model-name view-module
                                     view-module-version view-name))
         (class (gethash name *widgets*)))
    (when class
      (with-trait-silence
        (let ((w (make-instance class :create-comm t)))
          (update-state w data buffers)
          w)))))

(defun observe (instance name/s handler)
  (setf (widget-on-trait-change instance)
        (nconc (widget-on-trait-change instance)
               (if (listp name/s)
                 (mapcar (lambda (name) (cons name handler)) name/s)
                 (list (cons name/s handler))))))

(defgeneric %display (widget &rest args &key &allow-other-keys)
  (:documentation "Prepare widget for display")
  (:method (widget &rest args &key &allow-other-keys)
    (declare (ignore args))
    widget))

(defun display (widget &rest args &key &allow-other-keys)
  (jupyter:send-result (apply #'%display widget args))
  nil)
