(in-package #:cl-jupyter)

(defclass kernel ()
  ((config :initarg :config
           :reader kernel-config)
   (ctx :initarg :ctx
        :reader kernel-ctx)
   (hb :initarg :hb
       :reader kernel-hb)
   (shell :initarg :shell
          :reader kernel-shell)
   (stdin :initarg :stdin
          :reader kernel-stdin)
   (iopub :initarg :iopub
          :reader kernel-iopub)
   (session :initarg :session
            :reader kernel-session)
   (evaluator :initarg :evaluator
              :reader kernel-evaluator))
  (:documentation "Kernel state representation."))

(defun make-kernel (config)
  (let ((ctx (pzmq:ctx-new))
        (session-id (format nil "~W" (uuid:make-v4-uuid))))
    (make-instance 'kernel
                   :config config
                   :ctx ctx
                   :hb (make-hb-channel config ctx)
                   :shell (make-shell-channel config ctx)
                   :stdin (make-stdin-channel config ctx)
                   :iopub (make-iopub-channel config ctx)
                   :session session-id
                   :evaluator (make-evaluator))))

(defun get-argv ()
  ;; Borrowed from apply-argv, command-line-arguments.  Temporary solution (?)
  #+sbcl (cdr sb-ext:*posix-argv*)
  #+clozure (cdr ccl:*command-line-argument-list*)
  #+gcl si:*command-args*
  #+ecl (loop for i from 0 below (si:argc) collect (si:argv i))
  #+cmu extensions:*command-line-strings*
  #+allegro (sys:command-line-arguments)
  #+lispworks sys:*line-arguments-list*
  #+clisp ext:*args*
  #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
  (error "get-argv not supported for your implementation"))

(defun join (e l)
  (cond ((endp l) (list))
        ((endp (cdr l)) l)
        (t (cons (car l) (cons e (join e (cdr l)))))))

(example (join 1 '(a b c d e))
         => '(a 1 b 1 c 1 d 1 e))

(defun concat-all (kind term ls)
  (if (endp ls)
      term
      (concatenate kind (car ls) (concat-all kind term (cdr ls)))))

(example (concat-all 'string "" '("a" "b" "c" "d" "e"))
         => "abcde")

(defun banner (stream)
  (format stream (concatenate 'string
                              "~A: an enhanced interactive Maxima REPL~%"
                              "(Version ~A - Jupyter protocol v.~A)~%"
                              "--> (C) 2014-2015 Frederic Peschanski (cf. LICENSE)~%")
          +KERNEL-IMPLEMENTATION-NAME+
          +KERNEL-IMPLEMENTATION-VERSION+
          +KERNEL-PROTOCOL-VERSION+))

(defclass kernel-config ()
  ((transport :initarg :transport :reader config-transport :type string)
   (ip :initarg :ip :reader config-ip :type string)
   (shell-port :initarg :shell-port :reader config-shell-port :type fixnum)
   (stdin-port :initarg :stdin-port :reader config-stdin-port :type fixnum)
   (iopub-port :initarg :iopub-port :reader config-iopub-port :type fixnum)
   (control-port :initarg :control-port :reader config-control-port :type fixnum)
   (hb-port :initarg :hb-port :reader config-hb-port :type fixnum)
   (signature-scheme :initarg :signature-scheme :reader config-signature-scheme :type string)
   (key :initarg :key :reader config-key)))

(defun make-kernel-config (connection-file-name)
  (let ((config-js (jsown:parse (concat-all 'string "" (read-file-lines connection-file-name)))))
    (make-instance 'kernel-config
                   :transport (jsown:val config-js "transport")
                   :ip (jsown:val config-js "ip")
                   :shell-port (jsown:val config-js "shell_port")
                   :stdin-port (jsown:val config-js "stdin_port")
                   :iopub-port (jsown:val config-js "iopub_port")
                   :control-port (jsown:val config-js "control_port")
                   :hb-port (jsown:val config-js "hb_port")
                   :signature-scheme (jsown:val config-js "signature_scheme")
                   :key (let ((str-key (jsown:val config-js "key")))
                          (if (string= str-key "")
                              nil
                              (babel:string-to-octets str-key :encoding :ASCII))))))

(defmethod stop ((k kernel))
  (stop (kernel-hb k))
  (stop (kernel-iopub k))
  (stop (kernel-shell k))
  (stop (kernel-stdin k))
  (pzmq:ctx-destroy (kernel-ctx k)))

(defun kernel-start (connection-file-name)
  (info (banner nil))
  (info "[Kernel] connection file = ~A~%" connection-file-name)
  (unless (stringp connection-file-name)
    (error "[Kernel] Wrong connection file argument (expecting a string)"))
  (let ((config (make-kernel-config connection-file-name)))
    (when (not (string= (config-signature-scheme config) "hmac-sha256"))
      ;; XXX: only hmac-sha256 supported
      (error "[Kernel] Signature scheme 'hmac-sha256' required, was provided ~S." (config-signature-scheme config)))
      ;;(inspect config)
    (iter
      (with kernel = (make-kernel config))
      (with iopub = (kernel-iopub kernel))
      (with shell = (kernel-shell kernel))
      (initially
        (info "[Kernel] Entering mainloop ...~%")
        (send-status-starting iopub (kernel-session kernel)))
      (for msg = (message-recv shell))
      (for msg-type = (jsown:val (message-header msg) "msg_type"))
      (while
        (cond ((equal msg-type "kernel_info_request")
         (handle-kernel-info-request kernel msg))
        ((equal msg-type "execute_request")
         (handle-execute-request kernel msg))
        ((equal msg-type "shutdown_request")
         (handle-shutdown-request kernel msg))
        ((equal msg-type "is_complete_request")
         (handle-is-complete-request kernel msg))
        (t
         (warn "[Shell] message type '~A' not (yet ?) supported, skipping..." msg-type)
         t)))
      (finally-protected
        (info "[Kernel] Exiting mainloop.~%")
        (stop kernel)))))

;; This is the entry point for a saved lisp image created by
;; trivial-dump-core:save-executable or equivalent.
(defun kernel-start-exec ()
  ;; IS THERE OTHER STUFF HANDLED BY MAXIMA INIT-CL.LISP THAT WE NEED TO DUPLICATE HERE ??
  (setq *read-default-float-format* 'double-float)
  (kernel-start (car (last (get-argv)))))

;; This is the entry point for starting the kernel from within an existing
;; Maxima session.
(maxima::defmfun maxima::$kernel_start (connection-file-name)
  (kernel-start connection-file-name))

#|


### Message type: kernel_info_reply ###

|#

(defun handle-kernel-info-request (kernel msg)
  (info "[Shell] handling 'kernel-info-request'~%")
  (message-send (kernel-shell kernel)
    (make-message msg "kernel_info_reply"
      (jsown:new-js
        ("protocol_version" (jsown:val (message-header msg) "version"))
        ("implementation" +KERNEL-IMPLEMENTATION-NAME+)
        ("implementation_version" +KERNEL-IMPLEMENTATION-VERSION+)
        ("banner" (banner nil))
        ("help_links"
          (list
            (jsown:new-js
              ("text" "Maxima Reference Manual")
              ("url" "http://maxima.sourceforge.net/docs/manual/maxima.html"))
            (jsown:new-js
              ("text" "Maxima Documentation")
              ("url" "http://maxima.sourceforge.net/documentation.html"))))
        ("language_info"
          (jsown:new-js
            ("name" "maxima")
            ("version" maxima::*autoconf-version*)
            ("mimetype" "text/x-maxima")
            ("pygments_lexer" "maxima")
            ("codemirror_mode" "maxima")))))))
#|

### Message type: execute_request ###

|#

(let (execute-request-kernel execute-request-msg)

  (defun handle-execute-request (kernel msg)
    (info "[Shell] handling 'execute_request'~%")
    (let* ((shell (kernel-shell kernel))
           (iopub (kernel-iopub kernel))
           (content (message-content msg))
           (code (jsown:val content "code")))
      (send-status-update iopub msg "busy")
      (setq execute-request-kernel kernel)
      (setq execute-request-msg msg)
      ;;(info "  ===> Code to execute = ~W~%" code)
      (vbinds (execution-count results stdout stderr)
              (evaluate-code (kernel-evaluator kernel) code)
        ;(info "Execution count = ~A~%" execution-count)
        ;(info "results = ~A~%" results)
        ;(info "STDOUT = ~A~%" stdout)
        ;(info "STDERR = ~A~%" stderr)
        ;broadcast the code to connected frontends
        (send-execute-code iopub msg execution-count code)
        ;; send the stdout
        (when (and stdout (> (length stdout) 0))
              (send-stream iopub msg "stdout" stdout))
        ;; send the stderr
        (when (and stderr (> (length stderr) 0))
              (send-stream iopub msg "stderr" stderr))
        ;; send the results
        (dolist (result results)
          (cond ((eval-error-p result)
                 (send-execute-error iopub msg execution-count (caddr result) (cadddr result)))
                ((eq (caar result) 'maxima::displayinput)
                 (send-execute-result iopub msg execution-count (caddr result)))))
        ;; status back to idle
        (send-status-update iopub msg "idle")
        ;; send reply (control)
        (let ((errors (remove-if-not #'eval-error-p results)))
          (if errors
            (let ((ename (format nil "~{~A~^, ~}" (mapcar #'caddr errors)))
                  (evalue (format nil "~{~A~^, ~}" (mapcar #'cadddr errors))))
              (send-execute-reply-error shell msg execution-count ename evalue))
            (send-execute-reply-ok shell msg execution-count)))
        ;; return t if there is no quit errors present
        (notany #'quit-eval-error-p results))))

  ;; Redefine RETRIEVE in src/macsys.lisp to make use of input-request/input-reply.
  ;; MSG, FLAG, and PRINT? are declared special there, so be careful to
  ;; refer to those symbols in the :maxima package.

  (defun maxima::retrieve (maxima::msg maxima::flag &aux (maxima::print? nil))
    (declare (special maxima::msg maxima::flag maxima::print?))
    (or (eq maxima::flag 'maxima::noprint) (setq maxima::print? t))
    (let* ((retrieve-prompt (cond ((not maxima::print?)
                                   (setq maxima::print? t)
                                   (format nil ""))
                                  ((null maxima::msg)
                                   (format nil ""))
                                  ((atom maxima::msg)
                                   (format nil "~A" maxima::msg))
                                  ((eq maxima::flag t)
                                   (format nil "~{~A~}" (cdr maxima::msg)))
                                  (t
                                   (maxima::aformat nil "~M" maxima::msg))))
           (stdin (kernel-stdin execute-request-kernel)))
      (send-input-request stdin execute-request-msg retrieve-prompt)
      (let* ((msg (message-recv stdin))
             (content (message-content msg))
             (value (jsown:val content "value")))
        (maxima::mread-noprompt (make-string-input-stream (add-terminator value)) nil)))))

#|

### Message type: shutdown_request ###

|#

(defun handle-shutdown-request (kernel msg)
  (info "[Shell] handling 'shutdown_request'~%")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (restart (jsown:val content "restart")))
    (send-shutdown-reply shell msg restart)
    nil))

#|

### Message type: is_complete_request ###

|#

(defun handle-is-complete-request (kernel msg)
  (info "[Shell] handling 'is_complete_request'~%")
  (let* ((shell (kernel-shell kernel))
         (content (message-content msg))
         (code (jsown:val content "code"))
         (status (if (ends-with-terminator code)
                     "complete"
                     "incomplete")))
    (send-is-complete-reply shell msg status)
    t))
