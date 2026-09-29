(in-package :cl-user)
(defpackage clack.handler
  (:use :cl)
  (:import-from :clack.util
                :find-handler)
  (:import-from :bordeaux-threads
                :threadp
                :make-thread
                :thread-alive-p
                :join-thread
                :destroy-thread)
  (:import-from :usocket)
  (:export :run
           :stop))
(in-package :clack.handler)

(defstruct handler
  server
  swank-port
  acceptor)

(defun run (app server &rest args
                &key (address nil address-specified-p) use-thread
                     (swank-interface "127.0.0.1") swank-port debug
                     silent
                &allow-other-keys)
  (declare (ignorable swank-interface))
  (let ((handler-package (find-handler server))
        (bt2:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                          (*error-output* . ,*error-output*)
                                          ,@bt2:*default-special-bindings*)))
    (when (and debug
               (not silent))
      (format t "NOTICE: Running in debug mode. Debugger will be invoked on errors.
  Specify ':debug nil' to turn it off on remote environments."))
    (flet ((run-server ()
             (when swank-port
               #+swank
               (swank:create-server :interface swank-interface :port swank-port :dont-close t)
               #-swank
               (error "Swank is required to use :swank-port but is not loaded. Load SLIME or Swank first."))
             (apply (intern #.(string '#:run) handler-package)
                    app
                    :allow-other-keys t
                    (append
                      (and address-specified-p
                           (list :address
                                 (usocket:host-to-hostname
                                   (usocket:get-host-by-name address))))
                      args))))
      (make-handler
        :server server
        :swank-port swank-port
        :acceptor (if use-thread
                      (bt2:make-thread #'run-server
                                       :name (format nil "clack-handler-~(~A~)" server)
                                       :initial-bindings
                                       `((bt2:*default-special-bindings* . ',bt2:*default-special-bindings*)
                                         ,@bt2:*default-special-bindings*))
                      (run-server))))))

(defun stop-thread-gracefully (handler acceptor)
  (let* ((package (find-handler (handler-server handler)))
         (stop-symbol (find-symbol "STOP-GRACEFULLY" package)))
    (when (and stop-symbol (fboundp stop-symbol))
      ;; The handler thread can still be starting when STOP is called.
      (let ((requested
              (loop repeat 100
                    while (bt2:thread-alive-p acceptor)
                    when (funcall stop-symbol acceptor) return t
                    do (sleep 0.01))))
        (when requested
          ;; Woo can spend ten seconds stopping its worker cluster. Leave
          ;; time for event-loop and socket cleanup before the fallback.
          (loop repeat 1500 while (bt2:thread-alive-p acceptor)
                do (sleep 0.01))
          (unless (bt2:thread-alive-p acceptor)
            (bt2:join-thread acceptor)
            t))))))

(defun stop (handler)
  (let ((acceptor (handler-acceptor handler))
        (swank-port (handler-swank-port handler)))
    (if (bt2:threadp acceptor)
        (unless (stop-thread-gracefully handler acceptor)
          (when (bt2:thread-alive-p acceptor)
            (bt2:destroy-thread acceptor))
          (sleep 0.5))
        (let ((package (find-handler (handler-server handler))))
          (funcall (intern #.(string '#:stop) package) acceptor)))
    (when swank-port
      #+swank
      (swank:stop-server swank-port))
    t))
