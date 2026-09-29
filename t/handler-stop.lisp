(in-package :cl-user)

(defpackage clack.handler.test
  (:use :cl)
  (:export :run
           :stop-gracefully))

(in-package :clack.handler.test)

(defvar *stop-requested* nil)
(defvar *started* nil)
(defvar *finished* nil)
(defvar *stop-calls* 0)

(defun run (app &rest args)
  (declare (ignore app args))
  (setf *started* t)
  (unwind-protect
       (loop while (not *stop-requested*)
             do (sleep 0.01))
    (setf *finished* t)))

(defun stop-gracefully (acceptor)
  (declare (ignore acceptor))
  (incf *stop-calls*)
  (setf *stop-requested* t)
  t)

(defpackage clack.handler.test-no-hook
  (:use :cl)
  (:export :run))

(in-package :clack.handler.test-no-hook)

(defun run (app &rest args)
  (declare (ignore app args))
  (loop do (sleep 0.01)))

(in-package :cl-user)
(defpackage clack-test.handler-stop
  (:use :cl :rove))
(in-package :clack-test.handler-stop)

(deftest threaded-stop-uses-handler-graceful-hook
  (setf clack.handler.test::*stop-requested* nil
        clack.handler.test::*started* nil
        clack.handler.test::*finished* nil
        clack.handler.test::*stop-calls* 0)
  (let ((handler (clack.handler:run (lambda (env)
                                      (declare (ignore env))
                                      '(200 nil ("ok")))
                                    :test
                                    :use-thread t)))
    (unwind-protect
         (let ((acceptor (clack.handler::handler-acceptor handler)))
           (ok (bt2:threadp acceptor))
           (loop repeat 200 while (not clack.handler.test::*started*)
                 do (sleep 0.01))
           (ok clack.handler.test::*started*)
           (clack.handler:stop handler)
           (ok (= 1 clack.handler.test::*stop-calls*))
           (ok clack.handler.test::*finished*)
           (ok (not (bt2:thread-alive-p acceptor))))
      (when (bt2:thread-alive-p (clack.handler::handler-acceptor handler))
        (bt2:destroy-thread (clack.handler::handler-acceptor handler))))))

(deftest threaded-stop-falls-back-without-graceful-hook
  (let ((handler (clack.handler:run (lambda (env)
                                      (declare (ignore env))
                                      '(200 nil ("ok")))
                                    :test-no-hook
                                    :use-thread t)))
    (unwind-protect
         (let ((acceptor (clack.handler::handler-acceptor handler)))
           (ok (bt2:threadp acceptor))
           (clack.handler:stop handler)
           (ok (not (bt2:thread-alive-p acceptor))))
      (when (bt2:thread-alive-p (clack.handler::handler-acceptor handler))
        (bt2:destroy-thread (clack.handler::handler-acceptor handler))))))
