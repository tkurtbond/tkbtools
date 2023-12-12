;;; -*- geiser-scheme-implementation: chicken -*-
(module swipe ()

  (import scheme)
  (import args)
  (import format)
  (import loop)
  (import matchable)
  (import (chicken base))
  (import (chicken condition))
  (import (chicken file))
  (import (chicken file posix))
  (import (chicken pathname))
  (import (chicken port))
  (import (chicken process-context))
  (import exn-condition)
  (import (srfi 152))

  (import tkurtbond)

  (define delete-flag #t)
  (define recurse-flag #f)
  (define num-overwrites 3)

  (define (print-version)      ; Not a fatal error, so don't use die. 
    (format (current-error-port) "version 1.1 2021-07-05~%")
    (exit 1))


  (define buffer-size 10240)
  (define buffer (make-string buffer-size #\a))
  (define fill-chars-size (+ 1 1 1 26))
  (define fill-chars (make-string fill-chars-size #\space))
  (string-set! fill-chars 0 (integer->char #xFF))
  (string-set! fill-chars 1 (integer->char #x00))
  (string-set! fill-chars 2 (integer->char #xFF))
  (loop for c from (char->integer #\A) to (char->integer #\Z)
	for i from 3 to (- fill-chars-size 1)
	do (string-set! fill-chars i (integer->char c)))
  ;;(loop for i from 0 to (- fill-chars-size 1)
  ;;      do (print (char->integer (string-ref fill-chars i))))



  (define (wipe-dir dirname)
    (if recurse-flag
	(begin 
	  (info 1 "wiping directory ~A~%" dirname)
	  (let ((filenames (directory dirname #t)))
	    (for-each (lambda (filename)
			(wipe (make-pathname dirname filename))) filenames)))
	(info 1 "skipping directory ~A~%" dirname))
    (when delete-flag
      (info 1 "deleting directory: ~A~%" dirname)
      (condition-case (delete-directory dirname)
        [ex (exn i/o file)
            (die 127 "unable to delete directory ~s: ~a.~a~%"
                 dirname (exn-message ex) (if (string-contains (exn-message ex) "Directory not empty") "  Did you forget --recurse or -R?" ""))])))

  (define (wipe-file filename)
    (info 1 "wiping file ~A~%" filename)
    ;; actually wipe file
    (loop for i from 1 to num-overwrites
	  do (let ((fill-char (string-ref fill-chars 
					  (modulo (- i 1) fill-chars-size)))
		   (size (file-size filename)))
	       (string-fill! buffer fill-char)
	       (info 2 "open file ~a, round ~d of ~d~%" filename i num-overwrites)
	       (let ((fileno (file-open filename open/wronly))
		     (bytes-written 0))
		 (do ()
		     ((>= bytes-written size))
		   (let* ((bytes-to-write (min buffer-size
					       (- size bytes-written)))
			  (n (file-write fileno buffer bytes-to-write)))
		     (set! bytes-written (+ bytes-written n))))
		 (file-close fileno)
		 (info 2 "wrote ~d bytes (0x~x) to ~a, round ~d of ~d~%"
		       bytes-written (char->integer fill-char) filename i 
		       num-overwrites))))
    (when delete-flag
      (info 1 "deleting file: ~A~%" filename)
      (delete-file filename)))

  (define (wipe filename)
    (match (file-type filename #f #f)
      (#f (warn "file does not exist: ~A~%" filename))
      ('directory (wipe-dir filename))
      ('regular-file (wipe-file filename))
      (type (info 1 "skipping ~A file ~A~%" type filename))))
  
  (define +command-line-options+
    (list (args:make-option (d delete) #:none "Delete after wiping"
			    (set! delete-flag #t))
	  (args:make-option (D no-delete) #:none "Do *not* delete after wiping"
			    (set! delete-flag #f))
	  (args:make-option (h help)      #:none     "Display this text"
			    (usage))
	  (args:make-option (R recurse) #:none "Recurse into subdirectories"
			    (set! recurse-flag #t))
	  (args:make-option (V verbose) #:none "Describe actions of program"
			    (increase-verbosity))
	  (args:make-option (r rounds) #:required "Overwrite ARG times"
			    (set! num-overwrites (string->number arg))
                            (unless num-overwrites
                              (die 127 "Unable to convert ~s into a number.~%"
                                   arg)))
	  (args:make-option (v version) #:none "Print version info and exit"
		 	    (print-version))))

  (define (usage)
    (with-output-to-port (current-error-port)
      (lambda ()
	(print "Usage: " (program-name) " [options...] [files...]")
	(newline)
	(print (args:usage +command-line-options+))
	(format (current-error-port) "Current argv: ~s~%" (argv))))
    (exit 1))

  (define (anonymous-arg operand options operands)
    (wipe operand)
    (values options operands))

  (receive (options operands)
      (args:parse (command-line-arguments)
		  +command-line-options+
		  operand-proc: anonymous-arg)
    (exit 0))
  )
