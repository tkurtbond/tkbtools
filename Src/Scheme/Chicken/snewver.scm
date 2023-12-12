;; -*- geiser-scheme-implementation: chicken -*-
(import args)
(import format)
(import srfi-19)
(import (chicken pathname))
(import (chicken file))
(import (chicken port))
(import (chicken process-context))

(define (numeric-part n)
  (if (= n 0)
      ""                        ; No numeric part for the zeroth time.
      (string-append "_" (number->string n))))

(define (process-pathname pathname)
  (let* ((extension (pathname-extension pathname))
         (pathbase (pathname-strip-extension pathname))
         (date (current-date))
         (date-string (date->string date "-~Y-~m-~d")))
    (let loop ((i 0))
      (let ((newname (string-append pathbase date-string
                                    (if *label* (string-append "-" *label*) "")
                                    (numeric-part i)
                                    (if extension (string-append "." extension)
                                        ""))))
        (cond ((file-exists? newname)
               (loop (+ i 1)))
              (else
               (format 3 "'~A' ~A '~A'~%" pathname
                       (if *dry-run* "would be copied to" "copied to")
                       newname)
               (unless *dry-run* (copy-file pathname newname #f 10240))))))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "snewver makes copies of files, with file.ext being copied to 
file-YYYY-MM-DD.ext if that file doesn't exist, or with _<N> append after the
date if that file existins, with N starting at 1 and increasing until no file
with that name already exists.")
      (newline)
      (print "Usage: " (program-name) " [options...] [files...]")
      (newline)
      (print (args:usage +options+))
      (format #t "Current argv: ~s~%" (argv))))
  (exit 1))

(define *dry-run* #f)
(define *label* #f)

(define +options+
  (list (args:make-option (h help)    #:none      "Display this text"
	  (usage))
        (args:make-option (l label)   #:required  "Label to add after the date"
          (set! *label* arg))
        (args:make-option (n dry-run) #:none      "Don't actually do anything"
          (set! *dry-run* #t))))

(receive (options operands) (args:parse (command-line-arguments) +options+)
  (for-each process-pathname operands))

  
