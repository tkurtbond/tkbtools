;;; -*- geiser-scheme-implementation: chicken -*-
(module date-diff ()
  
(import scheme)
(import (chicken base))

(import (chicken process-context))
(import (chicken port))

(import args)
(import bindings)
(import srfi-19)
(import loop)
(import format)
(import (chicken irregex))

(import tkurtbond
        (tkurtbond date))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (format #t "Usage: ~a [options...] [older-time younger-time]...~%"
              (program-name))
      (format #t "~a~%" (args:usage +command-line-options+))
      (format #t "arguments: ~s~%~%" (command-line-arguments))
      (format #t "\
Dates are specified in mostly RFC 3339/ISO 8601 style, YYYY-MM-DD,
optionally followed by a space or 'T' and a time, HH:MM:SS, optionally
followed by a time zone offset of 'Z' or [-+]HH:MM or [-+]HHMM.  If no
time zone offset is specified the local timezone offset is used.

If older-time is actually less than younger-time they are flipped, so
the result is always a postive duration.

Examples:
    $ date-diff 2023-03-08 2022-01-01
    1 year and 66 days

    $ date-diff 2023-03-08T10:15:00 2022-01-01T08:11:12
    1 year, 66 days, 2 hours, 3 minutes, and 48 seconds

    $ date-diff 2023-03-08T08:59:00Z 2023-03-08T08:59:00
    -5 hours    
~%")))
  (exit 127))

(define *verbose* #f)

(define +command-line-options+
  (list (args:make-option
         (h help) #:none "Display a help message and exit."
         (usage))
        (args:make-option
         (v verbose) #:none "Use verbose output."
         (set! *verbose* #t))))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments)
                                          +command-line-options+)
    (when (not (= 0 (modulo (length operands) 2)))
      (die 127 "~a~%args: ~s~%"
              "You must specify pairs of YYYY-MM-DD dates to take the difference."
              (command-line-arguments)))
    (diff-dates operands *verbose*)))


;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  (compiling
   (main))
  (else
   (define (test)
     (define d1 (make-date 0 14 30 10 11 06 2021))
     (define d2 (make-date 0 0 0 0 23 12 2022))
     (define d3 (make-date 0 0 5 0 23 12 2022))
     (define diff1 (date-difference d2 d1))
     (define diff2 (date-difference d3 d2))
     (define diff3 (date-difference d1 d1))
     (define diff4 (date-difference d1 d2))
     (format #t "diff1: ~a~%" (duration->string diff1))
     (format #t "diff2: ~a~%" (duration->string diff2))
     (format #t "diff3: ~a~%" (duration->string diff3))
     (format #t "diff4: ~a~%" (duration->string diff4)))))

)
