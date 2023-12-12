;;; -*- geiser-scheme-implementation: chicken -*-
;;;!> todeltatime.scm - convert time in sec. to delta time
;;; 10000 becomes 2h46m40s
;;; 324291030 becomes 10y5M3d8h50m30s.
;;; 317330821250 becomes 102c2y3M10h20m50s.

(module todeltatime ()

(import scheme
        (chicken base)
        (chicken io)
        (chicken port)
        (chicken process-context)
        (chicken string)
        args
        bindings
        format
        loop
        srfi-141
        tkurtbond)

(define start-with 'seconds-to-minutes)
  
(define time-units '((seconds-to-minutes "s" 60)
                     (minutes-to-hours   "m" 60)
                     (hours-to-days      "h" 24)
                     (days-to-months     "d" 30)
                     (months-to-years    "M" 12)
                     (years-to-centuries "y" 100)
                     ))


(define (process-duration duration)
  (let ((d (string->number duration)))
    (unless d
      (die 2 "Unable to convert duration ~A into a number.~%" duration))
    (define (find-start)
      (let loop ((tu time-units))
        (cond ((null? tu)
               (die 3
                    "Unable to find a time unit to start with for duration \
~A.~%" duration))
              ((eq? start-with (caar tu))
               tu)
              (else
               (loop (cdr tu))))))
    (define (convert)
      (let loop ((tu (find-start))
                 (acc '())
                 (d d))
        (cond ((null? tu)
               ;; Whatever d is let off is a century.
               (if (zero? d)
                   acc
                   (cons (format #f "~dc" d) acc)))
              (else
               (bind (conversion unit divisor) (car tu)
                 (receive (q r) (truncate/ d divisor)
                   (loop (cdr tu)
                         (if (zero? r)
                             acc
                             (cons (format #f "~d~a" r unit) acc))
                         q)))))))
    (format #t "~a~%" (apply string-append (convert)))))

(define (process-port port)
  (loop for line = (read-line) while (not (eof-object? line)) do
        (loop for part in (string-split line) do (process-duration part))))


(define +options+
  (list (args:make-option (h help) #:none "Display this text"
          (usage))
        (args:make-option (s seconds) #:none "Start with seconds"
          (set! start-with 'seconds-to-minutes))
        (args:make-option (m minutes) #:none "Start with minutes"
          (set! start-with 'minutes-to-hours))
        (args:make-option (h hours) #:none "Start with hours"
          (set! start-with 'hours-to-days))
        (args:make-option (d days) #:none "Start with days"
          (set! start-with 'days-to-months))
        (args:make-option (M months) #:none "Start with months"
          (set! start-with 'months-to-years))
        (args:make-option (y years) #:none "Start with years"
          (set! start-with 'years-to-centuries))                           
        ))
          
               
(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options...] [duration...]")
      (newline)
      (print "Converts duration in seconds, minutes, hours, days, months, or years to a")
      (print "delta time.")
      (newline)
      (print (args:usage +options+))
      (format (current-error-port) "Current argv: ~s~%" (argv))))
  (exit 1))
        

(define (main)
  (receive (options operands)
      (args:parse (command-line-arguments) +options+)
    (if (zero? (length operands))
        (process-port (current-input-port))
        (for-each process-duration operands))))

(cond-expand
  (compiling (main))
  (else))
)

