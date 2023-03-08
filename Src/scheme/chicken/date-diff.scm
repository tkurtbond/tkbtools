(module date-diff ()
  
(import (scheme))
(import (chicken base))

(import (chicken process-context))
(import (chicken port))

(import (args))
(import (bindings))
(import (srfi-19))
(import (loop))
(import (format))
(import (chicken irregex))

(define (die status . args)
  (format (current-error-port) "~a: " (program-name))
  (apply format (cons (current-error-port) args))
  (format (current-error-port) "~%")
  (exit status))

(define *english-list*                  ; Format is hairy, but useful.
  "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(define (duration->string duration)
  ;; What about:
  ;;     $ date-diff 2022-03-09T08:59:00Z 2023-03-08T08:59:00
  ;;     -364 days and -5 hours
  (let*-values (((minutes seconds)
                 (quotient&remainder (time->seconds duration) 60))
                ((hours minutes) (quotient&remainder minutes 60))
                ((days hours) (quotient&remainder hours 24))
                ((years days) (quotient&remainder days 365)))
    (let ((segments
           (loop for n in (list years days hours minutes seconds)
                 for type in (list "year" "day" "hour" "minute" "second")
                 when (not (= n 0)) collect (format #f "~d ~a~p" n type n))))
      ;; The format egg apparently doesn't implement ~:}.  Sigh.
      ;; Otherwise I'd use
      ;; "~{~#[no time at all~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~:}"
      ;; above instead.
      (if (null? segments)
          "no time at all"
          (format #f *english-list* segments)))))

(define date-time-sre
  '(seq (* (/ #\space #\space #\tab #\tab))
        (submatch-named date
          (seq (submatch-named year (= 4 (/ #\0 #\9)))
               "-"
               (submatch-named month (= 2 (/ #\0 #\9)))
               "-"
               (submatch-named day (= 2 (/ #\0 #\9)))))
        (?                              ; Optional time: HH:MM:SS
         (or #\space #\T)
         (submatch-named time
           (seq (submatch-named hour (= 2 (/ #\0 #\9)))
                ":"
                (submatch-named minute (= 2 (/ #\0 #\9)))
                ":"
                (submatch-named second (= 2 (/ #\0 #\9)))))
         (?                    ; optional time zone offset: [-+]HH:?MM
          (submatch-named offset
            (or  (submatch-named offset-zulu #\Z)
                 (seq (submatch-named offset-sign (/ #\- #\- #\+ #\+))
                      (submatch-named offset-hours (= 2 (/ #\0 #\9))) ;hours offset
                      (? ":")
                      (submatch-named offset-minutes (= 2 (/ #\0 #\9))))))) ;minutes offset
         (* (/ #\space #\space #\tab #\tab)))))

(define date-time-irx (sre->irregex date-time-sre))

(cond-expand
  (compiling)
  (else
   (define (s m n)
     (irregex-match-substring m n))
   (set! m1 (irregex-match date-time-irx "2023-03-06 13:54:18-05:00"))
   (set! m2 (irregex-match date-time-irx "2023-03-04T14:40:18-06:00"))
   (set! m3 (irregex-match date-time-irx "2023-03-03T15:30:18-0700"))
   (set! m4 (irregex-match date-time-irx "2023-03-03T15:20:18+0800"))
   (set! m5 (irregex-match date-time-irx "2023-03-02T16:10:18Z"))
   (set! m6 (irregex-match date-time-irx "2023-03-02T16:05:18"))
   (set! m7 (irregex-match date-time-irx "2023-03-01"))
   ))

(define (parse-date date-string)
  (let* ((m (irregex-match date-time-irx date-string)))
    (unless m
      (die 127 "Unable to parse date ~s" date-string))
    (let ((year   (string->number (irregex-match-substring m 'year)))
          (month  (string->number (irregex-match-substring m 'month)))
          (day    (string->number (irregex-match-substring m 'day)))
          ;; Time is optional.
          (hour   (if (irregex-match-substring m 'hour)
                      (string->number (irregex-match-substring m 'hour))
                      0))
          (minute (if (irregex-match-substring m 'minute)
                      (string->number (irregex-match-substring m 'minute))
                      0))
          (second (if (irregex-match-substring m 'second)
                      (string->number (irregex-match-substring m 'second))
                      0))
          ;; Time zone offset is optional.
          (offset (if (irregex-match-substring m 'offset)
                      (if (string=? "Z" (irregex-match-substring m 'offset))
                          0
                          (* (if (string=?
                                  "-" (irregex-match-substring m 'offset-sign))
                                 -1
                                 1)
                             (+ (* 60 60 (string->number
                                          (irregex-match-substring m 'offset-hours)))
                                (* 60 (string->number
                                       (irregex-match-substring m 'offset-minutes))))))
                      (timezone-locale-offset))))
      (make-date 0 second minute hour day month year offset))))


(define (date->utc date)
  (let* ((zone-offset-duration
          (make-duration #:seconds (date-zone-offset date)))
         (utc-date (make-date 0
                              (date-second date)
                              (date-minute date)
                              (date-hour   date)
                              (date-day    date)
                              (date-month  date)
                              (date-year   date)
                              0))
         (utc-date (date-subtract-duration utc-date zone-offset-duration)))
    utc-date))

(define (diff-dates args)
  (bind-loop (ds1 ds2 rest ...) args
    (let* ((d1       (parse-date ds1))
           (d2       (parse-date ds2))
           (lesser (date<? (date->utc d1) (date->utc d2)))
           (diff     (if lesser
                         (date-difference d2 d1)
                         (date-difference d1 d2)))                     
           (duration-string (duration->string diff)))
      (if *verbose*
          (format #t "~a - ~a = ~a ~a~%" ds1 ds2 duration-string
                  (if lesser "before" "after"))
          (format #t "~a ~a~%" duration-string (if lesser "before" "after")))
      (unless (null? rest)
        (loop rest)))))


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
      (format (current-error-port) "~a: ~a~%args: ~s~%" (program-name)
              "You must specify pairs of YYYY-MM-DD dates to take the difference."
              (command-line-arguments))
      (exit 127))
    (diff-dates operands)))


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
     (format #t "diff1: ~a~%" (duration->string diff1))
     (format #t "diff2: ~a~%" (duration->string diff2))
     (format #t "diff3: ~a~%" (duration->string diff3)))))

)
