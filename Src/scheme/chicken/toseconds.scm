;;; -*- geiser-scheme-implementation: chicken -*-
;;;!> toseconds.scm -- convert delta time to seconds.
;;; Delta times are [YEARSy][MONTHSM][DAYSd][HOURSh][MINUTESm][SECONDSs].
;;;
;;; This does no calendar calculations.  It assumes that years are 12
;;; months long and months are 30 days long.
(module toseconds ()

(import scheme
        (chicken base)
        (chicken port)
        (chicken io)
        (chicken process-context)
        (chicken string)
        args
        (clojurian syntax)
        format
        (chicken irregex)
        loop
        miscmacros
        tkurtbond)

(define sre '(seq (? (seq (=> years (+ (/ #\0 #\9))) #\y))
                  (? (seq (=> months (+ (/ #\0 #\9))) #\M))
                  (? (seq (=> days (+ (/ #\0 #\9))) #\d))
                  (? (seq (=> hours (+ (/ #\0 #\9))) #\h))
                  (? (seq (=> minutes (+ (/ #\0 #\9))) #\m))
                  (? (seq (=> seconds (+ (/ #\0 #\9))) #\s))))
(define irx (sre->irregex sre))

;; (process-deltatime "10y5M3d8h50m30s")
(define (process-deltatime deltatime)
  (define m (irregex-match irx deltatime))
  (define parts-converted 0)
  (define (v group)
    (if m
        (let ((s (irregex-match-substring m group)))
          (cond (s (inc! parts-converted)
                   (string->number s))
                (else 0)))
        0))
  (define seconds
    (-> 0
        (+ (* 12 30 24 60 60 (v 'years)))
        (+ (* 30 24 60 60 (v 'months)))
        (+ (* 24 60 60 (v 'days)))
        (+ (* 60 60 (v 'hours)))
        (+ (* 60 (v 'minutes)))
        (+ (v 'seconds))))
  (if (zero? parts-converted)
      (die 2 "unable to decode delta time ~A~%" deltatime))
  (format #t "~d~%" seconds))

(define (process-port port)
  ;; Should I split the line on whitespace and do each part???
  (loop for line = (read-line) while (not (eof-object? line)) do
        (loop for part in (string-split line) do (process-deltatime part))))

(define +options+
  (list (args:make-option (h help) #:none "Display this text"
          (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [options...] [deltatime...]")
      (print)
      (print "Converts delta times of the form:")
      (print "    [[0-9]+y][[0-9]+M][[0-9]+d][[0-9]+h][[0-9]+m][[0-9]+s]")
      (print "to seconds.")
      (newline)
      (print (args:usage +options+))
      (format #t "Current argv: ~s~%" (argv))))
  (exit 1))

(define (main)
  (receive (options operands)
      (args:parse (command-line-arguments) +options+)
    (if (zero? (length operands))
        (process-port (current-input-port))
        (for-each process-deltatime operands))))

(cond-expand
  (compiling (main))
  (else))
)
