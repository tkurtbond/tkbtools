;; -*- geiser-scheme-implementation: chicken -*-
(define-library (tkurtbond)
  (export ignore-warnings warn increase-verbosity info err die)
  (import scheme
          (chicken base)
          (chicken process-context)
          format)

  (begin
    (define *verbosity* 0)
    (define *ignore-warnings* #f)

    (define (ignore-warnings)
      (set! *ignore-warnings* #t))
    
    (define (warn . args)
      (unless *ignore-warnings*
        (format (current-error-port) "~A: warning: " (program-name))
        (apply format (cons (current-error-port) args))))

    (define (increase-verbosity)
      (set! *verbosity* (+ *verbosity* 1)))

    (define (info level . args)
      (when (<= level *verbosity*)
        (format (current-error-port) "~a: " (program-name))
        (apply format (cons (current-error-port) args))))

    (define (err . args)
      (format (current-error-port) "~a: " (program-name))
      (apply format (cons (current-error-port) args)))

    (define (die status . args)
      (format (current-error-port) "~a: " (program-name))
      (apply format (cons (current-error-port) args))
      (exit status))))
