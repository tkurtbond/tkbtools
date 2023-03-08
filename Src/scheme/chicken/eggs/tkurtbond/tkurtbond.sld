(define-library (tkurtbond)
  (export die)
  (import scheme
          (chicken base)
          (chicken process-context)
          format)

  (begin
    (define (die status . args)
      (format (current-error-port) "~a: " (program-name))
      (apply format (cons (current-error-port) args))
      (exit status))))
