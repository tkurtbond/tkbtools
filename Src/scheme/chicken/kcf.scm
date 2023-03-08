(module kcf ()

  (import scheme)
  (import (chicken base))
  (import args)
  (import (chicken process-context))
  (import format)
  (import loop)

  (define (die status . args)
    (format (current-error-port) "~A: fatal error: " (program-name))
    (apply format (cons (current-error-port) args))
    (format (current-error-port) "\n")
    (exit status))
  
  (define (celsius-to-kelvin celsius)
    (+ celsius 273.15))
  (define (celsius-to-fahrenheit celsius)
    (+ (* (/ 9 5.0) celsius) 32))
  (define (kelvin-to-celsius kelvin)
    (- kelvin 273.15))
  (define (kelvin-to-fahrenheit kelvin)
    (+ (* (/ 9 5.0) (- kelvin 273)) 32))
  (define (fahrenheit-to-celsius fahrenheit)
    (* (- fahrenheit 32) (/ 5 9.0)))
  (define (fahrenheit-to-kelvin fahrenheit)
    (+ (* (- fahrenheit 32) (/ 5 9.0)) 273.15))

  (define (usage)
    (format (current-error-port) "Usage: ~a [option] [temperture...]~%"
            (program-name))
    (format (current-error-port) "~a~%" (args:usage +command-line-options+))
    (exit 1))


  (define convert-from 'kelvin)
  (define +command-line-options+
    (list (args:make-option (c celsius) #:none
                            "Convert from Celsius/Centigrade"
                            (set! convert-from 'celsius))
          (args:make-option (f fahrenheit) #:none
                            "Convert from Fahrenheit"
                            (set! convert-from 'fahrenheit))
          (args:make-option (k kelvin) #:none
                            "Convert from Kelvin"
                            (set! convert-from 'kelvin))
          (args:make-option (h help) #:none
                            "Display usage message"
                            (usage))))
  #|
  (define (to-f-and-c kelvin)           ;
  (let ((celsius (- kelvin 273.15))     ;
  (fahrenheit (+ (* 1.8 (- kelvin 273)) 32))) ;
  (format #t "~f K is ~f ℃ is ~f ℉~%" kelvin celsius fahrenheit))) ;
                                        ;
  (loop for kelvin in (command-line-arguments) ;
  do (to-f-and-c (string->number kelvin))) ;
  |#

  (define (convert-temperature temperature)
    (cond ((eq? convert-from 'celsius)
           (format #t "~f ℃ is ~f ℉ and ~f K~%" temperature
                   (celsius-to-fahrenheit temperature)
                   (celsius-to-kelvin temperature)))
          ((eq? convert-from 'fahrenheit)
           (format #t "~f ℉ is ~f ℃ and ~f K~%" temperature
                   (fahrenheit-to-celsius temperature)
                   (fahrenheit-to-kelvin temperature)))
          ((eq? convert-from 'kelvin)
           (format #t "~f K is ~f ℉ and ~f ℃~%" temperature
                   (kelvin-to-fahrenheit temperature)
                   (kelvin-to-celsius temperature)))
          (else (die 2 "unable to tell what to do with ~s~%" convert-from))))

  (receive (options operands)
      (args:parse (command-line-arguments) +command-line-options+)
    (format #t "operands: ~s~%" operands)
    (loop for temperature in operands
          do (convert-temperature (string->number temperature))))
  )
