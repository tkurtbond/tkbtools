;;;; rstsection.scm - Generate an reStructuredText section header.
(module rstsection ()
(import (scheme))
(import (chicken base))
(import (chicken process-context))
(import (matchable))
(import (fmt))
(import (srfi 152))

(define (die status . args)
  (fmt (current-error-port) (program-name) ": ")
  (apply fmt (cons (current-error-port) args))
  (fmt (current-error-port) "\n")
  (exit status))

;; (define (die status . args)
;;   (apply fmt (cons (current-error-port) args))
;;   (fmt (current-error-port) "\n")
;;   (exit status))

(define (one-char? s)
  (and (string? s)
       (= 1 (string-length s))))

(define (parse args)
  (match args
    (((? one-char? underliner)
      (? string? section-title) . others)
     (let* ((len (string-length section-title))
           (underline (make-string len (string-ref underliner 0))))
       (fmt #t section-title nl underline nl nl))
     (parse others))
    (())                                ;exit
    (x (die 127 "incorrect arguments: " (wrt x)))
    ))

(define (main)
  (let* ((args (command-line-arguments))
         (len (length args)))
    (when (or (= len 0) (odd? len))
      (die 127 "Usage: " (program-name)
           " underliner section-title [underliner section-title]..." nl nl
           "You must specify an even positive number of arguments."))
    (parse args)))

;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  ((and chicken-5 compiling)
   (main))
  ((and chicken-5 csi)))
)

  
