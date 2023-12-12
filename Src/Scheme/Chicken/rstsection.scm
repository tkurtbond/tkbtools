;;;; rstsection.scm - Generate an reStructuredText section header.
(module rstsection ()
(import scheme)
(import (chicken base))
(import (chicken process-context))
(import matchable)
(import format)
(import (srfi 152))
(import tkurtbond)

(define (one-char? s)
  (and (string? s)
       (= 1 (string-length s))))

(define (parse args)
  (match args
    (((? one-char? underliner)
      (? string? section-title) . others)
     (let* ((len (string-length section-title))
           (underline (make-string len (string-ref underliner 0))))
       (format #t "~a~%~a~%~%" section-title underline))
     (parse others))
    (())                                ;exit
    (x (die 127 "incorrect arguments: ~s~%" x))
    ))

(define (main)
  (let* ((args (command-line-arguments))
         (len (length args)))
    (when (or (= len 0) (odd? len))
      (die 127 "Usage: ~a underliner section-title [underliner section-title]...

You must specify an even positive number of arguments.~%" (program-name)))
    (parse args)))

;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  ((and chicken-5 compiling)
   (main))
  ((and chicken-5 csi)))
)

  
