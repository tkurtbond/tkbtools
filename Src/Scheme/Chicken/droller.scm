;;; -*- geiser-scheme-implementation: chicken -*-
;;; droller.scm -- dice roller.
;;; This is what it implements currently (things in angle brackets are the
;;; names of numbers):
;;;     [<num-rolls>*][<num-dice>]d<num-sides>[(l|h)<keep-n>][(+|-|*)<operand>]
;;; 
;;; Ex: 2d6, 6*3d6, 4d3-8
;;;     4d6h3 -- 4d6, keep highest 3
;;;
;;; PCRE tranlation:
;;;     "((?:(?<num-rolls>[]+)\\*))?(?<num-dice>[]+)?d(?<num-sides>[]+)((?:(?<keep>(?:l|h))(?<keep-n>[]+)))?((?:(?<operation>[])(?<operand>[]+)))?"
;;;
;;; This is what I want it to implement:
;;;     [<num-rolls>*][<num-dice>]d<num-sides>[(m|f|o)|(l|h)<keep-n>)][(+|-|/|*)<operand>]
(module droller ()


(import scheme)
(import loop)
(import (srfi 1))
(import (chicken base))
(import (chicken io))
(import (chicken irregex))
(import (chicken random))
(import (chicken sort))
(import (chicken process-context))
(import format) 

(define roller-sre
  '(seq (? (submatch (seq (submatch-named num-rolls (+ (/ #\0 #\9))) "*")))
	(? (submatch-named num-dice (+ (/ #\0 #\9))))
	"d"
	(submatch-named num-sides (+ (/ #\0 #\9)))
	(? (submatch
	    (seq (submatch-named keep (or #\l #\h))
		 (submatch-named keep-n (+ (/ #\0 #\9))))))
	(? (submatch
	    (seq (submatch-named operation (/ #\- #\- #\+ #\+ #\* #\*
;; Doesn't work yet.
;;                                              #\/ #\/
                                              ))
		 (submatch-named operand (+ (/ #\0 #\9))))))))

(define roller-regex (sre->irregex roller-sre 'i))

(define (default-match match match-num default)
  (or (and-let* ((n (irregex-match-substring match match-num))
		 n)
	(string->number n))
      default))

(define (D num-sides) (+ 1 (pseudo-random-integer num-sides)))

(define (top-n n l) (take-right (sort l <) n))
(define (bottom-n n l) (take (sort l <) n))

(define (nDsHm num-sides num-dice num-highest)
  (apply + (top-n num-highest 
		  (loop for i from 1 to num-dice collect (D num-sides)))))

(define (nDsLm num-sides num-dice num-lowest)
  (apply + (bottom-n num-lowest
		     (loop for i from 1 to num-dice collect (D num-sides)))))

(define (nD num-sides num-dice keep keep-n)
  (if keep
      ((if (string=? keep "l") nDsLm nDsHm) num-sides num-dice keep-n)
      (let loop ((i 1) (acc (D num-sides)))
	(if (= i num-dice)
	    acc
	    (loop (+ 1 i) (+ acc (D num-sides)))))))

(define (do-operation operation operand n)
  (if operation
      (cond ((string=? operation "+")
	     (+ n operand))
	    ((string=? operation "-")
	     (- n operand))
	    ((string=? operation "*")
	     (* n operand))
;; Doesn't work yet.
;;            ((string=? operation "/")
;;             (/ (exact->inexact n) (exact->inexact operand)))
	    (else
	     (error "Unknown operand")))
      n))

(define (rolls num-sides num-dice num-rolls keep keep-n operation operand)
  (let loop ((i 1))
    (if (> i 1) (format #t ", "))
    (format #t "~D" (do-operation operation operand 
				  (nD num-sides num-dice keep keep-n)))
    (if (= i num-rolls)
	(format #t "~%")
	(loop (+ 1 i)))))

(define (show num-sides num-dice num-rolls keep keep-n operation operand)
  (format #t "num-sides: ~S~%num-dice: ~S~%num-rolls: ~S~%keep: ~S~%keep-n: ~S~%operation: ~S~%operand: ~S~%"  num-sides num-dice num-rolls keep keep-n operation operand))

(define (evaluate-die-roller m)
  (let* ((num-rolls  (default-match m 'num-rolls 1))
	 (num-dice   (default-match m 'num-dice 1))
	 (num-sides  (default-match m 'num-sides 1)) ; Note the default will never happen
	 (keep       (irregex-match-substring m 'keep))
	 (keep-n     (default-match m 'keep-n num-dice))
	 (operation  (irregex-match-substring m 'operation))
	 (operand    (default-match m 'operand 1)))
    (format #t "~A => " (irregex-match-substring m 0))
    (if (and keep (< num-dice keep-n))
	(format #t "Number of dice ~D is less than number specified to keep ~D~%"
		num-dice keep-n)
	(rolls num-sides num-dice num-rolls keep keep-n operation operand))))

(define (help)
  (display "[num-rolls*][num-dice]d[num-sides][(l|h)keep-n][(+|-)operand]")
  (newline))

(define (main)
  (let ((args (command-line-arguments)))
    (if (> (length args) 0)
        (loop for dice in args
              do (let ((m (irregex-search roller-regex dice)))
                   (if (not (irregex-match-data? m))
                       (format #t "Not a die roller expression: ~A~%" dice)
                       (evaluate-die-roller m))))
        (let loop ((line (read-line)))
          (cond ((eof-object? line)
                 #f)
                (else
                 (when (string=? line "?") (help) (loop (read-line)))
                 (let ((m (irregex-search roller-regex line)))
                   (if (not (irregex-match-data? m))
	               (format #t "Not a die roller expression: ~A~%" line)
	               (evaluate-die-roller m))
                   (loop (read-line)))))))))


(main)
)
