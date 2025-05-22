(module rst-incl ()

(import scheme)
(import args)
(import loop)
(import (chicken io))
(import (chicken irregex))
(import (chicken process-context))
(import srfi-13)

(define include-sre
  '(seq bos ".." (+ space) "include::" (+ space) 
        (submatch-named filename (+ nonl)) eos))
(define include-irx (sre->irregex include-sre))

(define (process-line line)
  (define match (irregex-match include-irx line))
  (if match
      (process-filename (string-trim-both (irregex-match-substring match 'filename)))
      (write-line line)))

(define (process-from-input)
  (loop for line = (read-line)
        until (eof-object? line)
        do (process-line line)))

(define (process-filename filename)
  (if (string= filename "-")
      (process-from-input)
      (with-input-from-file filename process-from-input)))

(define (main)
  (loop for filename in (command-line-arguments) do (process-filename filename)))


(cond-expand
  ((and chicken-5 compiling)
   (main))
  ((and chicken-5 csi)))
)
