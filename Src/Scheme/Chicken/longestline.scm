;;; -*- geiser-scheme-implementation: chicken -*-
(module longestline ()

  (import scheme)
  (import chicken.base)
  (import chicken.io)
  (import chicken.port)
  (import chicken.process-context)
  (import format)
  (import args)

  (define *longest* 0)

  (define (find-longest-line port)
    (let loop ((line (read-line port)))
      (cond ((eof-object? line)
             )                          ;just return
            (else (let ((len (string-length line)))
                 (when (> len *longest*) (set! *longest* len))
                 (loop (read-line port)))))))

  (define (process-file filename)
    (if (string=? "-" filename)
        (find-longest-line (current-input-port))
        (call-with-input-file filename find-longest-line)))


  (define +command-line-options+
    (list (args:make-option (h help)      #:none     "Display this text"
                            (usage))
          ))

  (define (usage)
    (with-output-to-port (current-error-port)
      (lambda ()
        (print "Usage: " (program-name) " [options...] [filename...]")
        (newline)
        (print "Filename can be a -, in which stdin is read.")
        (newline)
        (print "If no filenames are specified, stdin is read.")
        (newline)
        (print (args:usage +command-line-options+))
        (format (current-error-port) "Current argv: ~s~%" (argv))))
    (exit 1))

  (define (anonymous-arg operand options operands)
    (process-file operand)
    (values options operands))

  (receive (options operands)
      (args:parse (command-line-arguments)
                  +command-line-options+
                  operand-proc: anonymous-arg)
    (when (= (length operands) 0)
      (find-longest-line (current-input-port)))
    (print *longest*)
    (newline)
    (exit 0))
  )
