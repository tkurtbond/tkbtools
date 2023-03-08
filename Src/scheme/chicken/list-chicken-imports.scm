;;; -*- geiser-scheme-implementation: chicken -*-
;;;!> list-chicken-imports.scm -- Read a CHICKEN Scheme source fiel and list the imports.
;;;
;;; Todo:
;;; - Since this is mostly so I can find eggs, I ought to add an option to ignore
;;;   the library names from chicken and scheme.
;;;
(module list-chicken-imports ()

  (import scheme)
  (import args)
  (import format)
  (import loop)
  (import matchable)
  (import (chicken base))
  (import (chicken port))
  (import (chicken process-context))
  (import (chicken condition))
  (import exn-condition)            ; Part of the condition-utils egg.

  (define (process-import-set import-set)
    (match import-set
      ((? symbol? symbol)
       ;;(format #t "symbol: ~s~%" sym)
       (format #t "~s~%" symbol))
      (('only import-set ids ...)
       ;;(format #t "only: ~s ~s~%" import-set ids)
       (process-import-set import-set))
      (('except import-set ids ...)
       ;;(format #t "except: ~s ~s~%" import-set ids)
       (process-import-set import-set))
      (('prefix import-set ids ...)
       ;;(format #t "prefix: ~s ~s~%" import-set ids)
       (process-import-set import-set))
      (('rename import-set renamings ...)
       ;;(format #t "rename: ~s ~s~%" import-set renamings)
       (process-import-set import-set))
      (other
       ;;(format #t "other: ~s~%" other)
       (format #t "~s~%" other))))

  (define (process-import-sets import-sets)
    (loop for import-set in import-sets do (process-import-set import-set)))

  (define (find-module-imports forms)
    (loop for form in forms
          do (match form
               (('import imports ...)
                (process-import-sets imports))
               (_ '()))))

  (define (find-file-imports port)
    (let loop ((form (read port)))
      ;;(format #t "form: ~S~%" form)
      (match form
        ((? eof-object?)
         ;;(format #t "EOF~%")
         )           ;don't loop
        (('module name exports forms ...)
         ;;(format #t "module: ~s~%" name)
         (find-module-imports forms)
         (loop (read port)))
        (('import import-sets ...)
         ;;(format #t "imports: ~s~%" form)
         (process-import-sets import-sets)
         (loop (read port)))
        (_ (loop (read port))))))

  (define (process-file filename)
    (condition-case 
        (if (string=? "-" filename)
            (find-file-imports (current-input-port))
            (call-with-input-file filename find-file-imports))
      (var (exn syntax)
           (format (current-error-port) "error: ~a: syntax error: ~a~%"
                   filename
                   (exn-message var)))
      (var ()
           (format (current-error-port)
                   "error: ~a: unhandled error: ~s: ~s ~s~%"
                   filename var
                   (exn-arguments var)
                   (exn-message var)))))
          
    

  (define verbose-flag #f)

  (define +command-line-options+
    (list (args:make-option (h help)      #:none     "Display this text"
                            (usage))
          (args:make-option (verbose) #:none "Describe actions of program"
                            (set! verbose-flag (+ 1 verbose-flag)))))

  (define (usage)
    (with-output-to-port (current-error-port)
      (lambda ()
        (print "Usage: " (program-name) " [options...] [files...]")
        (newline)
        (print (args:usage +command-line-options+))
        (format (current-error-port) "Current argv: ~s~%" (argv))))
    (exit 1))

  (define (anonymous-arg operand options operands)
    (process-file operand)
    (values options operands))

  (receive (options operands)
      (args:parse (command-line-arguments) +command-line-options+
                  operand-proc: anonymous-arg)
    (exit 0))

  )
