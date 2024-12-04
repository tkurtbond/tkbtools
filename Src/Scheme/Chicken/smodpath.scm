;;; -*- geiser-scheme-implementation: chicken -*-
;;; smodpath.scm -- Scheme version of modpath - modify paths
;;;
;;; Typically used something like
;;;     eval $(smodpath --delete /usr/local/bin --start /usr/public/bin \
;;;	               --before /usr/ccs/bin /usr/gnu/bin)
;;;
;;; Or put something like:
;;; 	function srepath {
;;; 	    eval $(smodpath "$@" --unique)
;;; 	}
;;;
;;; in the appropriate shell startup file, and do:
;;;     srepath --delete /usr/local/bin --start /usr/public/bin \
;;;	        --before /usr/ccs/bin /usr/gnu/bin
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module smodpath ()

(import scheme)
(import args)
(import format)
(import matchable)
(import (srfi 13))
(import (chicken base))
(import (chicken file))
(import (chicken pathname))
(import (chicken port))
(import (chicken process-context))
(import (chicken platform))
(import (chicken string))

(import tkurtbond)

(define (print-version)         ; Not a fatal error, so don't use die.
  (format (current-error-port) "version 1.1 2019-11-01~%")
  (exit 1))

(define exists-flag #f)
(define relative-flag #f)
(define msys-style #f)
(define warn-flag #f)

(define in-path-sep (match (software-type)
		      ('unix ":")
		      ('windows ";")))
(define out-path-sep in-path-sep)
(define (set-sep s)
  (set! in-path-sep s)
  (set! out-path-sep s))

(define path-var "PATH")
(define path "")
(define path-list '())
(define (set-path newpath)
  (set! path newpath)
  (set! path-list (string-split newpath in-path-sep)))
(define (set-path-from-var var)
  (let ((path (get-environment-variable var)))
    (if path
	(set-path path)
	(if warn-flag 
	    (begin 
	      (warn "unable to get path from environment variable ~A~%" var)
	      (set-path ""))
	    (die 3 "error: unable to get path from environment variable ~A~%" var)))))
(define (set-path-and-var-from-var var)
  (set! path-var var)
  (set-path-from-var var))

(define mode 'mode-end)
(define (check-mode mode)
  (match mode
    (('mode-after item) (warn "unprocessed --after ~A~%" item))
    (('mode-before item) (warn "unprocessed --before ~A~%" item))
    ('mode-start (warn "unprocessed --start~%"))
    ('mode-end))
					;(values)
  'nothing-to-see-here)

(define (set-add-after-mode item)
  (check-mode mode)
  (set! mode (list 'mode-after item)))
(define (set-add-before-mode item)
  (check-mode mode)
  (set! mode (list 'mode-before item)))
(define (set-add-start-mode)
  (check-mode mode)
  (set! mode 'mode-start))
(define (set-add-end-mode)
  (check-mode mode)
  (set! mode 'mode-end))

(define (add-end item)
  (set! path-list (append path-list (list item))))

(define (add-start item)
  (set! path-list (cons item path-list)))

(define (unique)
  (define (iter acc l)
    (match l
      (() acc)
      ((item . rest)
       (if (not (member item acc))
	   (iter (append acc (list item)) rest)
	   (iter acc rest)))))
  (set! path-list (iter '() path-list)))

(define (add-after after item)
  (define (iter acc rest)
    (match rest
      (()
       (warn "~A is not in path to add ~A after it; adding at end~%" 
	     after item)
       (append acc (list item)))
      ((part . rest)
       (if (string=? after part)
	   (append acc (list part item) rest)
	   (iter (append acc (list part)) rest)))))
  (set! path-list (iter '() path-list)))

(define (add-before before item)
  (define (iter acc rest)
    (match rest
      (()
       (warn "~A is not in path to add ~A before it; adding at start~%"
	     before item)
       (cons item acc))
      ((part . rest)
       (if (string=? before part)
	   (append acc (list item part) rest)
	   (iter (append acc (list part)) rest)))))
  (set! path-list (iter '() path-list)))

(define (delete-from-path item)
  (define (iter acc rest found)
    (match rest
      (()
       (if (not found) (warn "~A is not in path to delete it~%" item))
       acc)
      ((part . rest)
       (if (string=? item part)
	   (iter acc rest #t)
	   (iter (append acc (list part)) rest found)))))
  (set! path-list (iter '() path-list #f)))


(define (anonymous-arg operand options operands)
  (let ((operand (if relative-flag 
		     operand 
		     (if (absolute-pathname? operand)
			 operand
			 (make-absolute-pathname (current-directory)
						 operand)))))
    (if (or (not exists-flag) (directory-exists? operand))
	(begin
	  (match mode
	    (('mode-after after) (add-after after operand))
	    (('mode-before before) (add-before before operand))
	    ('mode-start (add-start operand))
	    ('mode-end (add-end operand))))
	(warn "pathname does not exist: ~A~%" operand)))
  (set! exists-flag #f)
  (set! mode 'mode-end)
  (values options operands))

(define-syntax ignore
  ;; Ignore multiple return values.
  (syntax-rules ()
    ((_ expr)
     (let-values ((_ expr)) #f))))

(define (add-current)
  (ignore (anonymous-arg (current-directory) '() '())))

(define (add-empty)
  (ignore (anonymous-arg "" '() '())))

(define +command-line-options+
  (list (args:make-option (A absolute) #:none "Non-absolute names are made absolute using the\n                          current directory."
	  (set! relative-flag #f))
        (args:make-option (a after) #:required "add next argument to path after ARG"
	  (set-add-after-mode arg))
	(args:make-option (b before) #:required "add next argument to path before ARG"
	  (set-add-before-mode arg))
	(args:make-option (cmd) #:none "Output NT cmd.exe command to set path"
	  (set! output 'out-cmd))
	(args:make-option (h help)      #:none     "Display this text"
	  (usage))
	(args:make-option (csh) #:none "Output csh command to set the path"
	  (set! output 'out-csh))
	(args:make-option (c current) #:none "Add current directory to path"
	  (add-current))
	(args:make-option (d delete) #:required "delete all occurances of ARG in path"
	  (delete-from-path arg))
	(args:make-option (E empty) #:none "Add an empty element to the path"
	  (add-empty))
	(args:make-option (e end) #:none "Add next argument to the end of the path"
	  (set-add-end-mode))
	(args:make-option (exists) #:none "Add next item only if it exists"
	  (set! exists-flag #t))
	(args:make-option (i insep) #:required "Set the input path separator"
	  (set! in-path-sep arg))
	(args:make-option (I ivar) #:required "Set the path from environment variable ARG"
	  (set-path-from-var arg))
        (args:make-option (msys) #:none "Output in msys style"
          (die 1 "--msys not implemented\n"))
	(args:make-option (n name) #:required "Name of path environment variable for output"
	  (set! path-var arg))
	(args:make-option (nice) #:none "Print the path out \"nicely\", one item per line"
	  (set! output 'out-nice))
	(args:make-option (o outsep) #:required "Set the output path separator"
	  (set! out-path-sep arg))
	(args:make-option (p path) #:required "Set the path to work on"
	  (set-path arg))
	(args:make-option (quiet) #:none "Don't print out the path"
	  (set! output 'out-quiet))
	(args:make-option (R relative) #:none "Interpret non-absolute paths as relative to\n                          the current directory"
	  (set! relative-flag #t))
	(args:make-option (S sep) #:required "Set the input and output path separators"
	  (set-sep arg))
	(args:make-option (sh) #:none "Output sh command to set the path"
	  (set! output 'out-sh))
	(args:make-option (simple) #:none "Output just the new value"
	  (set! output 'out-simple))
	(args:make-option (s start) #:none "Add next argument to the start of the path"
	  (set-add-start-mode))
	(args:make-option (u unique) #:none "Eliminate duplicate items"
	  (unique))
	(args:make-option (v var) #:required "Set the path from the enivornment variable ARG,\n                          and make ARG be the name of the output environment\n                          variable"
	  (set-path-and-var-from-var arg))
	(args:make-option (w warnings) #:none "Warn about missing environment variables instead of\n                          exiting with an error"
	  (set! warn-flag #t))
	(args:make-option (V version) #:none "Print version info and exit"
	  (print-version))))


(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (program-name) " [[option...] [item...]]...")
      (newline)
      (print "\
Prints a new version of the path, which defaults to the value of the PATH
environment variable, adding or deleting path elements in positions specified
by the user.")
      (newline)
      (print "Use it with something like:
        function srepath {
            eval $(smodpath \"$@\" --unique)
        }
in your .bashrc or its equivalent and always execute srepath.")
      (newline)
      (print "\
Note that long option names beflow show the argument separated from the
long option name by a an equals sign (\"=\").  The equals signs can be
replace with a space as well.")
      (newline)
      (print (args:usage +command-line-options+))
      (format (current-error-port) "Current argv: ~s~%" (argv))))
  (exit 1))

(set-path-from-var path-var)

;; out-nice out-simple out-cmd out-csh out-sh out-quiet
(define output (match (software-type)
		 ('unix 'out-sh)
		 ('windows 'out-cmd)))

(define (main)
  (receive (options operands)
      (args:parse (command-line-arguments)
		  +command-line-options+
		  operand-proc: anonymous-arg)
    (set! path (string-append "'" (string-intersperse path-list out-path-sep) 
			      "'"))
    (match output 
      ('out-nice   (for-each print path-list))
      ('out-simple (print path))
      ('out-cmd    (print "path " path))
      ('out-csh    (print "setenv " path-var " " path))
      ('out-sh     (print path-var "=" path "\nexport " path-var))
      ('out-quiet  #f))))

;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  ((and chicken-5 compiling)
   (main))
  ((and chicken-5 csi)))
)
