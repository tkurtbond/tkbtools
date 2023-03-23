;;;> get-pipermail-archives.scm -- Get the archives of a pipermail mailing list.
;;; Example:
;;; get-pipermail-archives -d ~/tmp/ecl-devel "https://mailman.common-lisp.net/pipermail/ecl-devel/"

(module get-pipermail-archives ()

(import scheme)
(import (chicken base))
(import (chicken file))
(import (chicken io))                   ; Or you could use srfi-152?
(import (chicken port))
(import (chicken process-context))

(import (tkurtbond))

(import args)
(import format)
(import loop)
(import html-parser)
(import http-client)
(import sxpath)
(import uri-common)

(define (get-pipermail-archives top-url)
  (define top-uri (uri-reference top-url))

  (define top-html (with-input-from-request top-url #f read-string))
  (define top-sxml (with-input-from-string top-html html->sxml))

  (define urls ((sxpath "//td[3]/a/@href") top-sxml))

  (loop for url in urls do (download-archive (cadr url) top-uri)))

(define (download-archive relative-url top-uri)
  (format #t "relative-url: ~s~%" relative-url) (flush-output)
  (define uri (uri-reference relative-url))
  (define archive-uri (uri-relative-to uri top-uri))
  (define url-string (uri->string archive-uri))
  (format #t "working on ~s~%" url-string) (flush-output)
  (define archive (with-input-from-request archive-uri #f read-string))
  (with-output-to-file relative-url (lambda () (display archive))))

(define *output-directory* #f)

(define +command-line-options+
  (list (args:make-option
         (d directory) #:required "Output directory"
         (set! *output-directory* arg))
        (args:make-option
         (h help) #:none "Display this text"
	 (usage))))

(define (usage . msg)
  (usage-msg 127 "~a~%~a~a~%" "[options...] [files...]"
             (if msg (car msg) "")
             (args:usage +command-line-options+)))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments)
                                          +command-line-options+)
    (when (< (length operands) 1)
      (usage "\nSpecify at least one file!\n\n"))
    (when *output-directory*
      (cond ((directory-exists? *output-directory*)
             (change-directory *output-directory*))
            (else
             (create-directory *output-directory*)
             (change-directory *output-directory*))))
    (loop for url in operands do (get-pipermail-archives url))))

;; Only invoke main if this has been compiled.  That way we can load the
;; module into csi and debug it. 
(cond-expand
  (compiling
   (main))
  (else)))
