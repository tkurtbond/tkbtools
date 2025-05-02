(import (chicken io))
(import (chicken port))
(import (chicken process-context))
(import (html-parser))
(import (sxpath))
(import (http-client))
(import (args))

(define *version-number* "5")
(define *user-usage* #f)

(define +command-line-options+
  (list (args:make-option (version) #:required "The major version number of Chicken for which to get the number of eggs."
          (set! *version-number* arg))
        (args:make-option (h help) #:none "Display this text"
          (set! *user-usage* #t)
          (usage))))

(define (usage)
  (define port (if *user-usage*
                   (current-output-port)
                   (current-error-port)))
  (with-output-to-port port
    (lambda ()
      (print "Usage: " (program-name) " [option...]")
      (newline)
      (print "Reads the list of CHICKEN Scheme eggs from the website and displays how many\
there were.")
      (newline)
      (print (args:usage +command-line-options+))
      (exit (if *user-usage* 0 1)))))
             
(args:parse (command-line-arguments) +command-line-options+)

(define url-prefix "http://eggs.call-cc.org/")
(define url (string-append "http://eggs.call-cc.org/" *version-number* "/"))

(define eggs-page (with-input-from-request url #f read-string))
(define eggs-sxml (with-input-from-string eggs-page html->sxml))
(define eggs-urls ((sxpath '(// td a @ href *text*)) eggs-sxml))

(display (length eggs-urls))
(newline)
