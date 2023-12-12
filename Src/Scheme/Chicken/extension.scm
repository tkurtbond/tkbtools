;;; extension.scm -- return just the extension of pathname.
(import (chicken pathname))
(import (chicken process-context))

(for-each (lambda (pathname)
            (let* ((extension (pathname-extension pathname))
                   (s (if extension (string-append "." extension) "")))
              (display s))
            (newline))
          (command-line-arguments))
