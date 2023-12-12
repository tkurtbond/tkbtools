;;; extension.scm -- return just the extension of pathname.
(import (chicken pathname))
(import (chicken process-context))

(for-each (lambda (pathname)
            (let* ((directory (pathname-directory pathname)))
              (when directory
                (display directory))
              (newline)))
          (command-line-arguments))
