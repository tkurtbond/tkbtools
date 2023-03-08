(import (chicken pathname))
(import (chicken process-context))

(for-each (lambda (path)
            (display (pathname-strip-extension path))
            (newline))
          (command-line-arguments))
