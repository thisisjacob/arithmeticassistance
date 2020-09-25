#lang racket
(require racket/gui/base)

(define menuBar%
  (class object%
    (init-field
     menuParent
     )
    (super-new)
    (define bar (new menu-bar% (parent menuParent)))
    (define/public (activate)
      (send bar enable 0)
    )
    ))

(provide menuBar%)
                