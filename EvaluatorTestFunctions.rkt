(require "evaluator.rkt")

(define bar (open-input-file "DND/class/barbarian/level1.csv")

(display (csv->list bar))