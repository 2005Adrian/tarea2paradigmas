#lang racket/gui
;; ============================================================================
;;  BusCEMinas - GUI (paneles anidados + colores + contador + revelar minas)
;;  - Lógica pura en logic.rkt (no se toca)
;;  - En la GUI usamos variables, for, lambdas, etc.
;; ============================================================================

(require "logic.rkt"
         racket/list
         racket/class
         racket/draw) ; color% / font%

;; ---------------------------------------------------------------------------
;; Selectores locales de celda (no dependemos de exports extra de logic.rkt)
;; ---------------------------------------------------------------------------
(define (cell-get-mine cell) (car cell))
(define (cell-get-adj  cell) (cadr cell))
(define (cell-get-st   cell) (caddr cell))

;; ---------------------------------------------------------------------------
;; Colores / fuentes
;; ---------------------------------------------------------------------------
(define (rgb r g b) (make-object color% r g b))

;; Fondos
(define bg-covered (rgb 245 245 245))  ; tapada
(define bg-visited (rgb 220 235 255))  ; descubierta
(define bg-flagged (rgb 255 240 200))  ; bandera
(define bg-bomb    (rgb 255 220 220))  ; mina revelada (derrota)

;; Textos
(define fg-covered (rgb 0 0 0))
(define fg-flagged (rgb 120 80 0))
(define fg-bomb    (rgb 128 0 0))

;; Colores típicos de números 1..8
(define (num->fg n)
  (cond [(= n 1) (rgb 0 0 255)]
        [(= n 2) (rgb 0 128 0)]
        [(= n 3) (rgb 255 0 0)]
        [(= n 4) (rgb 0 0 128)]
        [(= n 5) (rgb 128 0 0)]
        [(= n 6) (rgb 0 128 128)]
        [(= n 7) (rgb 0 0 0)]
        [(= n 8) (rgb 128 128 128)]
        [else fg-covered]))

;; Setters tolerantes (algunos temas no soportan estos métodos)
(define (try-set-bg w col)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (send w set-background col)))
(define (try-set-fg w col)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (send w set-foreground col)))
(define (try-set-font w size)
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (send w set-font (make-object font% size 'default 'normal 'normal))))

;; ---------------------------------------------------------------------------
;; Parámetros por defecto / Estado
;; ---------------------------------------------------------------------------
(define DEFAULT-ROWS 10)
(define DEFAULT-COLS 10)

(define current-rows DEFAULT-ROWS)
(define current-cols DEFAULT-COLS)
(define current-difficulty 'facil)
(define flag-mode? #f)
(define current-board (start-game current-rows current-cols current-difficulty))

;; Botones en matriz 2D (lista de filas)
(define buttons '())

;; ---------------------------------------------------------------------------
;; Ventana y controles superiores
;; ---------------------------------------------------------------------------
(define frame (new frame% [label "BusCEMinas (GUI)"]))

(define top (new horizontal-panel% [parent frame]))
(new message% [parent top] [label "Filas:"])
(define ch-rows
  (new choice%
       [parent top]
       [label ""]
       [choices (for/list ([n (in-range 8 16)]) (number->string n))]
       [callback (lambda (_ e) (void))]))
(send ch-rows set-selection (- DEFAULT-ROWS 8))

(new message% [parent top] [label "Cols:"])
(define ch-cols
  (new choice%
       [parent top]
       [label ""]
       [choices (for/list ([n (in-range 8 16)]) (number->string n))]
       [callback (lambda (_ e) (void))]))
(send ch-cols set-selection (- DEFAULT-COLS 8))

(new message% [parent top] [label "Dificultad:"])
(define rb-diff
  (new radio-box%
       [parent top]
       [label ""]
       [choices (list "Fácil" "Medio" "Difícil")]
       [style '(horizontal)]
       [callback (lambda (_ e) (void))]))
(send rb-diff set-selection 0)

(define cb-flag
  (new check-box%
       [parent top]
       [label "Modo bandera"]
       [value #f]
       [callback (lambda (_ e)
                   (set! flag-mode? (send cb-flag get-value)))]))

(define btn-new
  (new button%
       [parent top]
       [label "Nueva partida"]
       [callback (lambda (_ e) (start-new-game!))]))

;; Panel de estado (contador)
(define status-panel (new horizontal-panel% [parent frame]))
(define status-msg
  (new message% [parent status-panel]
       [label "Minas: -   Banderas: -   Restantes: -"]))

;; Panel del tablero (contiene filas horizontales)
(define board-panel (new vertical-panel% [parent frame] [stretchable-height #t]))

;; ---------------------------------------------------------------------------
;; Contadores y actualización de estado
;; ---------------------------------------------------------------------------
(define (count-mines board)
  (for*/sum ([r (in-range current-rows)]
             [c (in-range current-cols)])
    (if (= (cell-get-mine (get-cell board r c)) 1) 1 0)))

(define (count-flags board)
  (for*/sum ([r (in-range current-rows)]
             [c (in-range current-cols)])
    (if (equal? (cell-get-st (get-cell board r c)) 'bandera) 1 0)))

(define (update-status!)
  (define total (count-mines current-board))
  (define flgs  (count-flags current-board))
  (define rest  (max 0 (- total flgs)))
  (send status-msg set-label
        (format "Minas: ~a   Banderas: ~a   Restantes: ~a" total flgs rest)))

;; ---------------------------------------------------------------------------
;; Revelar minas al perder (solo visual, GUI)
;; ---------------------------------------------------------------------------
(define (reveal-all-mines board)
  ;; Devuelve un nuevo tablero con todas las minas en 'descubierta
  (for*/fold ([b board])
             ([r (in-range current-rows)]
              [c (in-range current-cols)])
    (let ([cell (get-cell b r c)])
      (if (= (cell-get-mine cell) 1)
          (set-cell b r c (list 1 (cell-get-adj cell) 'descubierta))
          b))))

;; ---------------------------------------------------------------------------
;; Lógica GUI
;; ---------------------------------------------------------------------------
(define (difficulty-from-radio sel)
  (cond [(= sel 0) 'facil] [(= sel 1) 'medio] [else 'dificil]))

(define (refresh-all!)
  (for ([r (in-range current-rows)])
    (for ([c (in-range current-cols)])
      (refresh-cell! r c)))
  (update-status!))

;; Pinta una celda según su estado
(define (refresh-cell! r c)
  (define btn  (list-ref (list-ref buttons r) c))
  (define cell (get-cell current-board r c))
  (cond
    [(equal? (cell-get-st cell) 'cubierta)
     (send btn enable #t)
     (try-set-bg btn bg-covered)
     (try-set-fg btn fg-covered)
     (try-set-font btn 10)
     (send btn set-label "")]
    [(equal? (cell-get-st cell) 'bandera)
     (send btn enable #t)
     (try-set-bg btn bg-flagged)
     (try-set-fg btn fg-flagged)
     (try-set-font btn 10)
     (send btn set-label "F")]
    [else
     (send btn enable #f)
     (let ([is-bomb (= (cell-get-mine cell) 1)])
       (try-set-bg btn (if is-bomb bg-bomb bg-visited))
       (if is-bomb
           (begin
             (try-set-fg btn fg-bomb)
             (try-set-font btn 12)
             (send btn set-label "💣"))
           (let ([n (cell-get-adj cell)])
             (try-set-fg btn (if (= n 0) fg-covered (num->fg n)))
             (try-set-font btn 11)
             (send btn set-label (if (= n 0) "" (number->string n))))))]))

;; Click en celda
(define (cell-click! r c)
  (if flag-mode?
      (set! current-board (click-flag current-board r c))
      (let-values ([(nb st) (click-reveal current-board r c)])
        (cond
          [(equal? st 'derrota)
           ;; Mostrar TODAS las minas, refrescar, y luego avisar
           (set! current-board (reveal-all-mines nb))
           (refresh-all!)
           (message-box "Derrota" "¡Pisaste una mina! Aquí estaban todas las bombas." frame)]
          [else
           (set! current-board nb)
           (refresh-all!)
           (when (equal? st 'victoria)
             (message-box "Victoria" "¡Limpiaste el tablero!" frame))])))
  ;; refresh ya se hace en cada rama
  )

;; Borra todos los hijos de un panel (evita apilar filas al reiniciar)
(define (remove-all-children! p)
  (for ([child (in-list (send p get-children))])
    (send p delete-child child)))

;; Construye la grilla de botones (reutiliza el mismo board-panel)
(define (build-board!)
  (remove-all-children! board-panel)
  (set! buttons
        (for/list ([r (in-range current-rows)])
          (define row-panel (new horizontal-panel% [parent board-panel]))
          (for/list ([c (in-range current-cols)])
            (let ([b (new button% [parent row-panel] [label ""]
                           [callback (lambda (_ e) (cell-click! r c))])])
              (try-set-bg b bg-covered)
              (try-set-fg b fg-covered)
              (try-set-font b 10)
              b))))
  (send board-panel reflow-container)
  (refresh-all!))

(define (read-controls!)
  (set! current-rows (+ 8 (send ch-rows get-selection)))
  (set! current-cols (+ 8 (send ch-cols get-selection)))
  (set! current-difficulty (difficulty-from-radio (send rb-diff get-selection)))
  (set! flag-mode? (send cb-flag get-value)))

(define (start-new-game!)
  (read-controls!)
  (set! current-board (start-game current-rows current-cols current-difficulty))
  (build-board!))

;; ---------------------------------------------------------------------------
;; Lanzamiento
;; ---------------------------------------------------------------------------
(build-board!)
(send frame show #t)
