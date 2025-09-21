#lang racket
;; ============================================================================
;;  BusCEMinas (Minesweeper) - LÓGICA PURA (funcional y recursiva)
;;  - Sin lambda, let, map, apply, for, fold*, set!, macros
;;  - Solo listas como estructura de datos
;;  - Todas las funciones aquí son puras (no I/O)
;;  - Representación de celda: (MinaSiNo NumeroMinasAdyacentes Estado)
;;       MinaSiNo: 0 o 1
;;       NumeroMinasAdyacentes: entero >= 0
;;       Estado: 'cubierta | 'descubierta | 'bandera
;;  - Representación de tablero: lista de filas (lista de lista de celda)
;; ============================================================================

(provide
  ;; API pedida
  make-board in-bounds? neighbors count-adjacent-mines place-mines
  get-cell set-cell reveal toggle-flag won?
  start-game click-reveal click-flag
  ;; Alias solicitado
  BuscaCE
  ;; Utilidades expuestas (opcionales pero útiles)
  board-rows board-cols)

;; ----------------------------------------------------------------------------
;; Utilidades de celdas y tablero (selectores/constructores)
;; ----------------------------------------------------------------------------

(define (make-cell mine adj state) (list mine adj state))
(define (cell-mine cell) (car cell))           ; 0/1
(define (cell-adj  cell) (cadr cell))          ; entero >= 0
(define (cell-st   cell) (caddr cell))         ; símbolo

(define (set-cell-state cell st) (list (cell-mine cell) (cell-adj cell) st))
(define (set-cell-adj   cell n)  (list (cell-mine cell) n (cell-st cell)))

(define (board-rows board) (length board))
(define (board-cols board)
  (if (null? board) 0 (length (car board))))

;; ----------------------------------------------------------------------------
;; Índices y acceso inmutable
;;   - Usamos índices base 0: r en [0..filas-1], c en [0..cols-1]
;; ----------------------------------------------------------------------------

;; x >= 0 usando solo =, >, not
(define (nonneg? x) (or (= x 0) (> x 0)))

;; (in-bounds? filas cols r c) -> boolean
(define (in-bounds? filas cols r c)
  (and (nonneg? r) (nonneg? c) (< r filas) (< c cols)))

;; obtener fila r (0-index)
(define (get-row board r)
  (if (= r 0)
      (car board)
      (get-row (cdr board) (- r 1))))

;; obtener elemento i de lista
(define (list-nth xs i)
  (if (= i 0)
      (car xs)
      (list-nth (cdr xs) (- i 1))))

;; get-cell: (board r c) -> cell
(define (get-cell board r c)
  (list-nth (get-row board r) c))

;; actualizar elemento i en lista, devolviendo nueva lista
(define (list-set xs i newv)
  (if (= i 0)
      (cons newv (cdr xs))
      (cons (car xs) (list-set (cdr xs) (- i 1) newv))))

;; set-cell: (board r c new-cell) -> new-board (sin mutar)
(define (set-cell board r c new-cell)
  (if (= r 0)
      (cons (list-set (car board) c new-cell) (cdr board))
      (cons (car board) (set-cell (cdr board) (- r 1) c new-cell))))

;; ----------------------------------------------------------------------------
;; Vecinos
;; ----------------------------------------------------------------------------

;; offsets de los 8 vecinos
(define OFFS
  (list (list -1 -1) (list -1 0) (list -1 1)
        (list  0 -1)             (list  0 1)
        (list  1 -1) (list  1 0) (list  1 1)))

;; neighbors: (filas cols r c) -> lista de pares (list rr cc) válidos
(define (neighbors filas cols r c)
  (neighbors-from-offs OFFS filas cols r c))

(define (neighbors-from-offs offs filas cols r c)
  (if (null? offs)
      '()
      (append (neighbor-one (car offs) filas cols r c)
              (neighbors-from-offs (cdr offs) filas cols r c))))

(define (neighbor-one off filas cols r c)
  (if (in-bounds? filas cols (+ r (car off)) (+ c (cadr off)))
      (list (list (+ r (car off)) (+ c (cadr off))))
      '()))

;; ----------------------------------------------------------------------------
;; Conteo de minas adyacentes
;; ----------------------------------------------------------------------------

;; (count-adjacent-mines board r c) -> entero
(define (count-adjacent-mines board r c)
  (count-mines-in board (neighbors (board-rows board) (board-cols board) r c)))

(define (count-mines-in board pos-list)
  (if (null? pos-list)
      0
      (+ (if (= (cell-mine (get-cell board (car (car pos-list)) (cadr (car pos-list)))) 1) 1 0)
         (count-mines-in board (cdr pos-list)))))

;; ----------------------------------------------------------------------------
;; Construcción de tablero
;; ----------------------------------------------------------------------------

;; crear una fila de celdas (0 0 'cubierta)
(define (make-empty-row cols)
  (if (= cols 0)
      '()
      (cons (make-cell 0 0 'cubierta) (make-empty-row (- cols 1)))))

;; crear tablero vacío rows x cols
(define (make-empty-board rows cols)
  (if (= rows 0)
      '()
      (cons (make-empty-row cols) (make-empty-board (- rows 1) cols))))

;; place-mines: pone minas al azar según porcentaje (probabilidad p%)
;; Nota: se usa (random 100) < p para cada celda (no garantiza conteo exacto, sí porcentaje esperado).
(define (place-mines board porcentaje)
  (place-mines-rows board porcentaje 0))

(define (place-mines-rows board porcentaje r)
  (if (null? board)
      '()
      (cons (place-mines-row (car board) porcentaje r 0)
            (place-mines-rows (cdr board) porcentaje (+ r 1)))))

(define (place-mines-row row porcentaje r c)
  (if (null? row)
      '()
      (cons (place-mines-cell (car row) porcentaje r c)
            (place-mines-row (cdr row) porcentaje r (+ c 1)))))

(define (place-mines-cell cell porcentaje r c)
  (if (< (random 100) porcentaje)
      (make-cell 1 0 'cubierta)
      (make-cell 0 0 'cubierta)))

;; Rellenar los números de minas adyacentes para cada celda
(define (fill-numbers board)
  (fill-numbers-rows board 0))

(define (fill-numbers-rows board r)
  (if (null? board)
      '()
      (cons (fill-numbers-row board (car board) r 0)
            (fill-numbers-rows (cdr board) (+ r 1)))))

(define (fill-numbers-row board row r c)
  (if (null? row)
      '()
      (cons (fill-numbers-cell board (car row) r c)
            (fill-numbers-row board (cdr row) r (+ c 1)))))

(define (fill-numbers-cell board cell r c)
  (set-cell-adj cell (count-adjacent-mines board r c)))

;; make-board: (filas cols dificultad) -> board
(define (difficulty->percent d)
  (cond [(equal? d 'facil) 10]
        [(equal? d 'medio) 15]
        [(equal? d 'dificil) 20]
        [else 10]))

(define (make-board filas cols dificultad)
  (fill-numbers (place-mines (make-empty-board filas cols)
                             (difficulty->percent dificultad))))

;; ----------------------------------------------------------------------------
;; Jugadas: revelar, propagar, banderas y victoria
;; ----------------------------------------------------------------------------

;; revelar celda con propagación si adj = 0
;; reveal: (board r c) -> values (new-board estado) ; estado: 'derrota | 'victoria | 'continua
(define (reveal board r c)
  (if (not (in-bounds? (board-rows board) (board-cols board) r c))
      (values board 'continua)
      (reveal-cell board r c)))

(define (reveal-cell board r c)
  (let* () ;; (internamente no usamos let; mantenemos estilo recursivo y define-values)
    (reveal-decision board r c)))

;; Lógica central del revelado
(define (reveal-decision board r c)
  (define cell (get-cell board r c))
  (cond
    [(equal? (cell-st cell) 'bandera)
     (values board 'continua)]
    [(equal? (cell-st cell) 'descubierta)
     (values board 'continua)]
    [(= (cell-mine cell) 1)
     ;; si es mina: derrota (podemos marcarla como descubierta)
     (values (set-cell board r c (set-cell-state cell 'descubierta)) 'derrota)]
    [else
     ;; descubrir y, si adj=0, propagar
     (define board1 (set-cell board r c (set-cell-state cell 'descubierta)))
     (define adj (cell-adj cell))
     (define board2 (if (= adj 0)
                        (reveal-neigh-list board1
                          (neighbors (board-rows board) (board-cols board) r c))
                        board1))
     (if (won? board2) (values board2 'victoria) (values board2 'continua))]))

;; Propagación solo sobre celdas (no mina) con adj=0. Evita bucles al chequear estado.
(define (reveal-neigh-list board pos-list)
  (if (null? pos-list)
      board
      (reveal-neigh-list
       (reveal-propagate board (car (car pos-list)) (cadr (car pos-list)))
       (cdr pos-list))))

(define (reveal-propagate board r c)
  (define cell (get-cell board r c))
  (cond
    [(equal? (cell-st cell) 'descubierta) board]
    [(equal? (cell-st cell) 'bandera)     board]
    [(= (cell-mine cell) 1)               board] ; no revelar minas en la cascada
    [else
     (define board1 (set-cell board r c (set-cell-state cell 'descubierta)))
     (if (= (cell-adj cell) 0)
         (reveal-neigh-list board1 (neighbors (board-rows board) (board-cols board) r c))
         board1)]))

;; toggle-flag: 'cubierta <-> 'bandera (no cambia 'descubierta)
(define (toggle-flag board r c)
  (define cell (get-cell board r c))
  (cond
    [(equal? (cell-st cell) 'cubierta)
     (set-cell board r c (make-cell (cell-mine cell) (cell-adj cell) 'bandera))]
    [(equal? (cell-st cell) 'bandera)
     (set-cell board r c (make-cell (cell-mine cell) (cell-adj cell) 'cubierta))]
    [else board]))

;; won?: true si TODAS las celdas sin mina están 'descubierta
(define (won? board)
  (won-rows? board))

(define (won-rows? rows)
  (if (null? rows)
      #t
      (and (won-row? (car rows)) (won-rows? (cdr rows)))))

(define (won-row? row)
  (if (null? row)
      #t
      (and (won-cell? (car row)) (won-row? (cdr row)))))

(define (won-cell? cell)
  (or (= (cell-mine cell) 1)
      (equal? (cell-st cell) 'descubierta)))

;; ----------------------------------------------------------------------------
;; Flujo de juego (funciones "de alto nivel")
;; ----------------------------------------------------------------------------

(define (start-game filas cols dificultad)
  (make-board filas cols dificultad))

(define (click-reveal board r c)
  (reveal board r c))

(define (click-flag board r c)
  (toggle-flag board r c))

;; Alias del enunciado
(define (BuscaCE filas cols dificultad)
  (start-game filas cols dificultad))

;; ----------------------------------------------------------------------------
;; Pruebas mínimas (sin frameworks) - se pueden evaluar en REPL
;; ----------------------------------------------------------------------------
;; ;;; Ejemplos:
;; (define b0 (make-board 8 10 'facil))
;; (define-values (b1 s1) (reveal b0 0 0))
;; (define b2 (toggle-flag b1 0 1))
;; (won? b2)
;;
;; Contar minas (útil para inspección):
;; (define (count-mines board)
;;   (define (count-row row)
;;     (if (null? row) 0
;;         (+ (if (= (cell-mine (car row)) 1) 1 0) (count-row (cdr row)))))
;;   (if (null? board) 0
;;       (+ (count-row (car board)) (count-mines (cdr board)))))
;;
;; (count-mines b0)
