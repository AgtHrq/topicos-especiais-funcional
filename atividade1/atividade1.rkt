#lang racket

;; 1
(define a 42)

;; 2
(define b 14)

;; 3.1
(+ a b)

;; 3.2
(- a b)

;; 3.3
(+ a (* 3 b) 7)

;; 3.4
(/ (+ a b) 2)

;; 3.5
(sqrt (* a b))

;; 3.6
(/ 2 (+ (/ 1 a) (/ 1 b)))

;; 4
(define soma-medias (+ (/ (+ a b) 2) (/ 2 (+ (/ 1 a) (/ 1 b)))))

;; 5
(if (= soma-medias 49) "teste 1 ok" "teste 1 falhou")

;; 6
(define (quadrado x)(* x x))

;; 7
(define (delta a b c)
  (- (quadrado b) (* 4 a c)))

(define (raiz-positiva a b c)
  (/ (+ (- b) (sqrt (delta a b c)))
     (* 2 a)))

;; 8
(define (potencia base expoente)
  (if (= expoente 0) 1 (* base (potencia base (- expoente 1)))))

;; 8 testes
(if (= (potencia 2 2) 4) "Test: 2^2 = 4 OK" "2^2 != 4 ERROR")
(if (= (potencia 2 3) 8) "Test: 2^3 = 8 OK" "2^3 != 8 ERROR")
(if (= (potencia 5 5) 3125) "Test: 5^5 = 3125 OK" "5^5 != 3125 ERROR")