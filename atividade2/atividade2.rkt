#lang racket

;; Neste exercício vamos usar testes,
;; o objetivo é terminar com todos os testes passando
(require rackunit rackunit/text-ui)

;; --- Exercício 1 ---------------------

;; Crie uma função (mult m n) que multiplica os dois números
;; naturais m e n, usando apenas a operação de soma.

;; aqui está uma definição incorreta da função, para que os testes funcionem.
;; para resolver o exercício deve-se alterar o corpo da função para uma versão
;; que faça os testes passarem
(define (mult m n)
  (if (= n 0) 0 (if (= n 1) m (+ m (mult m (- n 1)))))
)

(define-test-suite testes-mult
  (test-equal? "3 * 4"  (mult 3 4)    12)
  (test-equal? "5 * 0"  (mult 5 0)    0)
  (test-equal? "0 * 5"  (mult 0 5)    0)
  (test-equal? "13 * 1" (mult 13 1)   13)
  (test-equal? "1 * 13" (mult 1 13)   13))

;; --- Exercício 2 ---------------------

;; Crie uma função (sub m n) que calcula a subtração de m por n,
;; usando apenas as funções add1 e sub1. Pode ser assumido que
;; m >= n, mas não é difícil escrever uma função que funcione mesmo
;; quando m < n.

(define (sub m n)
  (if (= n 0) m (sub (- m 1) (- n 1)))
)

(define-test-suite testes-sub
  (test-equal? "42 - 0"  (sub 42 0)   42)
  (test-equal? "32 - 16" (sub 32 16)  16)
  (test-equal? "42 - 42" (sub 42 42)  0)
  (test-equal? "11 - 10" (sub 11 10)  1)
  (test-equal? "11 - 10" (sub 10 15)  (- 5))
  (test-equal? "10 - 11" (sub 10 11)  (- 1)))

;; --- Executa todos os testes ---------
(run-tests
  (test-suite "todos os testes"
              testes-mult
              testes-sub
              ))