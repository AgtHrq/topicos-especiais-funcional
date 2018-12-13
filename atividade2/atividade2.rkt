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


  ;; --- Executa todos os testes ---------
(run-tests
  (test-suite "todos os testes"
              testes-mult
              ))