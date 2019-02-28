#lang racket

;; Neste exercício vamos usar testes,
;; o objetivo é terminar com todos os testes passando
(require rackunit rackunit/text-ui)

;; --- Exercício 8 ---------------------

;; Crie uma função recursiva soma-lista (abaixo) que, dada uma lista de números,
;; calcula a soma dos números contidos
(define (soma-lista l acc)
		(if (empty? l) 
			acc
			(soma-lista (rest l) (+ (first l) acc))	
		)
)

(define-test-suite testes-soma-lista
  (test-equal? "soma da lista vazia"                (soma-lista '() 0)                  0)
  (test-equal? "soma de um número apenas"           (soma-lista '(13) 0)                13)
  (test-equal? "soma de vários números"             (soma-lista (list 5 4 3 2 1) 0)     15)
  (test-equal? "soma de números em ordem diferente" (soma-lista (list 1 2 3 4 5) 0)     15)
	(test-equal? "soma de lista com zero"             (soma-lista (list 1 0 2 0 13 0) 0)  16))
	

	;; --- Executa todos os testes ---------
(run-tests
	(test-suite "todos os testes"
							testes-soma-lista
							))
 