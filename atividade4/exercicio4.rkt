#lang racket


(require rackunit rackunit/text-ui)


;; --- Exercício 1 ---------------------

;; Crie uma função recursiva soma-lista (abaixo) que, dada uma lista de números,
;; calcula a soma dos números contidos
(define (soma-lista l acc)
    (if (empty? l) acc
        (soma-lista (rest l) (+ (first l) acc)))
)

(define-test-suite testes-soma-lista
  (test-equal? "soma da lista vazia"                (soma-lista '() 0)                  0)
  (test-equal? "soma de um número apenas"           (soma-lista '(13) 0)                13)
  (test-equal? "soma de vários números"             (soma-lista (list 5 4 3 2 1) 0)     15)
  (test-equal? "soma de números em ordem diferente" (soma-lista (list 1 2 3 4 5) 0)     15)
  (test-equal? "soma de lista com zero"             (soma-lista (list 1 0 2 0 13 0) 0)  16))

;; --- Exercício 2 ---------------------

;; Crie uma função recursiva mult-lista (abaixo) que, dada uma lista de números,
;; calcula o produto dos números contidos (a lista vazia deve ter produto igual a 1)
(define (mult-lista l acc)
  (if (empty? l) acc

      (mult-lista (rest l) (* (first l) acc))

      ))


(define-test-suite testes-mult-lista
  (test-equal? "produto da lista vazia"            (mult-lista '() 1)                  1)
  (test-equal? "produto de lista com zero"         (mult-lista (list 1 0 2 0 13 0) 1)  0)
  (test-equal? "produto de um número"              (mult-lista '(55) 1)                55)
  (test-equal? "produto de vários números"         (mult-lista (list 1 2 3 4 5) 1)     120)
  (test-equal? "produto de números em outra ordem" (mult-lista (list 2 5 1 4 3) 1)     120))

;; --- Exercício 3 --------------------

;; Crie uma função recursiva max-lista (abaixo) que, dada uma lista de números naturais,
;; calcula o maior número entre os presentes na lista. Use (max-lista '()) = 0.
;(define (max-lista l)
;  (if (empty? l) 0 (if (> (first l (max-lista (rest l))))  (first l) (max-lista (rest l)) ))
;)

(define (max-lista l acc)
  (if(empty? l) acc

     (if (> (first l) acc) (max-lista (rest l) (first l))
         (max-lista (rest l) acc))
         )
     )

(define-test-suite testes-max-lista
  (test-equal? "maximo da lista vazia"       (max-lista '() 0)                     0)
  (test-equal? "maximo de lista unitaria"    (max-lista '(22) 0)                   22)
  (test-equal? "maximo de lista com numeros" (max-lista (list 8 55 13 24 45) 0)    55)
  (test-equal? "maximo não muda com ordem"   (max-lista (list 45 13 8 55 24) 0)    55)
  (test-equal? "maximo de lista com zeros"   (max-lista (list 1 0 13 0 356 0) 0)   356))



;; --- Exercício 4 --------------------

;; Muitas vezes precisamos transformar os elementos de uma lista da mesma
;; maneira. Escreva a função quadrado-lista (abaixo) que, dada uma lista de
;; números, obtém uma lista contendo o quadrado de cada número da lista
;; original (nas mesmas posições)
(define (quadrado-lista l acc)
  (if (empty? l) acc
  (quadrado-lista (rest l)(append acc (cons (*(first l)(first l))'()) ))
  )
)

(define-test-suite testes-quadrado-lista
  (test-equal? "quadrado da lista vazia"  (quadrado-lista '() '())        '())
  (test-equal? "quadrado de um número"    (quadrado-lista '(5) '())       '(25))
  (test-equal? "quadrado de números"
               (quadrado-lista (list 2 5 12 25) '())
               (list 4 25 144 625)))

;; --- Exercício 5 --------------------

;; Agora vamos selecionar itens em uma lista. Crie uma função filtra-par (abaixo)
;; que, dado uma lista de números naturais, retorna uma outra lista contendo apenas
;; os números pares da lista original. Use a função par definida no exercício 3

(define (par n)
	(if (negative? n) #f (if (= n 0) #t (par (- n 2))))
)

(define (filtra-par l acc)
  (if (empty? l) acc
      
  (if (par (first l)) (filtra-par (rest l)(append acc (cons (first l) '())))
  (filtra-par (rest l) acc)
  )
  ))

(define-test-suite testes-filtra-par
  (test-equal? "filtragem da lista vazia"     (filtra-par '() '())                  '())
  (test-equal? "filtragem de lista sem pares" (filtra-par (list 1 3 5 7 9) '())     '())
  (test-equal? "filtragem de lista com pares" (filtra-par (list 1 2 3 4 5) '())     (list 2 4))
  (test-equal? "filtragem com todos os itens pares"
               (filtra-par (list 2 4 22 144) '())
               (list 2 4 22 144)))

;; --- Questão 6 ----------------------------

;; Escreva uma função remove-primeiro tal que
;; (remove-primeiro x lst) remove a primeira ocorrência do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
;; Veja os testes para exemplos.
(define (remove-primeiro x lst acc)
  (if (empty? lst) 
    acc
    (if (equal? (first lst) x) 
    (append acc (rest lst))
    (remove-primeiro x (rest lst) (append acc (cons (first lst) '()))
    )
  )
))

(define-test-suite test-remove-primeiro
  (test-equal? "lista vazia"
               (remove-primeiro 5 '() '())              '())
  
  (test-equal? "uma ocorrência"
               (remove-primeiro 5 '(1 3 5 7) '())       '(1 3 7))
  
  (test-equal? "múltiplas ocorrências"
               (remove-primeiro 5 '(1 3 5 7 5 9) '())   '(1 3 7 5 9))
  
  (test-equal? "nenhuma ocorrência"
               (remove-primeiro 3 '(11 7 23 55 42) '()) '(11 7 23 55 42)))


;; --- Questão 7 ----------------------------

;; Escreva uma função remove-todos tal que
;; (remove-todos x lst) remove todas as ocorrencias do elemento x
;; na lista lst (se houver), retornando uma nova lista com o resultado.
(define (remove-todos x lst acc)
  (if (empty? lst) 
    acc
    (if (equal? (first lst) x) 
     (remove-todos x (rest lst) acc)
     (remove-todos x (rest lst) (append acc (cons (first lst) '())))
    )
  )
)

(define-test-suite test-remove-todos
  (test-equal? "lista vazia"           (remove-todos 5 '() '())              '())
  (test-equal? "uma ocorrência"        (remove-todos 5 '(1 3 5 7) '())       '(1 3 7))
  (test-equal? "múltiplas ocorrências" (remove-todos 5 '(1 3 5 7 5 9) '())   '(1 3 7 9))
  (test-equal? "nenhuma ocorrência"    (remove-todos 3 '(11 7 23 55 42) '()) '(11 7 23 55 42)))


;; --- Questão 8 ----------------------------

;; As funções remove-primeiro e remove-todos, acima, funcionam apenas para
;; listas de números, ou também funcionam para listas de outros tipos de
;; elementos, como strings? Funciona com listas heterogêneas (com elementos
;; de tipos diferentes na mesma lista)? Faça alguns testes que demonstram se
;; funcionam ou não.
(test-equal? "remover primeiro lista com strings" (remove-primeiro "a" '("b" "a" "c" "a") '()) '("b" "c" "a"))
(test-equal? "remover todos lista com strings" (remove-todos "a" '("a" "b" "c" "a" "d" "e") '()) '("b" "c" "d" "e"))
(test-equal? "remover primeiro lista heterogênia" (remove-primeiro "a" '("a" 1 "a" 4 5) '()) '( 1 "a" 4 5))
(test-equal? "remover todos lista heterogênia" (remove-todos "a" '("a" 1 "a" 4 5 "a" 9 "e") '()) '( 1  4 5  9 "e"))


;; --- Questão 9 ----------------------------

;; Listas podem ser usadas como base para a criação de várias outras estruturas
;; de dados. Embora raramente uma implementação baseada em listas seja a mais
;; rápida, pode ser utilizada para conjuntos de dados pequenos e é fácil de criar
;; em uma linguagem funcional.

;; Uma estrutura de dados que pode ser construída em cima das listas são conjuntos.
;; Conjuntos são similares às listas, mas podem ter apenas uma ocorrência de cada
;; elemento. Algumas operações normalmente usadas com conjuntos são a união,
;; intersecção, diferença de conjuntos e teste de pertencimento.

;; Para o teste de pertencimento podemos continuar usando a receita recursiva baseada
;; na estrutura das listas:

;; Escreva uma função pertence? tal que
;; (pertence? x lst) retorna #t se o elemento x aparece na lista (conjunto) lst
(define (pertence? x lst acc)
  (if (empty? lst)
    acc
    (if (equal? x (first lst)) 
      
      (pertence? x (rest lst) #t)
      (pertence? x (rest lst) acc)
    )
  )
)

(define-test-suite test-pertence?
  (test-false "lista vazia"    (pertence? 5 '() #f))
  (test-true  "3 pertence"     (pertence? 3 '(1 2 3 4 5) #f))
  (test-false "9 não pertence" (pertence? 9 '(1 2 3 4 5) #f))
  (test-true  "5 pertence"     (pertence? 5 '(1 2 3 4 5) #f)))


;; --- Questão 10 ----------------------------

;; Infelizmente nem sempre podemos usar a mesma receita para recursividade. Um caso
;; comum são funções que devem combinar duas listas de alguma forma (como é o caso
;; das operações de união, intersecção e diferença de conjuntos).

;; Para praticar a ideia primeiro, escreva uma função combine tal que
;; (combine l1 l2) retorna uma lista de pares (listas de dois elementos) onde o primeiro
;; elemento de cada par vem de l1 e o segundo de l2. O número de pares deve ser igual ao
;; tamanho da menor lista. Veja os testes para exemplos.

(define (combine l1 l2 acc)
  (cond
    [(or (empty? l1) (empty? l2)) acc]  
    [else 
      (combine (rest l1) (rest l2) (append acc (cons (cons (first l1) (cons (first l2) '())) '())))
    ]
  )
)

(define-test-suite test-combine
  (test-equal? "listas de mesmo tamanho"
               (combine '(1 2 3) '(10 20 30) '())  '((1 10) (2 20) (3 30)))
  
  (test-equal? "listas de tamanho diferente"
               (combine '(1)     '(55 33 11) '())  '((1 55)))
  
  (test-equal? "primeira lista vazia"
               (combine '()      '(1 2 3) '())     '())
  
  (test-equal? "segunda lista vazia"
               (combine '(1 2 3) '() '())          '())
  
  (test-equal? "segunda lista menor"
               (combine '(4 5 6) '(22 33) '())     '((4 22) (5 33))))


;; --- Questão 11 ----------------------------

;; Antes de trabalhar com conjuntos, é interessante ter algumas funções de apoio.

;; Além da falta de itens duplicados, outra característica dos conjuntos é a
;; ausência de ordem. As listas (1 2 3), (3 1 2), (2 3 1) etc todas representam
;; o mesmo conjunto. Por isso, não podemos usar equal? para testar igualdade de
;; conjuntos.

;; Mesmo nos testes, podemos ter diferentes implementações das operações de conjuntos,
;; ambas corretas, mas que retornam os elementos em uma ordem diferente (por
;; exemplo (uniao '(1 2 5) (2 5 3)) pode retornar (1 2 3 5) ou (3 2 1 5), ambos
;; os resultados corretos).

;; Escreva uma função conjunto=? tal que
;; (conjunto=? c1 c2) retorna #t se c1 e c2 contêm os mesmos elementos, não
;; necessariamente na mesma ordem, e #f caso exista algum elemento que pertence
;; a um mas não a outro.
(define (conjunto=? c1 c2)
  (cond
    [(and (empty? c1) (empty? c2)) #t]
    [(and (empty? c1) (not (empty? c2))) #f]
    [(pertence? (first c1) c2 #f) (conjunto=? (rest c1) (remove-primeiro (first c1) c2 '()))]
    [else #f]
  )
)


(define-test-suite test-conjunto=?
  (test-true  "conjuntos vazios"        (conjunto=? '() '()))
  (test-false "vazio e unitário"        (conjunto=? '() '(1)))
  (test-true  "conjs. unitários"        (conjunto=? '(1) '(1)))
  (test-true  "iguais, mesma ordem"     (conjunto=? '(1 2 3) '(1 2 3)))
  (test-true  "iguais, ordem diferente" (conjunto=? '(1 2 3) '(1 3 2)))
  (test-true  "ordem diferente"         (conjunto=? '(2 1 3) '(2 3 1)))
  (test-false "(1 2 3) e (1 2 5)"       (conjunto=? '(1 2 3) '(1 2 5)))
  (test-false "(3 2 1) e (1 3 7)"       (conjunto=? '(3 2 1) '(1 3 7)))
  (test-false "(1 3) e (1 3 5 7)"       (conjunto=? '(1 3) '(1 3 5 7)))
)


;; --- Questão 12 ----------------------------

;; Outra função de apoio que pode ser útil é uma que, dada uma
;; lista qualquer (podendo conter itens duplicados) retorna uma lista válida como
;; conjunto, sem itens duplicados. Podemos chamar essa função remove-duplicatas.

;; Escreva a função remove-duplicatas tal que
;; (remove-duplicatas lst) retorna uma lista com os mesmos elementos de lst mas
;; sem que nenhum item ocorra mais de uma vez.
(define (remove-duplicatas lst acc)
  (cond
    [(empty? lst) acc]
    [else (remove-duplicatas (remove-todos (first lst) lst '()) (append acc (cons (first lst) '())))]
  )

)

;; Um outro nome para a mesma função poderia ser lista->conjunto, enfatizando a
;; sua aplicação na criação de conjuntos a partir de listas. Nesse caso podemos
;; definir um sinônimo para a mesma função acima
(define lista->conjunto remove-duplicatas)

;; Note que usamos conjunto=? nos testes, caso contrário funções que retornassem
;; elementos em ordens diferentes não passariam
(define-test-suite test-remove-duplicatas
  (test-true "sem duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 4 5) '()) '(1 2 3 4 5)))
  
  (test-true "lista vazia"
             (conjunto=? (remove-duplicatas '() '()) '()))
  
  (test-true "várias duplicatas"
             (conjunto=? (remove-duplicatas '(1 2 3 2 3 5) '()) '(1 2 3 5)))
  
  (test-true "apenas um elemento"
             (conjunto=? (lista->conjunto   '(5 5 5 5 5 5) '()) '(5)))
  
  (test-true "mais repetições"
             (conjunto=? (lista->conjunto '(1 2 3 1 2 3 1 2 3 1 2 3) '()) '(1 2 3))))

  
;; --- Questão 13 ----------------------------

;; Agora vamos implementar as operações de conjuntos implementados com listas.

;; Escreva a função uniao tal que
;; (uniao c1 c2) retorna um conjunto contendo os elementos de c1 e c2, sem duplicações.

(define (uniao c1 c2 acc)
  (cond
    [(and (empty? c1) (empty? c2)) acc]
    [else 
     (if (and(empty? c1) (not(empty? c2))) (uniao c1 (rest c2) (append acc(cons (first c2) '())))
         (uniao (rest c1) (remove-todos (first c1) c2 '()) (append acc(cons (first c1) '())))
         )
    ]
  )
)



;; testes

(define-test-suite test-uniao
  (test-true "Vazio é elemento neutro 1"
             (conjunto=? (uniao '() '(1 2 3) '())  '(1 2 3)))
  
  (test-true "Vazio é elemento neutro 2"
             (conjunto=? (uniao '(4 5 6) '() '())  '(4 5 6)))
  
  (test-true "União de vazios"
             (conjunto=? (uniao '() '() '())  '()))
  
  (test-true "Sem elementos em comum"
             (conjunto=? (uniao '(1 2 3) '(4 5 6) '())  '(1 2 3 4 5 6)))
  
  (test-true "Com elementos em comum"
             (conjunto=? (uniao '(1 4 5) '(4 5 6) '())  '(1 4 5 6))))


;; --- Questão 14 -----------------------

;; Escreva uma função interseccao tal que
;; (interseccao c1 c2) retorna um conjunto contendo os elementos que ocorrem
;; em ambos c1 e c2
(define (interseccao c1 c2 acc)
  (cond
    [(or (empty? c1) (empty? c2)) acc] 
    [else 
      (if (pertence? (first c1) c2 #f)
        (interseccao (rest c1) c2 (append acc(cons (first c1) '())))
        (interseccao (rest c1) c2 acc)
      )
    ]
  )
)

(define-test-suite test-interseccao
  (test-equal? "Conjuntos vazios"        (interseccao '()      '() '())      '())
  (test-equal? "Intersecção com vazio 1" (interseccao '(1 2 3) '() '())      '())
  (test-equal? "Intersecção com vazio 2" (interseccao '()      '(11 22) '()) '())
  (test-equal? "Sem elementos comuns"    (interseccao '(1 2 3) '(11 22) '()) '())

  (test-true "Um elemento em comum"
             (conjunto=? (interseccao '(1 2 3) '(11 1 121) '())  '(1)))

  (test-true "Vários elementos em comum"
             (conjunto=? (interseccao '(1 3 5 7 9 11)  '(11 3 1 13 17) '()) '(1 3 11)))

  (test-true "Mesmo conjunto"
             (conjunto=? (interseccao '(1 2 3 4 5) '(5 4 3 2 1) '())'(1 2 3 4 5))))


;; --- Questão 15 -----------------------

;; Escreva uma função diferenca tal que
;; (diferenca c1 c2) retorna um conjunto que tem todos os elementos de c1 que
;; não pertencem a c2. Por exemplo, (diferenca '(1 3 5 7) '(3 7)) deve retornar
;; '(1 5) (não necessariamente nesta ordem).


;; APRESENTA UM ERRO

(define (diferenca c1 c2 acc)
(cond
  [(empty? c1) acc]
  [(and (not(empty? c1)) (empty? c2)) c1]
  [(and (not(empty? c2)) (empty? c1)) c2]
  [else 
    (if (pertence? (first c1) c2 #f)
      (diferenca (rest c1) c2 acc)
      (diferenca (rest c1) c2 (append acc(cons (first c1) '())))
    )
  ]
  
)
)

;; Para esta função, escreva também um conjunto de testes, e adicione a suite de 
;; testes criados à execução de todos os testes, abaixo. Você pode escrever os
;; testes antes ou depois de implementar a função.

(define-test-suite test-diferenca
  (test-equal? "Conjuntos vazios"        (diferenca '()      '() '())      '())
  (test-equal? "Diferença com vazio 2" (diferenca '(1 2 3) '() '())      '(1 2 3))
  ;;(test-equal? "Diferença com vazio 1" (diferenca '()      '(11 22) '()) '(11 22)) apresenta erro quando c1 começa vazio
  
  (test-equal? "Um elemento que não é comum"
    (diferenca '(1 2 3 4) '(1 2 3) '())   '(4))
    
    (test-equal? "Vários elementos que não são comuns" (diferenca '(1 3 5 7 9 11) '(11 3 1 13 17) '())
    '(5 7 9))


  )

;; --- Executa todos os testes ---------
(run-tests
 (test-suite "todos os testes"
             
             testes-soma-lista
             testes-mult-lista
             testes-max-lista
             testes-quadrado-lista
             testes-filtra-par
             test-remove-primeiro
             test-remove-todos
             test-pertence?
             test-combine
             test-conjunto=?
             test-remove-duplicatas
             test-uniao
             test-interseccao
             test-diferenca
             
             ))
