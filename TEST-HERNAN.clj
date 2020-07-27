(require '[clojure.test :refer [is deftest run-tests]])

(load-file "tlc-lisp.clj")

;************************** INICIO CONTROLAR-ARIDAD ***************************
(deftest test-controlar-aridad
  (is (= '(*error* too-few-args) (controlar-aridad '(a b c) 4)))
  (is (= 4 (controlar-aridad '(a b c d) 4)))
  (is (= '(*error* too-many-args) (controlar-aridad '(a b c d e) 4)))
)
;************************** FIN CONTROLAR-ARIDAD ******************************
;******************************************************************************
;************************** INICIO BUSCAR *************************************
(deftest test-buscar
    (is (= nil (buscar nil '(+ add - sub))))
    (is (= 'sub (buscar '- '(+ add - sub))))
    (is (= '(lambda (x) (+ x x)) (buscar 'doble '(+ add r 4 doble (lambda (x) (+ x x))))))
    (is (= '(*error* unbound-symbol doble) (buscar 'doble '(+ add - sub))))
)
;************************** INICIO BUSCAR *************************************
;******************************************************************************
;************************** INICIO REVISAR-LAE ********************************
(deftest test-revisar-lae
    (is (= nil (revisar-lae '(1 add first))))
    (is (= '(*error* too-many-args) (revisar-lae '(1 add (*error* too-many-args) first))))
)
;************************** FIN REVISAR-LAE ***********************************
;******************************************************************************
;************************** INICIO REVISAR-F **********************************
(deftest test-revisar-f
    (is (= 'nil (revisar-f 'doble)))
    (is (= 'nil (revisar-f '(doble triple cuadruple))))
    (is (= '(*error* too-few-args) (revisar-f '(*error* too-few-args))))
)
;************************** INICIO REVISAR-F **********************************
;******************************************************************************
;************************** INICIO ACTUALIZAR-AMB *****************************
(deftest test-actualizar-amb
    (is (= '(+ add - sub x 1) (actualizar-amb '(+ add - sub) 'x 1)))
    (is (= '(+ add - sub x nil) (actualizar-amb '(+ add - sub) 'x '())))
    (is (= '(+ add - sub x 3 y 2) (actualizar-amb '(+ add - sub x 1 y 2) 'x 3)))
    (is (= '(+ add doble (lambda (x) (+ x x))) (actualizar-amb  '(+ add) 'doble '(lambda (x) (+ x x)))))
    (is (= '(+ add - sub) (actualizar-amb '(+ add - sub) 'x '(error 1 2 3))))
)
;************************** FIN ACTUALIZAR-AMB ********************************
;******************************************************************************
;************************** INICIO IMPRIMIR ***********************************
;prueba la salida que devuelve la funcion, si imprime con salto de linea no se puede testiar esto, si la salida
(deftest test-imprimir
    (is (= '\space (imprimir \space)))
    (is (= 'a (imprimir 'a)))
    (is (= 5 (imprimir 5)))
    (is (= "hola" (imprimir "hola")))
    (is (= '(*error* hola "mundo") (imprimir '(*error* hola "mundo"))))
    (is (= '(hola "mundo") (imprimir '(hola "mundo"))))
)
;************************** FIN IMPRIMIR **************************************
;******************************************************************************
;************************** INICIO IGUAL **************************************
(deftest test-igual
    (is (= true (igual? 1 1)))
    (is (= false (igual? 1 2)))
    (is (= false (igual? 1 "aaa")))
    (is (= true (igual? nil nil)))
    (is (= true (igual? nil 'NIL)))
    (is (= true (igual? nil "NIL")))
    (is (= true (igual? nil ())))
    (is (= true (igual? () 'NIL)))
    (is (= true (igual? 'a 'a)))
)
;*******************************************************************************
;************************** INICIO EVALUAR-SECUENCIA-EN-COND *************************************
(deftest test-evaluar-secuencia-en-cond
    (is (= '(2 (setq setq y 2)) (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil)))
    (is (= '(3 (setq setq y 2 z 3)) (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil)))
)
;************************** FIN EVALUAR-SECUENCIA-EN-COND ****************************************
;*******************************************************************************
;************************** INICIO EVALUAR-COND *************************************
(deftest test-evaluar-cond
    (is (= '(nil (+ add)) (evaluar-cond nil '(+ add) nil)))
    (is (= '(nil (equal equal first first)) (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil)))
    (is (= '(2 (equal equal setq setq y 2)) (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil)))
    (is (= '(3 (equal equal setq setq y 2 z 3)) (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil)))
)
;************************** FIN EVALUAR-COND ****************************************
;*******************************************************************************
;************************** INICIO EVALUAR ****************************************
(deftest test-evaluar
    (is (= '(add (+ add)) (evaluar '+ '(+ add) nil)))
    (is (= '("lambda" (+ add)) (evaluar "lambda" '(+ add) nil)))
    (is (= '(nil (+ add)) (evaluar '() '(+ add) nil)))
    (is (= '((*error* unbound-symbol a) (+ add)) (evaluar '(*error* unbound-symbol a) '(+ add) nil)))
    ;*******DE*************
    (is (= '(doble (+ add doble (lambda (x) (+ x x)))) (evaluar '(de doble (x) (+ x x)) '(+ add) nil)))
    (is (= '((*error* list expected nil) (+ add)) (evaluar '(de doble) '(+ add) nil)))
    (is (= '((*error* cannot-set nil) (+ add)) (evaluar '(de nil (x)) '(+ add) nil)))
    (is (= '((*error* symbol expected "doble") (+ add)) (evaluar '(de "doble" (x)) '(+ add) nil)))
    (is (= '((*error* list expected "body") (+ add)) (evaluar '(de doble "body") '(+ add) nil)))
    ;*******EXIT***********
    (is (= '(nil nil) (evaluar '(exit) '(+ add) nil)))
    (is (= '((*error* too-many-args) (+ add)) (evaluar '(exit 1) '(+ add) nil)))
    ;*******LAMBDA*********
    (is (= '((*error* list expected nil) (+ add)) (evaluar '(lambda) '(+ add) nil)))
    (is (= '((*error* list expected "algo") (+ add)) (evaluar '(lambda "algo") '(+ add) nil)))
    (is (= '((lambda (x) (+ x x )) (+ add)) (evaluar '(lambda (x) (+ x x )) '(+ add) nil)))
    ;*******OR************
    (is (= '(nil (+ add)) (evaluar '(or) '(+ add) nil)))
    (is (= '(10 (+ add)) (evaluar '(or 10) '(+ add) nil)))
    (is (= '(10 (+ add)) (evaluar '(or 10 nil 20) '(+ add) nil)))
    (is (= '(20 (+ add)) (evaluar '(or nil 20) '(+ add) nil)))
    (is (= '(20 (+ add)) (evaluar '(or () 20) '(+ add) nil)))
    (is (= '(20 (+ add)) (evaluar '(or () () 20) '(+ add) nil)))
    (is (= '(t (+ add equal equal)) (evaluar '(or (equal 1 1) () 20) '(+ add equal equal) nil)))
    (is (= '(t (+ add gt gt)) (evaluar '(or nil (gt 10 5) 20) '(+ add gt gt) nil)))
    (is (= '((*error* unbound-symbol bla) (+ add gt gt)) (evaluar '(or nil (gt 1 5) bla) '(+ add gt gt) nil)))
    ;*******QUOTE*********
    (is (= '(nil (+ add)) (evaluar '(quote) '(+ add) nil)))
    (is (= '(nil (+ add)) (evaluar '(quote ()) '(+ add) nil)))
    (is (= '(a (+ add)) (evaluar '(quote a) '(+ add) nil)))
    (is (= '((lambda (x) (+ x x )) (+ add)) (evaluar '(quote (lambda (x) (+ x x ))) '(+ add) nil)))
    ;*******SETQ*********
    (is (= '((*error* expected nil) (+ add)) (evaluar '(setq) '(+ add) nil)))
    (is (= '((*error* expected nil) (+ add)) (evaluar '(setq x) '(+ add) nil)))
    (is (= '((*error* cannot-set nil) (+ add)) (evaluar '(setq nil a) '(+ add) nil)))
    (is (= '((*error* symbol expected "algo") (+ add)) (evaluar '(setq "algo" 1) '(+ add) nil)))
    (is (= '(1 (+ add x 1)) (evaluar '(setq x 1) '(+ add) nil)))
    (is (= '(doble (+ add doble (lambda (x) (+ x x)) x doble)) (evaluar '(setq x (de doble (x) (+ x x))) '(+ add) nil)))
    (is (= '(1 (+ add doble (lambda (x) (+ x x)) x doble y 1)) (evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add) nil)))
    ;********COND**********
    (is (= '(nil (equal equal first first)) (evaluar '(cond nil) '(equal equal first first) nil)))
    (is (= '(nil (equal equal first first)) (evaluar '(cond ((equal 'a 'b) (setq x 1))) '(equal equal first first) nil)))
    (is (= '(2 (equal equal setq setq y 2)) (evaluar '(cond ((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil)))
    (is (= '(3 (equal equal setq setq y 2 z 3)) (evaluar '(cond ((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil)))
    (is (= '(t (equal equal first first)) (evaluar '(cond ((equal 'a 'a))) '(equal equal first first) nil)))
    (is (= '(nil (equal equal first first)) (evaluar '(cond ((equal 'a 'b))) '(equal equal first first) nil)))
    ;********IF**********
    (is (= '((*error* list expected nil) (equal equal first first)) (evaluar '(if) '(equal equal first first) nil)))
    (is (= '(2 (equal equal first first)) (evaluar '(if (equal 'a 'b) 1 2) '(equal equal first first) nil)))
    (is (= '(1 (equal equal first first)) (evaluar '(if (equal 'a 'a) 1 2) '(equal equal first first) nil)))
    (is (= '(1 (equal equal first first x 1)) (evaluar '(if (equal 'a 'a) (setq x 1) 2) '(equal equal first first) nil)))
    ;*******FUNCION********
    (is (= '(12 (+ add)) (evaluar '(+ x x) '(+ add) '(x 6))))
    (is (= '(12 (+ add)) (evaluar '((lambda (x) (+ x x ))6) '(+ add) '(x 6))))
    (is (= '(6 (+ add doble (lambda (x) (+ x x))) (evaluar '(doble 3) '(+ add doble (lambda (x) (+ x x))) nil))))
    (is (= '(8 (+ add r 4 doble (lambda (x) (+ x x))) (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil))))
)
;************************** FIN EVAVUAR ****************************************
;*******************************************************************************
;************************** INICIO APLICAR *************************************
(deftest test-aplicar
    (is (= '((*error* too-few-args) (cons cons)) (aplicar '(*error* too-few-args) '(a (b)) '(cons cons) nil)))
    (is (= '((*error* too-many-args) (cons cons)) (aplicar 'cons '(1 add (*error* too-many-args) first) '(cons cons) nil)))
    ; ********ENV********
    (is (= '((+ add r 5) (+ add r 5)) (aplicar 'env '() '(+ add r 5) nil)))
    ; ********ADD********
    (is (= '(9 (+ add r 5)) (aplicar 'add '(4 5) '(+ add r 5) nil)))
    ; ********SUB********
    (is (= '(-1 (+ add r 5)) (aplicar 'sub '(4 5) '(+ add r 5) nil)))
    (is (= '(1 (+ add r 5)) (aplicar 'sub '(6 5) '(+ add r 5) nil)))
    (is (= '((*error* number-expected) (+ add r 5)) (aplicar 'sub '(a 5) '(+ add r 5) nil)))
    ; ********REVERSE********
    (is (= '((d c b a) (+ add r 5)) (aplicar 'reverse '((a b c d)) '(+ add r 5) nil)))
    (is (= '((*error* list expected "1") (+ add r 5)) (aplicar 'reverse "1" '(+ add r 5) nil)))
    (is (= '((*error* too-many-args) (+ add r 5)) (aplicar 'reverse '((a) (b)) '(+ add r 5) nil)))
    (is (= '((*error* too-few-args) (+ add r 5)) (aplicar 'reverse '() '(+ add r 5) nil)))
    (is (= '(nil (+ add r 5)) (aplicar 'reverse '(nil) '(+ add r 5) nil)))
    (is (= '(nil (+ add r 5)) (aplicar 'reverse '("NIL") '(+ add r 5) nil)))
    ; ********FIRST********
    (is (= '(4 (+ add r 5)) (aplicar 'first '((4 5)) '(+ add r 5) nil)))
    ; ********APPEND********
    (is (= '((a b c d) (+ add r 5)) (aplicar 'append '((a b) (c d)) '(+ add r 5) nil)))
    ; ********CONS********
    (is (= '((a b) (cons cons)) (aplicar 'cons '(a (b)) '(cons cons) nil)))
    (is (= '(((a) b) (cons cons)) (aplicar 'cons '((a) (b)) '(cons cons) nil)))
    (is (= '(((a)) (cons cons)) (aplicar 'cons '((a) nil) '(cons cons) nil)))
    ; ********GT********
    (is (= '(nil (+ add r 5)) (aplicar 'gt '(4 5) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'gt '(44 5) '(+ add r 5) nil)))
    ; ********LT********
    (is (= '(t (+ add r 5)) (aplicar 'lt '(4 5) '(+ add r 5) nil)))
    (is (= '(nil (+ add r 5)) (aplicar 'lt '(44 5) '(+ add r 5) nil)))
    ; ********GE********
    (is (= '(nil (+ add r 5)) (aplicar 'ge '(4 5) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'ge '(5 5) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'ge '(6 5) '(+ add r 5) nil)))
    ; ********NOT********
    (is (= '(nil (+ add r 5)) (aplicar 'not '(t) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'not '(nil) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'not '(()) '(+ add r 5) nil)))
    (is (= '((*error* unbound-symbol a)(+ add r 5)) (aplicar 'not '(a) '(+ add r 5) nil)))
    ; ********NULL********
    (is (= '(nil (+ add r 5)) (aplicar 'null '(t) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'null '(nil) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'null '(()) '(+ add r 5) nil)))
    ; ********EQUAL********
    (is (= '(nil (+ add r 5)) (aplicar 'equal '(4 5) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'equal '(5 5) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'equal '('a 'a) '(+ add r 5) nil)))
    (is (= '(t (+ add r 5)) (aplicar 'equal '(('a 'b 'c) ('a 'b 'c)) '(+ add r 5) nil)))
   ; ********LENGTH********
   (is (= '(2 (+ add r 5)) (aplicar 'length '((4 5)) '(+ add r 5) nil)))
   (is (= '(0 (+ add r 5)) (aplicar 'length '(()) '(+ add r 5) nil)))
   (is (= '((*error* arg-wrong-type 2) (+ add r 5)) (aplicar 'length '(2) '(+ add r 5) nil)))
    ; ********LIST********
    (is (= '((nil 4 5 nil) (+ add r 5)) (aplicar 'list '(() 4 5 nil) '(+ add r 5) nil)))
    ; ********REST********
    (is (= '((5) (+ add r 5)) (aplicar 'rest '((4 5)) '(+ add r 5) nil)))
    (is (= '(nil (+ add r 5)) (aplicar 'rest '(()) '(+ add r 5) nil)))
    (is (= '((5 nil) (+ add r 5)) (aplicar 'rest '((4 5 ())) '(+ add r 5) nil)))
    ; ********LAMBDA********
    (is (= '(8 (+ add r 4 doble (lambda (x) (+ x x)))) (aplicar '(lambda (x) (+ x x)) '(4) '(+ add r 4 doble (lambda (x) (+ x x))) nil)))
)

(run-tests)
  