(require '[clojure.test :refer [is deftest run-tests]])

(load-file "tlc-lisp.clj")

;#########################
;TEST de Evaluar
;#########################
(deftest test-evaluar

    (is (= (evaluar 'nfsd '() '()) '((*error* unbound-symbol nfsd) ()) ))
    (is (= (evaluar '3 '(+ add) nil) '(3 (+ add)) ))
    (is (= (evaluar 'nil '(+ add nil nil) nil) '(nil (+ add nil nil)) ))
    (is (= (evaluar '( *error* 2 3 4) '(+ add) nil) '(( *error* 2 3 4) (+ add)) ))

    ;;DOC TESTS
    (is (= (evaluar '(setq r 3) '(+ add) nil) '(3 (+ add r 3)) ))
    (is (= (evaluar '(de doble (x) (+ x x)) '(+ add) nil) '(doble (+ add doble (lambda (x) (+ x x)))) ))
    (is (= (evaluar '(+ 2 3) '(+ add) nil) '(5 (+ add)) ))
    (is (= (evaluar '(+ 2 3) '(add add) nil) '((*error* unbound-symbol +) (add add)) ))
    (is (= (evaluar '(doble 3) '(+ add doble (lambda (x) (+ x x))) nil) '(6 (+ add doble (lambda (x) (+ x x)))) ))
    (is (= (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil) '(8 (+ add r 4 doble (lambda (x) (+ x x)))) ))
    (is (= (evaluar '((lambda (x) (+ x x)) 3) '(+ add) nil) '(6 (+ add)) ))
    ;;END DOC TESTS

    (is (= (evaluar '(if (gt 3 2) 3 5) '(gt gt) '()) '(3 (gt gt)) ))
    (is (= (evaluar '(if (gt 3 2) (+ 1 1) (+ 9 9)) '(+ add gt gt) '()) '(2 (+ add gt gt)) ))
    (is (= (evaluar '(if (gt 1 2) (+ 1 1) (+ 9 9)) '(+ add gt gt) '()) '(18 (+ add gt gt)) ))
    (is (= (evaluar '(if (+ 1 2) (+ 1 1) (+ 9 9)) '(+ add gt gt) '()) '(2 (+ add gt gt)) ))
    (is (= (evaluar '(if (+ 1 2) 1 2) '(+ add gt gt) '()) '(1 (+ add gt gt)) ))
    (is (= (evaluar '(if 1 "true" "false") '(+ add gt gt) '()) '("true" (+ add gt gt)) ))
    (is (= (evaluar '(if "algo" "true" "false") '(+ add gt gt) '()) '("true" (+ add gt gt)) ))
    (is (= (evaluar '(if dasdsa 56 (+ 9 9)) '(+ add gt gt) '()) '((*error* unbound-symbol dasdsa) (+ add gt gt)) ))
    (is (= (evaluar '(if (gt 1 2) (+ 1 1) (+ 9 9) (+ 9 9) (+ 9 9) (+ 9 9) "soy el útlimo") '(+ add gt gt) '()) '("soy el útlimo"(+ add gt gt)) ))
    
    (is (= (evaluar '(or nil nil (gt 9 5) ) '(nil nil + add gt gt) '()) '(t (nil nil + add gt gt) ) ))
    (is (= (evaluar '(or nil nil (gt 1 5) ) '(nil nil + add gt gt) '()) '(nil (nil nil + add gt gt) ) ))
    (is (= (evaluar '(or nil nil (gt 1 5) dasbd ) '(nil nil + add gt gt) '()) '((*error* unbound-symbol dasbd) (nil nil + add gt gt) ) ))
    (is (= (evaluar '(or nil nil (gt 10 5) dasbd ) '(nil nil + add gt gt) '()) '(t (nil nil + add gt gt) ) ))


    (is (= (evaluar '(setq dada nil) '(not not nil nil) '() ) '(nil (not not nil nil dada nil)) ))
    (is (= (evaluar '(setq dada 5) '(not not nil nil) '() ) '(5 (not not nil nil dada 5)) ))
    (is (= (evaluar '(setq x (de suma2 (x) (+ x x))) '(+ add) nil) '(suma2 (+ add suma2 (lambda (x) (+ x x)) x suma2 )) ))
    (is (= (evaluar '(setq x 1 y 2 z 3) '(+ add) nil) '(3 (+ add x 1 y 2 z 3)) ))

    ; (is (= (evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add) nil) '() ))
    ; (is (= '(1 (+ add doble (lambda (x) (+ x x)) x doble y 1)) (evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add) nil)))
    ; (evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add setq setq) nil)
    ; (evaluar '(setq y 1 x (de doble (x) (+ x x))) '(+ add setq setq) nil)
    ; (evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add setq setq) nil)

    (is (= (evaluar '(cond ((gt 1 5) 3) ( t (+ 89 9) )) '(gt gt + add) '()) '(98 (gt gt + add)) ))

    (is (= (evaluar '(de doble (x) (+ (setq ro 9) ro)) '(+ add) nil) '(doble (+ add doble (lambda (x) (+ x x)))) ))

    (evaluar '(de doble (x) (+ (setq ro 9) ro)) '(+ add) nil)

    (is (= (evaluar '((lambda (x) (+ ro x)) 3) '(+ add ro 20) '() ) '(23 (+ add ro 20)) ))

    ; exit
    ; load
    ; quote
)


;#########################
;TEST de Aplicar mediante Evaluar
;#########################
(deftest test-aplicar-evaluar
    (is (= (evaluar '(equal dasdas dasdasd) '(equal equal) '()) '((*error* unbound-symbol dasdas) (equal equal)) ))
    (is (= (aplicar 'equal '(das das) '(ambiente) '()) '(t (ambiente)) ))

    (is (= (evaluar '(add 3) '(add add) '()) '((*error* too-few-args) (add add)) ))
    (is (= (evaluar '(add 3 ro) '(add add ro 20) '()) '(23 (add add ro 20)) ))
    (is (= (evaluar '(add 3 ro) '(add add) '()) '((*error* unbound-symbol ro) (add add)) ))
    (is (= (evaluar '(add 3 "ro") '(add add) '()) '((*error* number-expected)(add add)) ))

    (is (= (evaluar '(eval (add 5 8)) '(eval eval add add) '()) '(13 (eval eval add add)) ))
    (is (= (evaluar '(eval) '(eval eval add add) '()) '((*error* too-few-args)(eval eval add add)) ))

    (is (= (evaluar '(list 2 3 4 5) '(list list) '()) '((2 3 4 5)(list list)) ))
    (is (= (evaluar '(list 2) '(list list) '()) '((2) (list list)) ))
    (is (= (evaluar '(list nil) '(list list nil nil) '()) '((nil) (list list nil nil)) ))
    (is (= (evaluar '(list) '(list list nil nil) '()) '(nil (list list nil nil)) ))

    (is (= (evaluar '(gt 4 5) '(gt gt) '() ) '(nil (gt gt)) ))
    (is (= (evaluar '(gt 7 5) '(gt gt) '() ) '(t (gt gt)) ))
    (is (= (evaluar '(gt (add 5 9) 5) '(gt gt add add) '(t (gt gt add add)) ) ))
    (is (= (evaluar '(gt "aaa" "zzz") '(gt gt) '() ) '((*error* number-expected "aaa")(gt gt)) ))

    (is (= (evaluar '(lt 4 5) '(lt lt) '() ) '(t (lt lt)) ))
    (is (= (evaluar '(lt 7 5) '(lt lt) '() ) '(nil (lt lt)) ))
    (is (= (evaluar '(lt (add 5 9) 5) '(lt lt add add) '() ) '(nil (lt lt add add)) ))
    (is (= (evaluar '(lt "aaa" "zzz") '(lt lt) '() ) '((*error* number-expected "aaa") (lt lt)) ))
    (is (= (evaluar '(lt (setq toto 10) 50) '(lt lt) '() ) '(t (lt lt)) ))

    (is (= (evaluar '(not 4 5) '(not not) '() ) '((*error* too-many-args) (not not)) ))
    (is (= (evaluar '(not) '(not not) '() ) '((*error* too-few-args)(not not)) ))
    (is (= (evaluar '(not 5) '(not not) '() ) '(nil (not not)) ))
    (is (= (evaluar '(not nil) '(not not nil nil) '() ) '(t (not not nil nil)) ))
    (is (= (evaluar '(not t) '(not not t t) '() ) '(nil (not not t t)) ))
    (is (= (evaluar '(not "string") '(not not) '() ) '(nil (not not)) ))
    (is (= (evaluar '(not (setq da 9)) '(not not setq setq) '() ) '(nil (not not setq setq)) ))

    (is (= (evaluar '(first '(3 5)) '(first first) '()) '(3 (first first)) ))
    (is (= (evaluar '(first '(3)) '(first first) '()) '(3 (first first)) ))
    (is (= (evaluar '(first '()) '(first first) '()) '(nil (first first)) ))
    (is (= (evaluar '(first '(hola 5)) '(first first) '()) '(hola (first first)) ))
    (is (= (evaluar '(first '(3 5)) '(first first) '()) '(3 (first first)) ))

    (is (= (evaluar '(null '()) '(null null nil nil) '() ) '(t (null null nil nil)) ))
    (is (= (evaluar '(null ()) '(null null nil nil) '() ) '(t (null null nil nil)) ))
    (is (= (evaluar '(null a) '(null null nil nil) '() ) '((*error* unbound-symbol a)(null null nil nil)) ))
    (is (= (evaluar '(null (setq a NIL)) '(null null nil nil) '() ) '(t (null null nil nil)) ))
    (is (= (evaluar '(null "a") '(null null nil nil) '() ) '(nil (null null nil nil)) ))

    (is (= (evaluar '(prin3 '()) '(prin3 prin3 nil nil) '() ) '(nil (prin3 prin3 nil nil)) ))
    (is (= (evaluar '(prin3 ()) '(prin3 prin3 nil nil) '() ) '(nil (prin3 prin3 nil nil)) ))
    (is (= (evaluar '(prin3 a) '(prin3 prin3 nil nil) '() ) '((*error* unbound-symbol a) (prin3 prin3 nil nil)) ))
    (is (= (evaluar '(prin3 (setq a NIL)) '(prin3 prin3 nil nil) '() ) '(nil (prin3 prin3 nil nil)) ))
    (is (= (evaluar '(prin3 "a") '(prin3 prin3 nil nil) '() ) '("a" (prin3 prin3 nil nil)) ))
    (is (= (evaluar '(prin3 'a) '(prin3 prin3 nil nil) '() ) '(a (prin3 prin3 nil nil)) ))

    (is (= (evaluar '(rest '(1 2 3 4 5)) '(rest rest nil nil) '() ) '((2 3 4 5) (rest rest nil nil)) ))
    (is (= (evaluar '(rest '(1 2 3 4 5) 3) '(rest rest nil nil) '() ) '((4 5) (rest rest nil nil)) ))
    (is (= (evaluar '(rest) '(rest rest nil nil) '() ) '((*error* too-few-args) (rest rest nil nil)) ))
    (is (= (evaluar '(rest '(5 8)) '(rest rest nil nil) '() ) '((8) (rest rest nil nil)) ))
    (is (= (evaluar '(rest nil) '(rest rest nil nil) '() ) '(nil (rest rest nil nil)) ))
    (is (= (aplicar 'rest '(()) '(rest rest nil nil add add) '() ) '(nil (rest rest nil nil add add)) ))

    (is (= (evaluar '(env) '(rest rest nil nil add add env env) '(hola hola) ) '((rest rest nil nil add add env env) (rest rest nil nil add add env env)) ))
    (is (= (evaluar '(env 1 3) '(env env) '() ) '((*error* too-many-args) (env env)) ))

    (is (= (evaluar '(read '()) '(read read nil nil) '() ) '((*error* too-many-args) (read read nil nil)) ))
    (is (= (evaluar '(read 1 2) '(read read nil nil) '() ) '((*error* too-many-args) (read read nil nil)) ))

)


;#########################
;TEST de Aplicar
;#########################
(deftest test-aplicar

    ;DOC TEST
    (is (= (aplicar 'cons '(a (b)) '(cons cons) nil) '((a b) (cons cons)) ))
    (is (= (aplicar 'add '(4 5) '(+ add r 5) nil) '(9 (+ add r 5)) ))
    (is (= (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil) '(8 (+ add r 4 doble (lambda (x) (+ x x)))) ))
    (is (= (aplicar '(lambda (x) (+ x x)) '(4) '(+ add r 4 doble (lambda (x) (+ x x))) nil) '(8 (+ add r 4 doble (lambda (x) (+ x x)))) ))
    ;END DOC TEST

    (is (= (aplicar 'gtasdass '(3 2) '() '()) '(nil ()) ))

    (is (= (aplicar 'gt '(1 2) '() '()) '(nil ()) ))
    (is (= (aplicar 'gt '(7 2) '() '()) '(t ()) ))

    (is (= (aplicar 'first '((3 2)) '(first first) '()) '(3 (first first)) ))
    (is (= (aplicar 'first '(nil) '() '()) '(nil ()) ))

    (is (= (aplicar 'eval '(add 5 5) '(amb amb add add) '()) '(10 (amb amb add add)) ))
    (is (= (aplicar 'eval '(5) '(amb amb add add) '()) '(5 (amb amb add add)) ))
    (is (= (aplicar 'eval '() '(amb amb add add) '()) '((*error* too-few-args)(amb amb add add)) ))
    ; (is (= (aplicar 'eval '(lambda) '(amb amb add add) '()) '((*error* too-few-args)(amb amb add add)) ))

    (is (= (aplicar 'add '(3) '() '()) '((*error* too-few-args) ()) ))
    (is (= (aplicar 'add '(3 5) '() '()) '(8 ()) ))
    (is (= (aplicar 'add '(3 5 6 8 9) '() '()) '(31 ()) ))
    (is (= (aplicar 'add '(3 5 6 8 "ro") '(ro 10) '(ro 10)) '((*error* number-expected) (ro 10)) ))

    (is (= (aplicar 'sub '(3 5 6 8 "ro") '(ro 10) '(ro 10)) '((*error* number-expected) (ro 10)) ))
    (is (= (aplicar 'sub '(3 5 6 8 9) '(ambiente amb) '(ambiente amb)) '(-25 (ambiente amb)) ))
    (is (= (aplicar 'sub '(3 3) '(ambiente amb) '(ambiente amb)) '(0 (ambiente amb)) ))
    (is (= (aplicar 'sub '(1) '(ambiente amb) '(ambiente amb)) '(-1 (ambiente amb)) ))
    (is (= (aplicar 'sub '() '(ambiente amb) '(ambiente amb))'((*error* too-few-args)(ambiente amb)) ))
    
    (is (= (aplicar 'list '(2 3 4 5) '(list list) '()) '((2 3 4 5) (list list)) ))
    (is (= (aplicar 'list '(2) '(list list) '()) '((2) (list list)) ))
    (is (= (aplicar 'list '(nil) '(list list nil nil) '()) '((nil) (list list nil nil)) ))
    (is (= (aplicar 'list '() '(list list nil nil) '()) '(nil (list list nil nil)) ))
    (is (= (aplicar 'list '(2 3 (add 5 9) 5) '(list list add add) '()) '((2 3 (add 5 9) 5) (list list add add)) ))

    (is (= (aplicar 'terpri '()'(amb amb) '()) '(nil (amb amb)) ))

    (is (= (aplicar 'env '() '(rest rest nil nil add add env env) '(hola hola) ) '((rest rest nil nil add add env env) (rest rest nil nil add add env env)) ))

)


;#########################
;Evaluar controlar-aridad 
;#########################
(deftest test-controlar-aridad
    (is (= (controlar-aridad '(a b c) 4) '(*error* too-few-args) ))
    (is (= (controlar-aridad '(a b c d) 4) 4 ))
    (is (= (controlar-aridad '(a b c d e) 4) '(*error* too-many-args) ))
)


;#########################
;Evaluar igual?
;#########################
(deftest test-igual?
    (is (= (igual? nil 'NIL) true ))
    (is (= (igual? nil "NIL") true ))
    (is (= (igual? nil ()) true ))
    (is (= (igual? () 'NIL) true ))
)



;#########################
;Evaluar imprimir
;#########################
(deftest test-imprimir
    (is (= (imprimir "hola") "hola" ))
    (is (= (imprimir 5) 5 ))
    (is (= (imprimir 'a) 'a))
    (is (= (imprimir \space) \space))
    (is (= (imprimir '(hola "mundo")) '(hola "mundo") ))
    (is (= (imprimir '(*error* hola "mundo")) '(*error* hola "mundo") ))
)


;#########################
;Evaluar actualizar-amb
;#########################
(deftest test-actualizar-amb
    (is (= (actualizar-amb '(+ add - sub) 'x 1) '(+ add - sub x 1) ))
    (is (= (actualizar-amb '(+ add - sub x 1 y 2) 'x 3) '(+ add - sub x 3 y 2) ))
)



;#########################
;Evaluar revisar-f
;#########################
(deftest test-revisar-f
    (is (= (revisar-f 'doble) 'nil ))
    (is (= (revisar-f '(*error* too-few-args)) '(*error* too-few-args) ))
)


;#########################
;Evaluar revisar-lae
;#########################
(deftest test-revisar-lae
    (is (= (revisar-lae '(1 add first)) 'nil ))
    (is (= (revisar-lae '(1 add (*error* too-many-args) first)) '(*error* too-many-args) ))
)


;#########################
;Evaluar buscar
;#########################
(deftest test-buscar
    (is (= (buscar '- '(+ add - sub)) 'sub ))
    (is (= (buscar 'doble '(+ add - sub)) '(*error* unbound-symbol doble) ))
)


;#########################
;Evaluar-secuencia-en-cond
;#########################
(deftest test-secuencia-en-cond
    (is (= (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil) '(2 (setq setq y 2)) ))
    (is (= (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil) '(3 (setq setq y 2 z 3)) ))
)


;#########################
;Evaluar evaluar-cond 
;#########################
(deftest test-evaluar-cond
    (is (= (evaluar-cond nil '(equal equal setq setq) nil) '(nil (equal equal setq setq)) ))
    (is (= (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) '(nil (equal equal first first)) ))
    (is (= (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil) '(2 (equal equal setq setq y 2)) ))
    (is (= (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil) '(3 (equal equal setq setq y 2 z 3)) ))
)

(run-tests)