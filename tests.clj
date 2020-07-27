(require '[clojure.test :refer [is deftest run-tests]])

; (load-file "HERNAN-TLC-LISP.clj")
(load-file "tlc-lisp.clj")

;#########################
;TEST de Evaluar
;#########################
(deftest test-evaluar
    (is (= (evaluar 'nfsd '() '()) '((*error* unbound-symbol nfsd) ()) ))
    (is (= (evaluar '3 '(+ add) nil) '(3 (+ add)) ))
    (is (= (evaluar 'nil '(+ add nil nil) nil) '(nil (+ add nil nil)) ))
    (is (= (evaluar '( *error* 2 3 4) '(+ add) nil) '(( *error* 2 3 4) (+ add)) ))
    (is (= (evaluar '(setq r 3) '(+ add) nil) '(3 (+ add r 3)) ))
    (is (= (evaluar '(de doble (x) (+ x x)) '(+ add) nil) '(doble (+ add doble (lambda (x) (+ x x)))) ))
    (is (= (evaluar '(+ 2 3) '(+ add) nil) '(5 (+ add)) ))
    (is (= (evaluar '(+ 2 3) '(add add) nil) '((*error* unbound-symbol +) (add add)) ))
    (is (= (evaluar '(doble 3) '(+ add doble (lambda (x) (+ x x))) nil) '(6 (+ add doble (lambda (x) (+ x x)))) ))
    (is (= (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil) '(8 (+ add r 4 doble (lambda (x) (+ x x)))) ))
    (is (= (evaluar '((lambda (x) (+ x x)) 3) '(+ add) nil) '(6 (+ add)) ))
    (is (= (evaluar '(first '(3 5)) '(first first) '()) '(3 (first first)) ))

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

    (is (= (evaluar '(equal dasdas dasdasd) '(equal equal) '()) '((*error* unbound-symbol dasdas) (equal equal)) ))
    (is (= (aplicar 'equal '(das das) '(ambiente) '()) '(t (ambiente)) ))
)

; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))


;#########################
;TEST de Aplicar
;#########################
(deftest test-aplicar
    (is (= (aplicar 'gt '(1 2) '() '()) '(nil ()) ))
    (is (= (aplicar 'gt '(7 2) '() '()) '(t ()) ))
    (is (= (aplicar 'gtasdass '(3 2) '() '()) '(nil ()) ))
    (is (= (aplicar 'first '((3 2)) '(first first) '()) '(3 (first first)) ))
    (is (= (aplicar 'first '(nil) '() '()) '(nil ()) ))
)
; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))
; (is (= () '() ))


; (second '(or))




;#########################
;Evaluar-secuencia-en-cond
;#########################
(is (= (evaluar-secuencia-en-cond '((setq y 2)) '(setq setq) nil) '(2 (setq setq y 2)) ))
(is (= (evaluar-secuencia-en-cond '((setq y 2) (setq z 3)) '(setq setq) nil) '(3 (setq setq y 2 z 3)) ))


;#########################
;Evaluar evaluar-cond 
;#########################
(is (= (evaluar-cond nil '(equal equal setq setq) nil) '(nil (equal equal setq setq)) ))
(is (= (evaluar-cond '(((equal 'a 'b) (setq x 1))) '(equal equal first first) nil) '(nil (equal equal first first)) ))
(is (= (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil) '(2 (equal equal setq setq y 2)) ))
(is (= (evaluar-cond '(((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil) '(3 (equal equal setq setq y 2 z 3)) ))
; (is (=  ' ))
; (is (=  ' ))
; (is (=  ' ))


; ;#########################
; ;cond 
; ;#########################
(is (= (evaluar '(cond ((gt 1 5) 3) ( t (+ 89 9) )) '(gt gt + add) '()) '(98 (gt gt + add)) ))



; ;#########################
; ;TEST de aplicar 
; ;#########################
; (aplicar 'add '(3) '() '())
; (evaluar '(add 3) '(add add) '())
; (aplicar 'add '(3 5) '() '())
; (aplicar 'add '(3 5 6 8 9) '() '())

; (evaluar '(add 3 ro) '(add add ro 20) '())
; (evaluar '(add 3 ro) '(add add) '())
; (evaluar '(add 3 "ro") '(add add) '())

; (aplicar 'add '(3 5 6 8 "ro") '(ro 10) '(ro 10))

; (aplicar 'sub '(3 5 6 8 9) '(ambiente) '(ambiente))
; (aplicar 'sub '(3 3) '(ambiente) '(ambiente))
; (aplicar 'sub '(1) '(ambiente) '(ambiente))
; (aplicar 'sub '() '(ambiente) '(ambiente))

; (evaluar '(sub 3 2) '(sub sub) '(amb))
; (evaluar '(sub 3) '(sub sub) '(amb))
; (evaluar '(sub) '(sub sub) '(amb))
; (aplicar 'sub '(3 5 6 8 "ro") '(ro 10) '(ro 10))
; (evaluar '(sub 3 "ro") '(sub sub) '())
; (evaluar '(sub 3 ro) '(sub sub ro 20) '())


;(aplicar 'terpri '()'(amb) '(amb))

; ;#########################
; ;TEST de eval
; ;#########################
; (aplicar 'eval '(5) '(amb amb + add) '(amb))

; (aplicar 'eval '(add 5 5) '(amb amb add add) '(amb))

; (aplicar 'eval '(5) '(amb amb add add) '(amb))

; (evaluar '(eval (add 5 8)) '(eval eval add add) '())

; (evaluar '(eval) '(eval eval add add) '())

; (aplicar 'eval '() '(amb amb add add) '(amb))



; ;#########################
; ;TEST de list
; ;#########################

; (evaluar '(list 2 3 4 5) '(list list) '())
; (evaluar '(list 2) '(list list) '())
; (evaluar '(list nil) '(list list nil nil) '())
; (evaluar '(list) '(list list nil nil) '())
; (evaluar '(list 2 3 (add 5 9) 5) '(list list add add) '())


; (aplicar 'list '(2 3 4 5) '(list list) '())
; (aplicar 'list '(2) '(list list) '())
; (aplicar 'list '(nil) '(list list nil nil) '())
; (aplicar 'list '() '(list list nil nil) '())
; (aplicar 'list '(2 3 (add 5 9) 5) '(list list add add) '())

; ;#########################
; ;TEST de gt
; ;#########################
; (evaluar '(gt 4 5) '(gt gt) '() )
; (evaluar '(gt 7 5) '(gt gt) '() )
; (evaluar '(gt (add 5 9) 5) '(gt gt add add) '() )
; (evaluar '(gt "aaa" "zzz") '(gt gt) '() )


; ;#########################
; ;TEST de lt
; ;#########################
; (evaluar '(lt 4 5) '(lt lt) '() )
; (evaluar '(lt 7 5) '(lt lt) '() )
; (evaluar '(lt (add 5 9) 5) '(lt lt add add) '() )
; (evaluar '(lt "aaa" "zzz") '(lt lt) '() )
;(evaluar '(lt (setq toto 10) 50) '(lt lt) '() )


;#########################
;TEST de not
;#########################
; (evaluar '(not 4 5) '(not not) '() )
; (evaluar '(not) '(not not) '() )
; (evaluar '(not 5) '(not not) '() )
; (evaluar '(not nil) '(not not nil nil) '() )
; (evaluar '(not t) '(not not t t) '() )
; (evaluar '(not "t") '(not not) '() )
; (evaluar '(not (add 5 9)) '(not not add add) '() )
; (evaluar '(not (setq da 9)) '(not not setq setq) '() )
; (evaluar '(not "aaa" "zzz") '(not not) '() )


;#########################
;TEST de setq
;#########################
(evaluar '(setq dada nil) '(not not nil nil) '() )
(evaluar '(setq dada 5) '(not not nil nil) '() )
(is (= '(1 (+ add doble (lambda (x) (+ x x)) x doble y 1)) (evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add) nil)))
(evaluar '(setq x (de doble (x) (+ x x)) y 1) '(+ add) nil)

(evaluar '(setq x (de doble (x) (+ x x))) '(+ add) nil)
;#########################
;TEST de null
;#########################
; (evaluar '(null '()) '(null null nil nil) '() )
; (evaluar '(null ()) '(null null nil nil) '() )
; (evaluar '(null a) '(null null nil nil) '() )
; (evaluar '(null a) '(null null nil nil a NIL) '() )
; (evaluar '(null "a") '(null null nil nil) '() )
; (evaluar '(null 'a) '(null null nil nil) '() )

;#########################
;TEST de prin3
;#########################
; (evaluar '(prin3 '()) '(prin3 prin3 nil nil) '() )
; (evaluar '(prin3 ()) '(prin3 prin3 nil nil) '() )
; (evaluar '(prin3 a) '(prin3 prin3 nil nil) '() )
; (evaluar '(prin3 a) '(prin3 prin3 nil nil a NIL) '() )
; (evaluar '(prin3 "a") '(prin3 prin3 nil nil) '() )
; (evaluar '(prin3 'a) '(prin3 prin3 nil nil) '() )


;#########################
;TEST de read
;#########################
;(evaluar '(read '()) '(read read nil nil) '() )
; (evaluar '(read) '(read read nil nil) '() )
; (evaluar '(read '(5 8)) '(read read nil nil) '() )
; (aplicar 'read '(()) '(read read nil nil add add) '() )

;#########################
;TEST de rest
;#########################
; (evaluar '(rest '(1 2 3 4 5)) '(rest rest nil nil) '() )
; (evaluar '(rest '(1 2 3 4 5) 3) '(rest rest nil nil) '() )
; (evaluar '(rest) '(rest rest nil nil) '() )
; (evaluar '(rest '(5 8)) '(rest rest nil nil) '() )
; (evaluar '(rest nil) '(rest rest nil nil) '() )
; (aplicar 'rest '(()) '(rest rest nil nil add add) '() )

; ; (evaluar '(env) '(rest rest nil nil add add env env) '(hola hola) )
; ; (aplicar 'env '() '(rest rest nil nil add add env env) '(hola hola) )





;#########################
;TEST de Hernan
;#########################

; (is = '((5) (+ add r 5)) (aplicar 'rest '((4 5)) '(+ add r 5) nil))
; (is = '(nil (+ add r 5)) (aplicar 'rest '(()) '(+ add r 5) nil))
; (is = '((5 nil) (+ add r 5)) (aplicar 'rest '((4 5 ())) '(+ add r 5) nil))
; (aplicar 'rest '((4 5 ())) '(+ add r 5) nil)


; (= '(nil (+ add r 5)) (aplicar 'null '(t) '(+ add r 5) nil))
; (= '(t (+ add r 5)) (aplicar 'null '(nil) '(+ add r 5) nil))
; (= '(t (+ add r 5)) (aplicar 'null '(()) '(+ add r 5) nil))


; (= '(12 (+ add)) (evaluar '((lambda (x) (+ x x ))6) '(+ add) '(x 6)))
; (= '(6 (+ add doble (lambda (x) (+ x x))) (evaluar '(doble 3) '(+ add doble (lambda (x) (+ x x))) nil)))
; (= '(8 (+ add r 4 doble (lambda (x) (+ x x))) (evaluar '(doble r) '(+ add r 4 doble (lambda (x) (+ x x))) nil)))


; (= '(nil (equal equal first first)) (evaluar '(cond nil) '(equal equal first first) nil))
; (evaluar '(cond nil) '(equal equal first first) nil)
; (= '(nil (equal equal first first)) (evaluar '(cond ((equal 'a 'b) (setq x 1))) '(equal equal first first) nil))
; (= '(2 (equal equal setq setq y 2)) (evaluar '(cond ((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2))) '(equal equal setq setq) nil))
; (= '(3 (equal equal setq setq y 2 z 3)) (evaluar '(cond ((equal 'a 'b) (setq x 1)) ((equal 'a 'a) (setq y 2) (setq z 3))) '(equal equal setq setq) nil))
; (= '(t (equal equal first first)) (evaluar '(cond ((equal 'a 'a))) '(equal equal first first) nil))
; (evaluar '(cond ((equal 'a 'a))) '(equal equal first first) nil)
; (= '(nil (equal equal first first)) (evaluar '(cond ((equal 'a 'b))) '(equal equal first first) nil))


(run-tests)