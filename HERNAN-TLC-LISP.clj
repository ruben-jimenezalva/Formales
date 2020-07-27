(declare evaluar)
(declare aplicar)
(declare controlar-aridad)
(declare igual?)
(declare cargar-arch)
(declare imprimir)
(declare actualizar-amb)
(declare revisar-f)
(declare revisar-lae)
(declare buscar)
(declare evaluar-cond)
(declare evaluar-secuencia-en-cond)

; REPL (read–eval–print loop).
; Aridad 0: Muestra mensaje de bienvenida y se llama recursivamente con el ambiente inicial.
; Aridad 1: Muestra >>> y lee una expresion y la evalua
; Si la 2da. posicion del resultado es nil, retorna true (caso base de la recursividad).
; Si no, imprime la 1ra. pos. del resultado y se llama recursivamente con la 2da. pos. del resultado.

(defn repl
   ([]
      	(println "Interprete de TLC-LISP en Clojure")
	 					(println "Trabajo Practico de 75.14/95.48 - Lenguajes Formales 2020")
	  				(println "Inspirado en:")
      	(println "  TLC-LISP Version 1.51 for the IBM Personal Computer")
      	(println "  Copyright (c) 1982, 1983, 1984, 1985 The Lisp Company") (flush)
      	(repl '(	add add append append cond cond cons cons de de env env equal equal eval eval exit exit
 					first first ge ge gt gt if if lambda lambda length length list list load load lt lt nil nil not not
 					null null or or prin3 prin3 quote quote read read rest rest reverse reverse setq setq sub sub
 				    t t terpri terpri + add - sub)))
   ([amb]  
      (print ">>> ") (flush)
      (try (let 	[res (evaluar (read) amb nil)]
	            	(if	(nil? (fnext res))
				    	true
			            (do (imprimir (first res)) (repl (fnext res)))))
           	(catch Exception e (println) (print "*error* ") (println (get (Throwable->map e) :cause)) (repl amb))))
)

(defn body
    [expre] (first (nnext expre))
)
(defn nameDe
    [expre] (fnext expre)
)
(defn nextSetq
    [expre] (cons 'setq  (next (nnext expre)))
)
; Evalua una expresion usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la evaluacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'too-many-args) y el ambiente es el ambiente global.
; Si la expresion es un escalar numero o cadena, retorna la expresion y el ambiente global.
; Si la expresion es otro tipo de escalar, la busca (en los ambientes local y global) y retorna el valor y el ambiente global.
; Si la expresion es una secuencia nula, retorna nil y el ambiente global.
; Si el primer elemento de la expresion es '*error*, retorna la expresion y el ambiente global.
; Si el primer elemento de la expresion es una forma especial o una macro, valida los demas elementos y retorna el resultado y el (nuevo?) ambiente.
; Si no lo es, se trata de una funcion en posicion de operador (es una aplicacion de calculo lambda), por lo que se llama a la funcion aplicar,
; pasandole 4 argumentos: la evaluacion del primer elemento, una lista con las evaluaciones de los demas, el ambiente global y el ambiente local. 
(defn evaluar 
    [expre ambGlobal ambLocal]
        (if (not (seq? expre))
            (if (or (number? expre) (string? expre))
                (list expre ambGlobal)
                (list (buscar expre (concat ambGlobal ambLocal)) ambGlobal)
            )
            (cond   
                (igual? expre nil) (list nil ambGlobal)
                (igual? (first expre) '*error*) (list expre ambGlobal)
                (igual? (first expre) 'cond) (evaluar-cond (next expre) ambGlobal ambLocal)
                (igual? (first expre) 'de) 
                    (cond 
                        (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) ambGlobal)
                        (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) ambGlobal)
                        (not (symbol? (fnext expre)) ) (list (list '*error* 'symbol 'expected (fnext expre) ) ambGlobal)
                        (and (not (igual? (body expre) nil)) (not (seq? (body expre))) ) (list (list '*error* 'list 'expected (body expre) ) ambGlobal)
                        true 
                            ( let [nameFunc (nameDe expre), bodyLambda (nnext expre)]
                                (list nameFunc (actualizar-amb ambGlobal nameFunc (cons 'lambda bodyLambda)))
                            )
                    )
                (igual? (first expre) 'exit) (if (< (count (rest expre)) 1) (list nil nil) (list (list '*error* 'too-many-args) ambGlobal))
                (igual? (first expre) 'lambda) 
                    (cond 
                        (< (count (next expre)) 1) (list (list '*error* 'list 'expected nil) ambGlobal)
                        (and (not (igual? (fnext expre) nil)) (not (seq? (fnext expre))) ) (list (list '*error* 'list 'expected (fnext expre) ) ambGlobal)
                        true (list expre ambGlobal)
                    )
                (igual? (first expre) 'if) 
                    (cond 
                        (< (count (next expre)) 2) (list (list '*error* 'list 'expected nil) ambGlobal)
                        true (do
                            (def responseCond (evaluar (fnext expre) ambGlobal ambLocal))
                            (if (igual? (first responseCond) nil)
                                (evaluar (second(nnext expre)) ambGlobal ambLocal)
                                (evaluar (first(nnext expre)) ambGlobal ambLocal)
                            )
                        )
                    )
                (igual? (first expre) 'load)  
                    (cond 
                        (< (count (next expre)) 1) (list (list 'error 'too-few-args) ambGlobal)
                        (> (count (next expre)) 2) (list (list 'error 'too-many-args) ambGlobal)
                        (= (count (next expre)) 1) 
                            (list 't (cargar-arch ambGlobal ambLocal (fnext expre)) )
                        (= (count (next expre)) 2)
                            (list (list 'error 'not-implementd-yet) ambGlobal)
                    )
                (igual? (first expre) 'or) 
                    (cond
                        (< (count (next expre))1) (list nil ambGlobal)
                        (= (count (next expre))1) (list (first (evaluar (fnext expre) ambGlobal ambLocal)) ambGlobal)
                        true (do (let [ response (evaluar (list 'or (fnext expre)) ambGlobal ambLocal)]
                            (if (igual? (first response) nil)
                                (evaluar (cons 'or (nnext expre)) ambGlobal ambLocal)
                                response
                            )
                        ))
                    )   
                ; para todos los argumentos evaluar y se corta en la primer condicion verdadera, o se devuelve nil si todos son falsos
                (igual? (first expre) 'quote) (list (if (igual? (fnext expre) nil) nil (fnext expre)) ambGlobal)

                (igual? (first expre) 'setq) 
                    (cond 
                        (< (count (next expre)) 2) (list (list '*error* 'expected nil) ambGlobal)
                        (igual? (fnext expre) nil) (list (list '*error* 'cannot-set nil) ambGlobal)
                        (not (symbol? (fnext expre)) ) (list (list '*error* 'symbol 'expected (fnext expre) ) ambGlobal)
                        (= (count (next expre)) 2) (let [response (evaluar (body expre) ambGlobal ambLocal), key (fnext expre), value (first response), newAmbGlobal (fnext response)]
                                (list value (actualizar-amb newAmbGlobal key value) )
                        )
                        true (
                            ;recursive setq (setq x (+ 1 1) y 2)
                            let [response (evaluar (body expre) ambGlobal ambLocal), key (fnext expre), value (first response), newAmbGlobal (fnext response)] ; evaluo primer cuerpo
                            (evaluar (nextSetq expre) (actualizar-amb newAmbGlobal key value) ambLocal) ; se retorna la ultima evaluacion de setq (cae en el caso anterior)
                        )
                    )
                true (do ( let [f (first (evaluar (first expre) ambGlobal ambLocal)), lae (map (fn [x] (first (evaluar x ambGlobal ambLocal))) (next expre) )]
                    (aplicar f lae ambGlobal ambLocal)
                ))
            )
        )
)


; Aplica una funcion a una lista de argumentos evaluados, usando los ambientes global y local. Siempre retorna una lista con un resultado y un ambiente.
; Si la aplicacion falla, el resultado es una lista con '*error* como primer elemento, por ejemplo: (list '*error* 'arg-wrong-type) y el ambiente es el ambiente global.
; Aridad 4: Recibe la func., la lista de args. evaluados y los ambs. global y local. Se llama recursivamente agregando 2 args.: la func. revisada y la lista de args. revisada.
; Aridad 6: Si la funcion revisada no es nil, se la retorna con el amb. global.
; Si la lista de args. evaluados revisada no es nil, se la retorna con el amb. global.
; Si no, en caso de que la func. sea escalar (predefinida o definida por el usuario), se devuelven el resultado de su aplicacion (controlando la aridad) y el ambiente global.
; Si la func. no es escalar, se valida que la cantidad de parametros y argumentos coincidan, y:
; en caso de que se trate de una func. lambda con un solo cuerpo, se la evalua usando el amb. global intacto y el local actualizado con los params. ligados a los args.,  
; en caso de haber multiples cuerpos, se llama a aplicar recursivamente, pasando la funcion lambda sin el primer cuerpo, la lista de argumentos evaluados,
; el amb. global actualizado con la eval. del 1er. cuerpo (usando el amb. global intacto y el local actualizado con los params. ligados a los args.) y el amb. local intacto. 

(defn aplicar
    ([f lae ambGlobal ambLocal]
        (aplicar (revisar-f f) (revisar-lae lae) f lae ambGlobal ambLocal)
    )
    ([fRevisada laeRevisada f lae ambGlobal ambLocal]
        (cond    
            fRevisada (list fRevisada ambGlobal)      ; Si la funcion esta revisada tiene errores, se retorna el error con el ambiente global
            laeRevisada (list laeRevisada ambGlobal)  ; Si la la lista de arg esta revisada tiene errores, se retorna el error con el ambiente global
            true (if (not (seq? f)) 
                (do (def res (cond
                    (igual? f 'env) (if (not (igual? lae nil))
                        (list '*error* 'too-many-args)
                        ;(if (igual? ambLocal nil) ambGlobal (concat ambGlobal ambLocal))
                        (seq (concat ambGlobal ambLocal))
                    )
                    (igual? f 'add) (if (< (count lae) 2)
                        (list '*error* 'too-few-args)
                        (try 
                            (reduce + lae)
                            (catch Exception e (list '*error* 'number-expected))
                        )
                    )
                    (igual? f 'sub) (if (< (count lae) 2)
                        (list '*error* 'too-few-args)
                        (try (reduce - lae)
                            (catch Exception e (list '*error* 'number-expected))
                        )
                    )
                    (igual? f 'first) ( let [ari (controlar-aridad lae 1)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (igual? (first lae) nil) nil
                            (not (seq? (first lae))) (list '*error* 'list-expected (first lae))
                            true (ffirst lae)
                        )
                    )
                    (igual? f 'reverse) ( let [ari (controlar-aridad lae 1)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (or (igual? lae nil) (igual? (first lae) nil)) nil
                            (not (seq? (first lae))) (list '*error* 'list 'expected (first lae))
                            true (reverse (first lae))
                        )
                    )
                    (igual? f 'equal) ( let [ari (controlar-aridad lae 2)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (igual? (first lae) (second lae)) 't
                            true nil
                        )
                    )
                    (igual? f 'null) ( let [ari (controlar-aridad lae 1)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (igual? (first lae) nil) 't
                            true nil
                        )
                    )
                    (igual? f 'gt) ( let [ari (controlar-aridad lae 2)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (not (number? (first lae))) (list '*error* 'number-expected (first lae))
                            (not (number? (second lae))) (list '*error* 'number-expected (second lae))
                            (> (first lae) (second lae)) 't
                            true nil
                        )
                    )
                    (igual? f 'lt) ( let [ari (controlar-aridad lae 2)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (not (number? (first lae))) (list '*error* 'number-expected (first lae))
                            (not (number? (second lae))) (list '*error* 'number-expected (second lae))
                            (< (first lae) (second lae)) 't
                            true nil
                        )
                    )
                    (igual? f 'ge) ( let [ari (controlar-aridad lae 2)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (not (number? (first lae))) (list '*error* 'number-expected (first lae))
                            (not (number? (second lae))) (list '*error* 'number-expected (second lae))
                            (>= (first lae) (second lae)) 't
                            true nil
                        )
                    )
                    (igual? f 'not) ( let [ari (controlar-aridad lae 1)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (igual? (first lae) nil) 't
                            (igual? (first lae) 't) nil
                            true (list '*error* 'unbound-symbol (first lae))
                        )
                    )
                    (igual? f 'append) ( let [ari (controlar-aridad lae 2)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (not (seq? (first lae))) (list '*error* 'list-expected (first lae))
                            (not (seq? (second lae))) (list '*error* 'list-expected (second lae))
                            true (concat (first lae) (second lae))
                        )
                    )
                    (igual? f 'cons) ( let [ari (controlar-aridad lae 2)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (igual? (second lae) nil) (list (first lae))
                            ;(seq? (second lae)) (cons (first lae) (second lae))
                            true (cons (first lae) (second lae))
                        )
                    )
                    (igual? f 'length) ( let [ari (controlar-aridad lae 1)]
                        (cond 
                            (seq? ari) ari ; Hubo un error ya que la aridad de lae es incorrecta
                            (and (not (seq? (first lae))) (not (igual? (first lae) nil))) (list '*error* 'arg-wrong-type (first lae))
                            true (count (first lae))
                        )
                    )
                    (igual? f 'list) (seq (map #(if (igual? % ()) nil %) lae))
                    (igual? f 'rest) (if (not (seq? (first lae)))
                        (list '*error* 'list-expected (first lae))
                        (if (igual? (first lae) nil) nil (seq(map #(if (igual? % ()) nil %) (next (first lae)))))
                    )
                    (igual? f 'prin3) (cond 
                        (and (igual? lae nil) (< (count lae) 1)) (list '*error* 'too-few-args)
                        (> (count lae) 2) (list '*error* 'too-many-args)
                        (= (count lae) 2) (list '*warning* 'not-implementd-yet)
                        true (do (print (first lae)) (first lae))
                    )
                    (igual? f 'terpri) (cond
                        (igual? lae nil) (do (println) nil)
                        (= (count lae) 1) (list '*warning* 'not-implementd-yet)
                        (> (count lae) 1) (list '*error* 'too-many-args)
                    )
                    (igual? f 'read) (cond
                        (igual? lae nil) (read)
                        (= (count lae) 1) (list '*warning* 'not-implementd-yet)
                        (> (count lae) 1) (list '*error* 'too-many-args)
                    )
                ))
                (list res ambGlobal))
                (cond 
                    (> (count (second f)) (count lae)) (list (list '*error* 'too-few-args) ambGlobal)
                    (< (count (second f)) (count lae)) (list (list '*error* 'too-many-args) ambGlobal)
                    true (let [hasOneBody (nil? (next (nnext f)))]
                        (if hasOneBody
                            (do (let [newAmbLocal (concat (reduce concat(map list (fnext f) lae)) ambLocal)] 
                                (evaluar (first (nnext f)) ambGlobal newAmbLocal))
                            )
                            (do (let [newAmbLocal (concat (reduce concat(map list (fnext f) lae)) ambLocal)] 
                                (aplicar (cons 'lambda (cons (fnext f) (next (nnext f)))) lae (fnext (evaluar (first (nnext f)) ambGlobal newAmbLocal)) ambLocal))
                            )
                        )
                    )
                )
            )
        )
    )
)

; Controla la aridad (cantidad de argumentos de una funcion).
; Recibe una lista y un numero. Si la longitud de la lista coincide con el numero, retorna el numero.
; Si es menor, retorna (list '*error* 'too-few-args).
; Si es mayor, retorna (list '*error* 'too-many-args).
(defn controlar-aridad 
    [lista valEsperado]      
    (def tamanio (count lista))
    (if (not (seq? lista)) 
        (list '*error* 'list 'expected lista)
        (cond
            (= tamanio valEsperado) valEsperado
            (< tamanio valEsperado) (list '*error* 'too-few-args)
            (> tamanio valEsperado) (list '*error* 'too-many-args)
        )
    )
)

 
; Compara la igualdad de dos simbolos.
; Recibe dos simbolos a y b. Retorna true si se deben considerar iguales; si no, false.
; Se utiliza porque TLC-LISP no es case-sensitive y ademas no distingue entre nil y la lista vacia.
(defn igual? 
   [a b]
   (let [ nulos '("NIL" "nil" NIL () 'nil nil) ]
       (cond
           (and (some #(= a %) nulos) (some #(= b %) nulos) ) true
           (or (some #(= a %) nulos) (some #(= b %) nulos) ) false
           true (if (= (clojure.string/lower-case a) (clojure.string/lower-case b)) true false)
       )
   )
)
 
; Carga el contenido de un archivo.
; Aridad 3: Recibe los ambientes global y local y el nombre de un archivo

; (literal como string o atomo, con o sin extension .lsp, o el simbolo ligado al nombre de un archivo en el ambiente), abre el archivo 
; y lee un elemento de la entrada (si falla, imprime nil), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y un arg. mas: el resultado de la evaluacion.
; Aridad 4: lee un elem. del archivo (si falla, imprime el ultimo resultado), lo evalua y llama recursivamente con el (nuevo?) amb., nil, la entrada y el resultado de la eval.
(defn cargar-arch
    ([amb-global amb-local arch]
        (let [nomb (first (evaluar arch amb-global amb-local))]
            (if (and (seq? nomb) (igual? (first nomb) '*error*))
                (do (imprimir nomb) amb-global) 
                (let [nm (clojure.string/lower-case (str nomb)),
                    nom (if (and (> (count nm) 4)(clojure.string/ends-with? nm ".lsp")) nm (str nm ".lsp")),
                    ret (try (with-open [in (java.io.PushbackReader. (clojure.java.io/reader nom))]
                                    (binding [*read-eval* false] (try (let [res (evaluar (read in) amb-global nil)]
                                                                                                    (cargar-arch (fnext res) nil in res))
                                                                                    (catch Exception e (imprimir nil) amb-global))))
                                        (catch java.io.FileNotFoundException e (imprimir (list '*error* 'file-open-error 'file-not-found nom '1 'READ)) amb-global))]
                        ret))))
    ([amb-global amb-local in res]
        (try (let [res (evaluar (read in) amb-global nil)] (cargar-arch (fnext res) nil in res))
            (catch Exception e (imprimir (first res)) amb-global)))
)

; Imprime, con salto de linea, atomos o listas en formato estandar (las cadenas con comillas) y devuelve su valor. Muestra errores sin parentesis.
; Aridad 1: Si recibe un escalar, lo imprime con salto de linea en formato estandar (pero si es \space no lo imprime), purga la salida y devuelve el escalar.
; Si recibe una secuencia cuyo primer elemento es '*error*, se llama recursivamente con dos argumentos iguales: la secuencia recibida.
; Si no, imprime lo recibido con salto de linea en formato estandar, purga la salida y devuelve la cadena.
; Aridad 2: Si el primer parametro es nil, imprime un salto de linea, purga la salida y devuelve el segundo parametro.
; Si no, imprime su primer elemento en formato estandar, imprime un espacio y se llama recursivamente con la cola del primer parametro y el segundo intacto.
(defn imprimir
    ([elem]
        (if (seq? elem)
            (if (= (first elem) '*error*)
                (imprimir elem elem) 
                (do 
                    (println (str elem))
                    elem
                )
            )
            (cond
                (= \space elem) elem
                true (do 
                        (if (string? elem) (println (str "\"" elem "\"")) (println elem))
                        elem
                )
            )
        )
    )
    ([lis orig]
        (if (empty? lis)
            (do
                (newline)
                orig
            )
            (do
                (def elem (nth lis 0))
                (if (string? elem)  (printf "%s " (str "\"" elem "\"")) (printf "%s " elem))
                (imprimir (nthrest lis 1) orig)
            )
        )
    )
)
 
(defn devolverNilTlcLisp
    [valor] ( if (igual? valor nil) nil valor)
)

; Actualiza un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...]
; Recibe el ambiente, la clave y el valor.
; Si el valor no es escalar y en su primera posicion contiene '*error*, retorna el ambiente intacto.
; Si no, coloca la clave y el valor en el ambiente (puede ser un alta o una actualizacion) y lo retorna.
(defn actualizar-amb
    [ambGlobal clave valor] 
    (cond
        (and (seq? valor) (= (first valor) 'error)) ambGlobal
        true (let [vect (vec ambGlobal)]
            (def dict (partition 2 vect))
            (def pair (filterv #(= (first %) clave) dict))
            (def value (second (first pair)))
            (if (nil? value)
                (concat ambGlobal (list clave (devolverNilTlcLisp valor)))
                (do 
                    (def indexClave (.indexOf vect clave))
                    (seq (assoc vect (inc indexClave) (devolverNilTlcLisp valor)))   
                )
            )
        )
    )
 )

; Revisa una lista que representa una funcion.
; Recibe la lista y, si esta comienza con '*error*, la retorna. Si no, retorna nil.
(defn revisar-f 
   [lista]
   (when (list? lista)
       (def existe (some #(= '*error* %) lista))
       (when existe lista)
   )
)
 
; Revisa una lista de argumentos evaluados.
; Recibe la lista y, si esta contiene alguna sublista que comienza con '*error*, retorna esa sublista. Si no, retorna nil.
(defn revisar-lae [lis]  
   (let [subSeq (filter seq? lis) ]
       (some #(if (= (first %) '*error*) %1) subSeq)
   )
)
 
; Busca una clave en un ambiente (una lista con claves en las posiciones impares [1, 3, 5...] y valores en las pares [2, 4, 6...] y retorna el valor asociado.
; Si no la encuentra, retorna una lista con '*error* en la 1ra. pos., 'unbound-symbol en la 2da. y el elemento en la 3ra.
(defn buscar 
    [elem lista]
    (if (igual? elem nil) 
        nil
        (do (let [vect (vec lista)]
            (def dict (partition 2 vect))
            (def pair (filterv #(igual? (first %) elem) dict))
            (def value (second (first pair)))
            (if (nil? value) (list '*error* 'unbound-symbol elem) value))
        )
    )
)

; Evalua el cuerpo de una macro COND. Siempre retorna una lista con un resultado y un ambiente.
; Recibe una lista de sublistas (cada una de las cuales tiene una condicion en su 1ra. posicion) y los ambientes global y local.
; Si la lista es nil, el resultado es nil y el ambiente retornado es el global.
; Si no, evalua (con evaluar) la cabeza de la 1ra. sublista y, si el resultado no es nil, retorna el res. de invocar a evaluar-secuencia-en-cond con la cola de esa sublista.
; En caso contrario, sigue con las demas sublistas. 


(defn evaluar-cond 
    [lis ambGlobal ambLocal]    
    (if (igual? lis nil) 
        (list 'nil ambGlobal)
        (do 
            (def response (evaluar (ffirst lis) ambGlobal ambLocal))
            (if (igual? (first response) nil)
                (evaluar-cond (next lis) ambGlobal ambLocal)
                (do 
                    (def conditions (next (first lis)))
                    (if (igual? conditions nil)
                        response
                        (evaluar-secuencia-en-cond conditions ambGlobal ambLocal)
                    )
                )
            )
        )  
    )
)
 
; Evalua (con evaluar) secuencialmente las sublistas de una lista y retorna el valor de la ultima evaluacion.
(defn evaluar-secuencia-en-cond 
    [lis ambGlobal ambLocal]
    (if (= (count lis) 1) 
        (evaluar (first lis) ambGlobal ambLocal)
        (do 
            (def response (evaluar (first lis) ambGlobal ambLocal))
            (evaluar-secuencia-en-cond (next lis) (second response) ambLocal)
        )
    )
)
 
; Al terminar de cargar el archivo, se retorna true.
(let [devolver true] devolver)


(evaluar '(or nil nil (gt 9 5) ) '(nil nil + add gt gt) '())
(evaluar '(or nil nil (gt 1 5) ) '(nil nil + add gt gt) '())
(evaluar '(or nil nil (gt 1 5) dasbd ) '(nil nil + add gt gt) '())
(evaluar '(setq x (de doble (x) (+ x x))) '(+ add) nil)