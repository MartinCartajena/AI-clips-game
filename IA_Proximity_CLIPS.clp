
(defglobal
    ?*DIM* = 4 ; Estos valores luego se cambian
    ?*TAM* = 16
    ?*FICH_JUG* = 8 ; Cantidad de fichas restantes
    ?*FICH_IA* = 8 
    ?*TURNO* = TRUE ; TRUE = turno jugador; FALSE = turno cpu
    ?*COLOR_B* = TRUE ; TRUE = blanco; FALSE = negro
    ?*COLOR_N* = FALSE
    ?*PUNT_B* = 0 ; Global para ir guardando la puntuacion
    ?*PUNT_N* = 0


    ; Globales para la IA.
    ?*CONT_ID* = 1
    ?*CONT_INSER* = 0
    ?*MAX_PROF* = 2
    ?*POS_IA* = 0 ; Despues de recorrer el arbol guardar aqui la posicion de la solucion
    ?*VALOR_IA* = 0 ; Despues de recorrer el arbol guardar aqui el valor de la solucion
)


;////////////// DEFTEMPLATES /////////////////

(deftemplate tablero
    (multislot matriz (type STRING))
)

(deftemplate ocupados
    (multislot casillas)
)

(deftemplate restantes
    (slot jugador)
    (multislot fichas)
)

(deftemplate ficha
    (slot color)
)

; => Parte IA
; Representa un estado en el arbol, es final si... (su nivel == ?*MAX_PROF* y valor != FALSE).
(deftemplate estado
    (multislot matriz (type STRING))
    (slot id) ; id del estado.
    (slot id_padre) ; id del estado padre.
    (slot nivel) ; nivel => profundidad del nodo. (Raiz en 0).
    (slot insertado (default FALSE)) ; ficha insertada para llegar al nodo (mov para padre => actual).
    (slot posicion)
    (slot valor (default FALSE)) ; valor heuristico.
)

(deftemplate solucion_candidata
    (slot id)
    (slot nivel)
    (slot valor)
    (slot posicion)
    (slot insertado)
)


;////////////// FUNCIONES AUXILIARES /////////////////

(deffunction establecer_parametros ()
    (if (= ?*DIM* 2) then
        (bind ?auxiliar 1)
        (bind ?*MAX_PROF* 2) 
    else (if (= ?*DIM* 4) then 
        (bind ?auxiliar (+ ?*DIM* 0)) 
        (bind ?*MAX_PROF* 2) 
        (if (> ?*CONT_INSER* 5) then
            (bind ?*MAX_PROF* 3)
        )

    else (if (= ?*DIM* 6) then
        (bind ?auxiliar (+ ?*DIM* 7))
        (bind ?*MAX_PROF* 2)

    else (if (= ?*DIM* 8) then
        (bind ?auxiliar (/ ?*TAM* 8))
        (bind ?*MAX_PROF* 1)
    ))))

    (return ?auxiliar)
)

; Funcion auxiliar para cambiar de turno
(deffunction cambiar_turno ()
    (bind ?*TURNO* (not ?*TURNO*)) ; Negar la variable logica, TRUE => FALSE / FALSE => TRUE
    (return ?*TURNO*)
)


(deffunction print_tablero_vacio ()

    (bind ?linea "   ")
    (loop-for-count (?i 1 ?*DIM*)
        (bind ?linea (str-cat ?linea  ?i "  "))
    )
    (printout t ?linea crlf)

    (loop-for-count (?i 1 ?*DIM*)
        (bind ?linea "")
        (bind ?linea (str-cat ?i "  ")) 

        (bind ?cont 1)
        (while (< ?cont (+ ?*DIM* 1))
            (bind ?linea (str-cat ?linea "-- "))     
            (bind ?cont (+ ?cont 1))
        )

        (printout t crlf ?linea crlf)
    )
    (printout t crlf)
)


(deffunction print_tablero ($?m)

    (printout t crlf "-> TABLERO:" crlf crlf)

    (bind ?linea "   ")
    (loop-for-count (?i 1 ?*DIM*)
        (bind ?linea (str-cat ?linea  ?i "  "))
    )
    (printout t ?linea crlf)    

    (loop-for-count (?fila 1 ?*DIM*)
        (bind ?linea "")
        (bind ?linea (str-cat ?fila "  ")) 

        (loop-for-count (?col 1 ?*DIM*)
            (bind ?indice (+ (* (- ?fila 1) ?*DIM*) ?col)) ; indice = (DIM * (Fila-1)) + Columna       
            (bind ?casilla (nth$ ?indice $?m))
            (bind ?linea (str-cat ?linea ?casilla " "))     
        )

        (printout t crlf ?linea crlf)
    )
    (printout t crlf)
    
)

(deffunction calcularPuntuacion ($?m)
    (bind ?blanco 0)
    (bind ?negro 0)

    (progn$ (?i $?m) 
         
        (bind $?aux (create$ ?i))
        (bind ?valor (implode$ ?aux)) ; Pasar el campo a string
        (bind ?color (sub-string 1 1 ?valor)) ; obtener el color
        (if (neq ?color "-") then

            (bind ?long (str-length ?valor)) ; Longitud para obtener la parte del numero
            (bind ?valor (sub-string 2 ?long ?valor)) ; Obtener el numero
            (bind ?valor (string-to-field ?valor)) ; Pasar de String a un campo para operar

            (if (eq ?color "b") then ; sumar a blancas
                (bind ?blanco (+ ?blanco ?valor))
            else                     ; sumar a negras
                (bind ?negro (+ ?negro ?valor))
            )
        )

    )   

    (bind ?*PUNT_B* ?blanco) ; asignar a la variable global
    (bind ?*PUNT_N* ?negro)
    (printout t "-> Puntuacion Blancas: " ?blanco crlf)
    (printout t "-> Puntuacion Negras: " ?negro crlf)  
)

; cambioas en el tablero despues de comprobar los adyacentes
(deffunction modificaciones (?valor ?valorAdy ?posAdy ?color $?m)
            ; valor = | valorAdy = string | posAdy = numerico | color = Boolean | $?m = multicampo
       
    ; Separar el color y el valor (son string)
    (bind ?colorAdy (sub-string 1 1 ?valorAdy)) 
    (bind ?longAdy (str-length ?valorAdy)) ; Longitud para obtener la parte del numero
    (bind ?valorAdy (sub-string 2 ?longAdy ?valorAdy)) ; Obtener el numero
    (bind ?valorAdy (string-to-field ?valorAdy)) ; Pasar de String a un campo para operar

    (if (eq ?colorAdy "b") then
        (bind ?colorAdyAux TRUE) ; Color blanco es True
    else
        (bind ?colorAdyAux FALSE) ; Color negro es False
    )

    (if (eq ?colorAdyAux ?color) then ; Si son las fichas el mismo jugador
        (bind ?valorAdy (+ ?valorAdy 1)) ; Sumar uno
        (bind ?nuevo (str-cat ?colorAdy ?valorAdy))
        (bind ?nuevo (string-to-field ?nuevo)) ; Pasar de string a campo 
        (bind $?m (replace$ $?m ?posAdy ?posAdy ?nuevo)) ; Insertar

    else 
        (if (> ?valor ?valorAdy) then ; Si el insertado es mayor que el vecino hacer el cambio
            (if (eq ?colorAdy "b") then ; Hcer el cambio
                (bind ?colorAdy "n") 
            else
                (bind ?colorAdy "b") 
            )
            (bind ?nuevo (str-cat ?colorAdy ?valorAdy))
            (bind ?nuevo (string-to-field ?nuevo)) ; Pasar de string a campo 
            (bind $?m (replace$ $?m ?posAdy ?posAdy ?nuevo)) ; Insertar   
        )
    ) 

    (return $?m)

)


(deffunction adyacentes (?pos ?valor ?color $?m)

    ; Empezar obteniendo el valor desde el string (porq se tiene guardado el color y el numero a la vez)
    (bind ?long (str-length ?valor))
    (bind ?valor (sub-string 2 ?long ?valor)) ; puede ser de un digito o 2
    (bind ?valor (string-to-field ?valor)) ; Pasar a campo para poder operar luego

    ; Obtener los indices de los adyacentes y hacer la modificacion (mismo codigo para los 8 casos)
    (if (> (- ?pos ?*DIM*) 0) then ; if i - DIM > 0 
        (bind ?posAdy (- ?pos ?*DIM*)) ; norte = pos - DIM
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )
        
    )
    (if (<= (+ ?pos ?*DIM*) ?*TAM*) then ; if i + DIM < TAM
        (bind ?posAdy (+ ?pos ?*DIM*)) ; sur = pos + DIM
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )
    (if (neq (mod ?pos ?*DIM*) 0) then ; if i % DIM != 0 
        (bind ?posAdy (+ ?pos 1)) ; este = pos - 1
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )
    (if (neq (mod (- ?pos 1) ?*DIM*) 0) then ; if (i-1) % DIM != 0 
        (bind ?posAdy (- ?pos 1)) ; oeste = pos + 1
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )

    (if (and (> (- (- ?pos ?*DIM*) 1) 0) (neq (mod (- ?pos 1) ?*DIM*) 0) ) then ; if (norte - 1 > 0) and ((i-1) % DIM != 0)
        (bind ?posAdy (- (- ?pos ?*DIM*) 1)) ; noroeste = norte - 1
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )
    (if (and (> (+ (- ?pos ?*DIM*) 1) 0) (neq (mod ?pos ?*DIM*) 0) ) then ; if (norte + 1 > 0) and (i % DIM != 0)
        (bind ?posAdy (+ (- ?pos ?*DIM*) 1)) ; noreste = norte + 1
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )
    (if (and (< (- (+ ?pos ?*DIM*) 1) ?*TAM*) (neq (mod (- ?pos 1) ?*DIM*) 0) ) then ; if (sur - 1 <= TAM) and ((i-1) % DIM != 0)
        (bind ?posAdy (- (+ ?pos ?*DIM*) 1)) ; suroeste = sur - 1
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )
    (if (and (<= (+ (+ ?pos ?*DIM*) 1) ?*TAM*) (neq (mod ?pos ?*DIM*) 0) ) then ; if (sur + 1 <= TAM) and (i % DIM != 0)
        (bind ?posAdy (+ (+ ?pos ?*DIM*) 1)) ; sureste = sur + 1
        (bind ?valorAdy (implode$ (create$ (nth$ ?posAdy $?m))) ) ; Obtener el valor del adyacente desde la matriz
        (if (neq ?valorAdy "--") then
            (bind $?m (modificaciones ?valor ?valorAdy ?posAdy ?color $?m))
        )

    )

    (return $?m)
)

; //////////  PARTE DEL JUGADOR /////////////
; Insertar la nueva ficha (devuelve el tablero modificado)
(deffunction insertar (?color ?cas ?fich $?m)

    ; Antes de empezar obtener el multicampo del tablero
    ; como el string dentro de un multicampo se quiere obtener el proio string (un solo elemento en el multiampo)
    (bind $?m (nth$ 1 $?m)) 
    (bind $?matriz (explode$ $?m)) ; Multicampo

    ; Preguntar datos
    (printout t "=> Que numero quieres insertar?: " )
    (bind ?num_escogida (read))
    ; Comprobar q es aceptable, si entre 1 y TAM, ademas si tiene disponible el jugador
    (while (eq (member$ ?num_escogida ?fich) FALSE )
        (printout t "=> Has anadido esta ficha, vuelve a insertar el numero:  " )
        (bind ?num_escogida (read))
    )
    (bind ?fichTot (/ ?*TAM* 2))
    (while (or (> ?num_escogida ?fichTot )(< ?num_escogida 1) )
        (printout t "=> Demasiado grande, vuelve a insertar el numero:  " )
        (bind ?num_escogida (read))
    )


	(printout t "=> Donde quieres colocar la ficha? " crlf)
	(printout t "==> Fila: " )
	(bind ?fil_escogida (read))
	(printout t "==> Columna: " ) 
	(bind ?col_escogida (read))
	
	; Comprobar coordenadas: mirar si los datos introducidos estan dentro del tablero
	(while (or (or (> ?fil_escogida ?*DIM* ) (< ?fil_escogida 1)) (or (> ?col_escogida ?*DIM* ) (< ?col_escogida 1) ) ) 
		; Si no se encuentra dentro del tablero preguntar de nuevo
		(printout t " Coordenada fuera del limite! " crlf)
		(printout t "==> Vuelve a insertar la Fila: " )
		(bind ?fil_escogida (read))
		(printout t "==> Vuelve a insertar la Columna: " ) 
		(bind ?col_escogida (read))
	)

    (bind ?pos (+ (* (- ?fil_escogida 1) ?*DIM*) ?col_escogida)) ; pos = (DIM * (Fila-1)) + Columna
    ; Comprobar que la casilla no esta ocupada
    (bind ?ocupado (member$ ?pos ?cas))
    (while ?ocupado
        (printout t " Casilla ocupada!  " crlf)
        (printout t "==> Vuelve a insertar la Fila: " )
		(bind ?fil_escogida (read))
		(printout t "==> Vuelve a insertar la Columna: " ) 
		(bind ?col_escogida (read))

        (bind ?pos (+ (* (- ?fil_escogida 1) ?*DIM*) ?col_escogida))
        (bind ?ocupado (member$ ?pos ?cas))
    )

    ; Obtener el nuevo valor, pero primero obtener el color
    (if ?color then
        (bind ?valor "b")
    else 
        (bind ?valor "n")
    )
    (bind ?valor (str-cat ?valor ?num_escogida))
    (bind ?valorAux ?valor) ; esto para las modificaciones
    (bind ?valor (explode$ ?valor)) 
    ; Insertar
    (bind $?matriz (replace$ $?matriz ?pos ?pos ?valor))

    
    ; // Para los asserts //
    ; Añadir al hecho la nueva posicicon en la que se ha insertado
    (bind $?nuevo (insert$ ?cas (+ 1 (length $?cas)) ?pos))
    (assert (ocupados (casillas ?nuevo)))

    ; Obtiene el indice de la ficha que se ha insertado en los restantes
    (bind ?posEnRestantes (member$ ?num_escogida ?fich)) ; Esto es arriesgado, pero arriba se ha gestionado
    ; De este modo se puede quitar esa ficha del hecho
    (bind ?fich (delete$ ?fich ?posEnRestantes ?posEnRestantes))
    (assert (restantes (jugador persona) (fichas ?fich))) ; jugador persona esta asi, pero eso hay q mirar


    ; Funcion para modificar los adyacentes despues de insertar 
    (bind $?matriz (adyacentes ?pos ?valorAux ?color $?matriz))


    (print_tablero $?matriz)

    ; Actualizar las nuevas puntuaciones
    (calcularPuntuacion $?matriz)
    (printout t "-> Fichas del Jugador: " ?fich crlf)

    ; Devolver el nuevo tablero para modificar el hecho luego
    (bind $?m (implode$ $?matriz)) ;pasar multicampo a string
    (return $?m)
 
)


(defrule turno_persona
    ?w <- (turno_jugador)
    ?t <- (tablero (matriz $?m))
    ?o <- (ocupados (casillas $?cas))
    ?r <- (restantes (jugador persona) (fichas $?fich))
    (restantes (jugador ia) (fichas $?fichIa)) ; Solo para el print de fichas restantes de la ia
  
    =>
    (printout t crlf "======>  TURNO DEL JUGADOR  <======" crlf)
    
    (bind $?nuevo (insertar ?*COLOR_B* $?cas $?fich $?m))
    (bind ?*FICH_JUG* (- ?*FICH_JUG* 1)) ; actualizar la cantidad de fichas del jugador
    (printout t "-> Fichas de la IA: "  ?fichIa crlf crlf) ; La parte de ia aqui t del jugador en la funcion insertar


    ; 2 formas: eliminar y volver a anadir o midificar
    (modify ?t (matriz $?nuevo))
    ;(assert (tablero (matriz $?nuevo)))
    ;(retract ?t)


    ; Eliminar los ocupados de antes de insertar
    (retract ?o)
    ; Eliminar los restantes de antes de insertar
    (retract ?r)
    ; eliminar el turno
    (retract ?w)

)

; //////////  PARTE DE LA IA  /////////////

(defrule turno_ia
    ?t <- (tablero (matriz $?m))
    ?w <- (turno_ia)

    =>

    (printout t crlf "======>  TURNO DEL ORDENADOR  <======" crlf)

    ; Creamos el nodo raiz.
    (assert (estado (matriz $?m) (id 0) (id_padre FALSE) (nivel 0)))

    ; Resetear para empezar de 0 con los nuevos id
    (bind ?*CONT_ID* 1)
    (bind ?*CONT_INSER* (+ ?*CONT_INSER* 1))

    (retract ?w)
)



(deffunction funcion_evaluacion (?resJug ?resIa $?m)
    (bind ?blanco 0)
    (bind ?negro 0)

    (progn$ (?i $?m) 
         
        (bind $?aux (create$ ?i))
        (bind ?valor (implode$ ?aux)) ; Pasar el campo a string
        (bind ?color (sub-string 1 1 ?valor)) ; obtener el color
        (if (neq ?color "-") then

            (bind ?long (str-length ?valor)) ; Longitud para obtener la parte del numero
            (bind ?valor (sub-string 2 ?long ?valor)) ; Obtener el numero
            (bind ?valor (string-to-field ?valor)) ; Pasar de String a un campo para operar

            (if (eq ?color "b") then ; sumar a blancas
                (bind ?blanco (+ ?blanco ?valor))
            else                     ; sumar a negras
                (bind ?negro (+ ?negro ?valor))
            )
        )

    )
    (bind $?resJug $?resJug)
    (bind ?sumResJug 1)
    (progn$ (?i $?resJug) 
        (bind ?sumResJug (+ ?sumResJug ?i))
    )

    (bind $?resIa $?resIa)
    (bind ?sumResIa 1)
    (progn$ (?i $?resIa) 
        (bind ?sumResIa (+ ?sumResIa ?i))
    )

    (bind ?negro (/ ?negro ?sumResIa )) 
    (bind ?negro (/ ?blanco ?sumResJug)) 
    (bind ?res (- ?negro ?blanco ))
    (return ?res)  

)


; Para crear los nuevos tableros cuando se crean los hechos con los nuevos estados
(deffunction insertar_ia (?color ?id_padre ?nuevo_nivel ?num ?pos ?resJug ?resIa $?matriz)
                        ; color = BOOLEAN | ?num = campo | $?matriz = multicampo

    (if ?color then
        (bind ?col "b")
    else 
        (bind ?col "n")
    )

    (bind ?valor (str-cat ?col ?num))
    (bind ?valorAux ?valor) ; para las adyacentes
    ;(bind ?valor (explode$ ?valor)) una de las 2
    (bind ?valor (string-to-field ?valor))
    ; Insertar
    (bind $?matriz (replace$ $?matriz ?pos ?pos ?valor))
    ; Funcion para modificar los adyacentes despues de insertar 
    (bind $?matriz (adyacentes ?pos ?valorAux ?color $?matriz))

    ; Crear un nuevo id.
    (bind ?id_nuevo ?*CONT_ID*)
    (bind ?*CONT_ID* (+ ?*CONT_ID* 1))

    ; En el caso de que el tablero se haya llenado, ponerle el valor q sino peta
    (bind ?lleno TRUE)
    (progn$ (?i $?matriz)
        (bind $?aux (create$ ?i))
        (bind ?aux (implode$ ?aux)) ; Pasar el campo a string
        (if (eq ?aux "--") then
            (bind ?lleno FALSE)
        )
    )

    ; Profundidad maxima alcanzada... estado final, anadir heuristico.
    (if (or (eq ?lleno TRUE)(= ?nuevo_nivel ?*MAX_PROF*) ) then
        (bind ?heur (funcion_evaluacion ?resJug ?resIa $?matriz))
        
    else
        (bind ?heur FALSE)
    )

    (bind $?m (implode$ $?matriz)) ;pasar multicampo a string
    (assert (estado (matriz $?m) (id ?id_nuevo) (id_padre ?id_padre) (nivel ?nuevo_nivel) (insertado ?valor) (posicion ?pos) (valor ?heur) ) )  
)



; Crear el arbol añadiendo hechos
(defrule crear_arbol
    (declare (salience 20))
    (estado (matriz $?m) (id ?id) (nivel ?nivel) (valor ?valor))
    (restantes (jugador ia) (fichas $?fich))
    (restantes (jugador persona) (fichas $?fichPe)) 

    (test (and (< ?nivel ?*MAX_PROF*) (eq ?valor FALSE))) ; para saber que no es estado terminal (no tiene heuristico ni sumera max prof.)
    =>

    (bind ?nuevo_nivel (+ ?nivel 1))
    
    ; Dependiendo del nivel... el siguiente nivel es IA o jugador.
    (if (= 0 (mod ?nuevo_nivel 2)) then 
        (bind ?color ?*COLOR_B*)
        (bind $?fichasAux $?fichPe) ; Dependiendo de quien sea el turno hay que analizar unas fichas u otras
    else
        (bind ?color ?*COLOR_N*)
        (bind $?fichasAux $?fich) ; Aux para no modificar estas
    )
    
    (bind $?m (nth$ 1 $?m)) 
    (bind $?matriz (explode$ $?m)) ; Multicampo


    (bind ?auxiliar (establecer_parametros))

    (if (< ?*CONT_INSER* ?auxiliar) then
        (bind ?pos 1)
        (progn$ (?j $?matriz) ; recorrer todos los estados  
            (bind $?auxj (create$ ?j))
            (bind ?auxj (implode$ ?auxj)) ; Pasar el campo a string
            (if (eq ?auxj "--") then

                ; insertar la nueva pos en la matriz y añadir el nuevo hecho como estado
                (if (= 0 (mod ?nuevo_nivel 2)) then ; Turno del jugador, insertar
                    (insertar_ia ?*COLOR_B* ?id ?nuevo_nivel ?*CONT_INSER* ?pos $?fichPe $?fich $?matriz)                
                else
                    (insertar_ia ?*COLOR_N* ?id ?nuevo_nivel ?*CONT_INSER* ?pos $?fichPe $?fich $?matriz)
                )        
            )
            (bind ?pos (+ ?pos 1)) 
        )

    
    else
        
        ; Por cada posible insercion crear nuevo nodo en el arbol
        (progn$ (?numEscogido $?fichasAux) ; recorrer fichas restantes
            (bind ?pos 1)
            (progn$ (?j $?matriz) ; recorrer todos los estados  
                (bind $?auxj (create$ ?j))
                (bind ?auxj (implode$ ?auxj)) ; Pasar el campo a string
                (if (eq ?auxj "--") then

                    ; insertar la nueva pos en la matriz y añadir el nuevo hecho como estado
                    (if (= 0 (mod ?nuevo_nivel 2)) then ; Turno del jugador, insertar
                        (insertar_ia ?*COLOR_B* ?id ?nuevo_nivel $?numEscogido ?pos $?fichPe $?fich $?matriz)                
                    else
                        (insertar_ia ?*COLOR_N* ?id ?nuevo_nivel $?numEscogido ?pos $?fichPe $?fich $?matriz)
                    )        
                )
                (bind ?pos (+ ?pos 1)) 
            )
        )
    )
  
    
)
; Indicar que se a terminado de crear el arbol y se puede analizar
(defrule arbol_creado
    (declare (salience 10))

    (estado (nivel ?n) (valor ?valor))
    (test (not (eq ?valor FALSE )))
    =>
    (assert (arbol_finalizado))
)

; Analizar el arbol para obtener posibles candidatos 
(defrule recorrer_arbol
    (declare (salience 120))
    (arbol_finalizado)
    ?actual <- (estado  (id ?id_a) (id_padre ?id_abuelo) (nivel ?nivel_a) (valor ?valor_a))
    ?hijo <- (estado  (id ?id_h) (id_padre ?id_padre) (nivel ?nivel_h) (valor ?valor_h) (posicion ?pos) (insertado ?insertado))

    ; Miras desde el anterior a las hojas (el hijo es hoja)
    (test (neq ?valor_h FALSE))
    (test (eq ?id_padre ?id_a)) 

    => 

    ; Para saber si el nodo es max o min.
    (bind ?max (= 0 (mod ?nivel_a 2)))

    ; Nodo actual es MAX.
    (if ?max then
        ; Si no tiene valor, ponerle por defecto-INF.
        ; Es diferente al de los apuntes
        (if (not ?valor_a) then
            (bind ?valor_a -99999)
        )
        
        ; Nuevo valor: Max (valor_previo, valor_hijo).
        (bind ?nuevoValActual (max ?valor_a ?valor_h))

    ; Nodo actual es MIN.    
    else
        ; Setear a valor por defecto, INF.
        (if (not ?valor_a) then
            (bind ?valor_a 99999)
        )

        ; Nuevo valor: Min (valor_previo, valor_hijo).
        (bind ?nuevoValActual (min ?valor_a ?valor_h))

    )
     
    ; Modificr el valor del padre (el actual).
    (modify ?actual (valor ?nuevoValActual))
    

    ; Si es el nodo raiz guardar la mejor solucion
    (if (= 0 ?id_a) then   
        (if (> ?valor_h ?valor_a) then ; si tiene mejor valor, actualizar la raiz (valor y movimiento)
            (assert (solucion_candidata (id ?id_a) (nivel ?nivel_a) (valor ?valor_h) (posicion ?pos) (insertado ?insertado)))
        )
        ;(assert (posible_solucion (valor ?valor_h) (movimiento ?mov)))
    )

    ; Eliminar hijo.
   (retract ?hijo) ; poner antes del modify porq sino bucle infinito

)

; Obtener la mejor de las soluciones candidatas
(defrule obtener_solucion
    (declare (salience 90))
    ?af <- (arbol_finalizado)
    ?raiz <- (estado (id 0) (valor ?valor_raiz))
    ?sol <- (solucion_candidata (valor ?valorS) (posicion ?pos) (insertado ?insertado))

    (test (= ?valor_raiz ?valorS))

     => ; de la forma qn la que se ha hecho habra varias soluciones candidatas. Pero el estado raiz tendra el mejor valor.
        ; De este modo, se toma la sol candidata  con ese mismo valor y se puede obtener la posicion y valor de la ficha a insetar

    (bind ?*POS_IA* ?pos)
    (bind ?*VALOR_IA* ?insertado)

    (retract ?af)
    (assert (eliminar_hechos))
)

(defrule eliminar_hechos_estados
    (declare (salience 300))
    (eliminar_hechos)
    ?e <- (estado)
    =>
    (retract ?e)
)
(defrule eliminar_hechos_candidatas
    (declare (salience 300))
    (eliminar_hechos)
    ?c <- (solucion_candidata)
    =>
    (retract ?c)
)
(defrule final_recorrido
    (declare (salience 290))
    ?eh <- (eliminar_hechos)
    =>
    (retract ?eh)
    (assert (solucion_obtenida))
)

; Una vez obtenido la ficha, insertar
(deffunction insertar_ia_sol (?color ?cas ?fich ?valorF ?pos $?m)
                ; ?color = BOOL | ?cas = ulticampo | ?fich = multicampo | valorF = string, | ?pos = slot | $?m = multicampo

    ; Antes de empezar obtener el multicampo del tablero
    ; como el string dentro de un multicampo se quiere obtener el proio string (un solo elemento en el multiampo)
    (bind $?m (nth$ 1 $?m)) 
    (bind $?matriz (explode$ $?m)) ; Multicampo
    

    ; Obtener el valor para hacer calculos luego
    (bind ?long (str-length ?valorF))
    (bind ?val (sub-string 2 ?long ?valorF)) ; puede ser de un digito o 2
    (bind ?val (string-to-field ?val)) ; Pasar a campo para poder operar luego

    ; Insertar
    (bind $?matriz (replace$ $?matriz ?pos ?pos ?valorF))

    ; // Para los asserts //
    ; Añadir al hecho la nueva posicicon en la que se ha insertado
    (bind $?nuevo (insert$ ?cas (+ 1 (length $?cas)) ?pos))
    (assert (ocupados (casillas ?nuevo)))

    ; Obtiene el indice de la ficha que se ha insertado en los restantes
    (bind ?posEnRestantes (member$ ?val ?fich)) ; Esto es arriesgado, antes se ha gestionado

    ; De este modo se puede quitar esa ficha del hecho
    (bind ?fich (delete$ ?fich ?posEnRestantes ?posEnRestantes))
    (assert (restantes (jugador ia) (fichas ?fich))) ; jugador persona esta asi, pero eso hay q mirar


    ; Funcion para modificar los adyacentes despues de insertar 
    (bind $?matriz (adyacentes ?pos ?valorF ?color $?matriz))


    (print_tablero $?matriz)
    
    (printout t "-> Ficha insertada por la IA: " ?valorF crlf crlf)

    ; Actualizar las nuevas puntuaciones
    (calcularPuntuacion $?matriz)
    (printout t "-> Fichas de la IA: " ?fich crlf)

    ; Devolver el nuevo tablero para modificar el hecho luego
    (bind $?m (implode$ $?matriz)) ;pasar multicampo a string
    (return $?m)

)

(defrule fin_turno_ia
    ?sol <- (solucion_obtenida)

    ?t <- (tablero (matriz $?m))
    ?o <- (ocupados (casillas $?cas))
    ?r <- (restantes (jugador ia) (fichas $?fich))
    (restantes (jugador persona) (fichas $?fichPe)) ; Lo mismo que en turno_persona pero al reves

    =>
    
    (bind $?nuevo (insertar_ia_sol ?*COLOR_N* $?cas $?fich ?*VALOR_IA* ?*POS_IA* $?m))
    (bind ?*FICH_IA* (- ?*FICH_IA* 1)) ; actualizar la cantidad de fichas de la ia
    (printout t "-> Fichas del Jugador: "  ?fichPe crlf crlf)
    
    (modify ?t (matriz $?nuevo))
    (retract ?o)
    (retract ?r)
)

; //////////  MECANICA DE TURNOS /////////////
; Cada cambio de turno se pasa por aqui para ver si el juego finaliza e indicarde quien es el turno 
(defrule turno
    ?t <- (tablero (matriz $?m))
    =>

    ; Mirar si el juego finaliza
    (if (and (eq ?*FICH_JUG* 0) (eq ?*FICH_IA* 0) ) then

        (if (eq ?*PUNT_B* ?*PUNT_N*) then
            (assert (tablas))
            (return)
        else (if (> ?*PUNT_B* ?*PUNT_N*) then
            (assert (ganan_blancas))
            (return)
        else 
            (assert (ganan_negras))
            (return)
        ))  
    )
   
    ; Mirar a quien le toca el turno
    (if ?*TURNO* then ; A quien le toca
        (assert (turno_jugador)) 
    else
        (assert (turno_ia)) 
    )
    (cambiar_turno) ; Cambio de la var global que indica el turno
)

(defrule ganan_blancas
    (declare(salience 101))
    ?t <- (tablero (matriz $?m))
    (ganan_blancas)
    =>
    (assert(fin_juego))
    (printout t crlf "=> Han ganado las blancas!" crlf)
    (retract ?t)
)

(defrule ganan_negras
    (declare(salience 102))
    ?t <- (tablero (matriz $?m))
    (ganan_negras)
    =>
    (assert (fin_juego))
    (printout t crlf "=> Han ganado las negras!" crlf)
    (retract ?t)
)

(defrule tablas
    ?t <- (tablero (matriz $?m))
    (tablas)
    =>
    (assert (fin_juego))
    (printout t crlf "=> Tablas!" crlf)
    (retract ?t)
)

; Regla para detectar final de juego, se activa con el hecho
; (fin_juego) y para la ejecucion (halt).
(defrule fin_juego
    (fin_juego)
    =>
    (printout t crlf "=> Fin del juego!" crlf )
    (halt)
)


; //////////  INICIALIZACION DEL JUEGO /////////////

(defrule iniciar_juego
    ?i <- (inicio_juego)
    =>

    ; Representar el tablero inicial
    (printout t crlf "====> TABLERO INICIAL <====" crlf crlf)
    (print_tablero_vacio)

    ; Obtener las fichas iniciales
    (bind ?fichas "")
    (bind ?cantidad (/ ?*TAM* 2) )
    (loop-for-count (?i 1 ?cantidad)
        (bind ?fichas (str-cat ?fichas ?i " "))
    )
    

    ; Cambiar las fichas de strings a multicampo
    (bind $?fichas (explode$ ?fichas))

    ; Anadir el hecho que guarda las fichas de cada jugador
    (assert (restantes (jugador persona) (fichas $?fichas)))
    (assert (restantes (jugador ia) (fichas $?fichas) ))

    ;Indicar en las variables glbales cuantas fichas iniciales
    (bind ?*FICH_JUG* (length $?fichas))
    (bind ?*FICH_IA* (length $?fichas))

    (printout t "=> Fichas del Jugador: " $?fichas crlf)
    (printout t "=> Fichas de la IA: " $?fichas crlf)


        
    (retract ?i)
)


(deffunction pedir_parametros()
    ; Preguntar tamano 4, 6 o 8
    (bind ?tam -1) 
    (while (or (< ?tam 2) (> ?tam 9) (not (= 0 (mod ?tam 2))))  
        (printout t "=> Elige el tamanio del tablero (2, 4, 6, 8): " )
        (bind ?tam (read))
    )
    (bind ?*DIM* ?tam)
    (bind ?*TAM* (* ?*DIM* ?*DIM*))
    
    ; Anadir el tablero vacio
    (bind ?casilla "")
     (loop-for-count (?i 1 ?*DIM*)
        (loop-for-count (?j 1 ?*DIM*)
            (bind ?casilla (str-cat ?casilla "-- "))
        )
        
    )
    (assert (tablero (matriz ?casilla)))
    (assert (ocupados (casillas 0)))
    


    ; Preguntar si empieza persona o jugador
    (bind ?exit TRUE)
    (while (eq ?exit TRUE)
        (printout t "=>  Empieza el jugador o el cpu? (p/c): ")
        (bind ?quien (read))
        (if (= 0 (str-compare "p" (lowcase ?quien))) then
            (bind ?*TURNO* TRUE)
            (bind ?exit FALSE)
        )
        (if (= 0 (str-compare "c" (lowcase ?quien))) then
            (bind ?*TURNO* FALSE)
            (bind ?exit FALSE)
        )
    )

    ; Preguntar el color 
    (bind ?exit TRUE)
    (while (eq ?exit TRUE)
        (printout t "=>  Fichas blancas o negras? (b/n): ")
        (bind ?color (read))
        (if (eq 0 (str-compare "b" (lowcase ?color))) then 
            (bind ?*COLOR_B* TRUE)
            (assert (ficha (color ?color)))
            (bind ?exit FALSE)
        )
        (if (= 0 (str-compare "n" (lowcase ?color))) then
            (bind ?*COLOR_N* FALSE)
            (assert (ficha (color ?color)))
            (bind ?exit FALSE)
        )
    )

)


; Cuando (inicio) exista, comenzamos el juego pidiendo parametros
; al jugador, despues activamos (init_global).
(defrule parametros_iniciales
    ?existe <- (inicio)
    =>
    (pedir_parametros)
    (assert (inicio_juego))
    (retract ?existe)
)

; Se activa (inicio) para comenzar el juego
(deffacts inicializacion
    (inicio)
)

