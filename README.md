# Proyecto-Inteligencia-Artificial

La finalidad de este proyecto es desarrollar una maquina inteligente que sea capaz de jugar
al juego que se propone más en adelante. Se ha hecho uso de la herramienta <b>CLIPS</b> para la implementación, un lenguaje para el desarrollo de sistemas
expertos.
  
 La implementación del proyecto constará de dos jugadores, uno de ellos tendrá que tomar
las decisiones manualmente (humano), y el otro realizará los movimientos automáticamente usando
conocimientos inteligentes (máquina).


El juego implementado es el juego del Proximity:

Proximity es un juego de estrategia que se juega por turnos, creado por Brian Cable. Este
juego, se podría decir que es una mezcla entre el Risk y Go. Es un juego simple de aprender.
Se reparten la misma cantidad de fichas (uno rojo y otro azul, misma cantidad que casillas tenga el
tablero) entre los dos jugadores, las cuales tienen asignadas diferentes valores. Por ejemplo, si el
tablero es de 6x6, habrá 36 casillas, por lo que cada jugador dispondrá de 18 fichas, y estas fichas
tendrán valores entre 1 y 18.

En cada turno, se debe escoger y colocar una ficha en cualquier espacio libre del tablero de
juego. Si el número de la ficha es mayor que la(s) ficha(s) enemiga(s) junto a la(s) que se ha
colocado, el color de esa(s) ficha(s) se convierte en tu color. Sin embargo, si la ficha que se coloca
en el tablero está situado al lado de alguna de tu(s) ficha(s), esta(s) ficha(s) que están al lado de la
colocada incrementan en 1 su valor. El ganador es aquél jugador que tenga la suma más grande de
los pesos de las fichas de su color sobre el tablero al finalizar la partida.

La partida continuaría hasta que los dos jugadores coloquen todas las fichas. Al final, el
jugador que tenga la suma más grande de los pesos de las fichas de su color, ganaría la partida.


