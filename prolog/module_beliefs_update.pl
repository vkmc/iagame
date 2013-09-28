:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2,
            visitados/1
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2, visitados/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultados por el resto del código del agente.
%

update_beliefs(Perc):-

	% Se elimina cualquier valor temporal asociado a una percepcion anterior
        % Se agrega el conocimiento del valor temporal de la percepcion actual.
        %
        % T es el tiempo actual

	retractall(time(_)),
	member(time(T), Perc),		
	assert(time(T)),

        % Eliminar conocimiento previo

	% Si el agente recuerda que en una posicion vio una entidad
        % y en la percepcion actual no encuentra nada sobre la misma posicion,
        % se elimina el conocimiento asociado.
        % Ejemplo, una entidad oro fue recogida por otro agente.
        % 
        % Pos1 es la posicion se deberia encontrar la entidad
        % E1 es la entidad recordada

	forall(
                (
                        member(node(Pos1,_,_),Perc),
			at(E1,Pos1),
			not(member(at(E1,Pos1),Perc))
		),
                        retract(at(E1,Pos1))               
              ),

        % Si el agente recuerda que en una posicion vio una entidad
        % y en la percepcion actual ve a esa misma entidad en otra posicion distinta,
        % se elimina el conocimiento asociado.
        % Ejemplo, un agente se mueve de Pos2 a Pos3.
        %
        % Pos2 es la posicion donde se deberia encontrar la entidad
        % Pos3 es la nueva posicion en donde se ve la entidad
        % E2 es la entidad recordada

        forall(
                (
                        member(at(E2, Pos3), Perc),
                        at(E2, Pos2),
                        Pos2 \= Pos3
                ),(
                        retract(at(E2, Pos2))
                )
              ),

        % Se elimina todo el conocimiento asociado a las posiciones en el mundo

        forall(
                (atPos(_,_)),
                retract(atPos(_,_))
              ),

        % Si el agente recuerda que una entidad E3 estaba en una posicion sobre el mapa
        % y en la percepcion actual ve que entidad E3 es propiedad de una entidad,
        % se elimina el conocimiento asociado.
        % Ejemplo, un agente recogio una entidad oro
        %
        % E3 es la entidad que estaba sobre el mapa y ahora es propiedad de otra entidad
        % Pos4 es la posicion del mapa en la que se encontraba E3

        forall(
                (
                        member(has(_,E3),Perc),
                        at(E3,Pos4)
                ),(
                        retract(at(E3,Pos4))
                )
              ),  
	
        % Si el agente recuerda que una entidad E4 tenia a otra entidad E5
        % y en la percepcion actual ve que entidad E5 esta en una posicion sobre el mapa,
        % se elimina el conocimiento asociado.
        % Ejemplo, un agente dejo caer una entidad oro.
        %
        % E4 es la entidad que tenia a la entidad E5
        % E5 es la entidad que era propiedad de E4 y ahora esta sobre el mapa

	forall(
		(
			member(at(E5,_),Perc),
			has(E4,E5)
		),
			retract(has(E4,E5))
	      ),

        % Si el agente recuerda que una entidad E6 tenia a otra entidad E8
        % y en la percepcion actual ve que entidad E7 tiene a la entidad E8,
        % se elimina el conocimiento asociado.
        % Ejemplo, un agente E6 dejo caer una entidad oro y otro agente E7 lo recogio.
        %
        % E6 es la entidad que tenia a la entidad E8
        % E7 es la entidad que actualmente tiene a la entidad E8

	forall(
		(
			member(has(E7,E8), Perc),
			has(E6,E8),
			E6 \= E7
		),
			retract(has(E6,E8))
	      ),

        % Se elimina el conocimiento sobre pertenencias
        %
        % E9 es la entidad que tenia a la entidad E10
        % E10 es la entidad de E9

        forall(
                (
                        member(at(E9,_),Perc),
                        has(E9,E10)
                ),
                        retract(has(E9,E10))
              ),

	% Se elimina el conocimiento de las descripciones de las entidades
        %
        % Nota: No tiene sentido controlar que descripciones 
        % han sido modificadas, por lo que se actualizan todas.
        %
        % E11 es la entidad descripta
        % L1 es la lista de descripciones de E11
         
	forall(
		(
			member(entity_descr(E11,_),Perc),
                        entity_descr(E11,L1)
		),(
                        retract(entity_descr(E11,L1))
		)
	      ),

        % Agregar conocimiento provisto por la percepcion

	% at/2
        % Se agregan las entidades que no estaban en la memoria del agente.
	
        forall(
                (
			member(at(E12,Pos5),Perc),
			not(at(E12,Pos5))
		),(
			assert(at(E12,Pos5))
                )
	      ),

        forall(
                (
                        member(atPos(E16, Vector), Perc),
                        not(atPos(E16, Vector))
                ),
                        assert(atPos(E16, Vector))
              ),                       

        % has/2
        % Se agregan todas las relaciones de pertenencia que no estaban
        % en la memoria del agente.

        forall(
                member(has(E13,E14), Perc),
                assert(has(E13,E14))
              ),

        % entity_descr/2
        % Se agregan todas las descripciones de entidades que no estaban
        % en la memoria del agente.

        forall(
                (
                        member(entity_descr(E15,L2), Perc),
                        L2 \= []
                ),
                        assert(entity_descr(E15,L2))
              ),

	% node/3
        % Se agrega los nodos que no estaban en la memoria del agente.
	forall(
		(
			member((node(Id,Pos6,Connections)), Perc),
			not(node(Id,Pos6,Connections))
		),
        		assert(node(Id,Pos6,Connections))				
	      ),

        visitados.

% visitados/1
% Se agrega las posiciones que han sido visitadas

visitados :- at([agent,me], Pos), visitados(Pos),!.
visitados :- at([agent,me], Pos), assert(visitados(Pos)).
