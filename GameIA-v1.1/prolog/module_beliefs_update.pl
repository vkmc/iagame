:- module(beliefs_update,
	  [
	    update_beliefs/1,
	    time/1,
	    node/3,
	    at/2,
	    atPos/2,
	    has/2,
	    entity_descr/2
	  ]).

:- dynamic time/1, node/3, at/2, atPos/2, has/2, entity_descr/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% IMPORTANTE: Debe exportarse todo predicado dinámico (creencia)
% manipulado por la actualización de creencias, para que puedan ser
% consultados por el resto del código del agente.
%

update_beliefs(Perc):-

	% Se agrega el conocimiento del nuevo tiempo percibidos.
	retractall(time(_)),
	member(time(T), Perc),		
	assert(time(T)),

	% Se eliminan las entidades que desaparecieron de su antigua posición.
	forall(
		(
			member(node(_,Pos,_),Perc),
			at(Entity,Pos),
			not(member(at(Entity,Pos),Perc))
		),
			retract(at(Entity,Pos))
	      ),

	% Se agregan las entidades que no estaban en la memoria del agente.
	forall(
		(
			member(at(Entity,Pos),Perc),
			not(at(Entity,Pos))
			
		),
			assert(at(Entity,Pos))
	      ),

	%retractall(atPos(_,_)),
	
	% Se eliminan los hechos has/2 que han sido afectados por la adicion de un at/2
	% Si esta has(E1,E2) entre nuestros hechos, pero en la percepcion detectamos que esta at(E2,_)
	% esto quiere decir que E1 no tiene mas a E2

	forall(
		(
			member(at(E2,_),Perc),
			has(E1,E2)
		),
			retract(has(E1,E2))
	      ),

	% Se eliminan los hechos at/2 que han sido afectados por la adicion de un has/2
	% Si esta at(E2,_) entre nuestros hechos, pero en la percepcion detectamos que esta has(E1,E2)
	% esto quiere decir que E2 no está más en el suelo. 

	forall(
		(
			member(has(E1,E2),Perc),
			at(E2,_)
		),(
			retract(at(E2,_)),
			assert(has(E1,E2))
		)
	      ),

	% Se eliminan los hechos has/2 que no sean validos en la percepcion actual
	% Si existe has(E1,E3) y en la percepcion existe has(E2,E3), y E1 != E2, 
	% entonces sustituimos has(E1,E3) con has(E2,E3)

	forall(
		(
			member(has(E2,E3), Perc),
			has(E1,E3),
			E1\=E2
		),(
			retract(has(E1,E3)),
			assert(has(E2,E3))
		)
	      ),

	% Se actualizan los conocimientos de entity_descr/2 eliminando los conocimientos previos y agregando los de la percepción actual.
	forall(
		(
			member(entity_descr(E,L2),Perc),
			entity_descr(E,L1)
		),(
			retract(entity_descr(E,L1)),
			assert(entity_descr(E,L2))
		)
	      ),


	% Se agrega el conocimiento de los nuevos nodos percibidos.
	forall(
		(
			member(nodo(Id,Pos,Connections), Perc),
			not(nodo(Id,Pos,Connections))
		),
		
			assert(nodo(Id,Pos,Connections))
				
	      ).
