%% Player-Agent BDI
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_actions_rep_and_projection, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1, ucs/0, atPos/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      ag_name(_AgName),

      tell('log.txt'),

      update_beliefs(Percept),

      % display_ag, nl, !,

      deliberate,

      % FUE IMPLEMENTADO DE MANERA QUE SI POSTERIORMENTE FALLA LA OBTENCI�N DE UN PLAN PARA LA INTENCI�N
      % EN PRINCIPIO SELECCIONADA, VUELVE A RECONSIDERAR INTENCI�N.

      planning_and_execution(Action),

      do_action(Action),

      run,!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%   1. UPDATING BELIEFS	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     FROM PERCEPTIONS	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% update_beliefs(+Perc)
%
% Se encuentra definido en el m�dulo 'module_beliefs_update'.

% << DESARROLLADO EN ETAPA 1 >>


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%     2. DELIBERATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% deliberate
%
% El agente analiza si continuar� con su intenci�n actual, considerando
% deseos de alta prioridad, la factibilidad del plan
% para la intencion actual, si la intenci�n actual fue lograda, etc.
% En caso de no continuar con la intenci�n corriente, establece cual
% ser� la nueva intenci�n analizando los deseos existentes y
% seleccionando uno de ellos.

deliberate:-

	high_priority(HPDesire, Explanation),	 % Si existe un deseo HPDesire de alta prioridad:
						 % ( <<<CHOICE POINT>>> -- Posibilidad de backtracking )

	not(intention(HPDesire)),        % y no es el caso que coincide con la intenci�n actual,

	write('High-priority Desire: '), write(HPDesire), write(', since '), writeln(Explanation), nl,

	retractall(intention(_)),     % (Estrat�gicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	      %	 ante la b�squeda de una intenci�n alternativa (por no haberse encontrado un plan
	                              %  para la anterior), la intenci�n anterior se elimina y se hace assert de la nueva).

	assert(intention(HPDesire)),     % se establece HPDesire como intenci�n actual.
	assert(plan([HPDesire])).

deliberate:-       % Si

	(   not(intention(_)),                     % actualmente no hay intenci�n
	    writeln('There is no intention '), nl
	;                                          % o
	    intention(Int),
	    achieved(Int),                         % la intenci�n corriente fue lograda
	    write('Intention '), write(Int), writeln(' achieved.')
	;					   % o

	    plan([]),                              % el plan para para la intenci�n actual fue consumido
	    writeln('Plan consumed.')
	;                                          % o
	    (

	        plan(Plan), Plan \= [], not(feasible(Plan))   % el plan corriente se torn� no factible, o

		;

	        not(plan(_))                                  % no hay plan. Esto ocurre si se descubre que el plan actual es no
	                                                      % factible al intentar obtener, sin �xito, el (sub) plan para la
	                                                      % siguiente acci�n de alto nivel (falla el next_primitive_action).
	    ),
	    writeln('Current plan became infeasible.'), nl
	),

	!,

	findall(Desire, desire(Desire, _Explanation), Desires),  % Calcular el conjunto de deseos
	write('Desires: '), writeln(Desires),nl,
	select_intention(NewInt, NewExpl, Desires),   % Seleccionar una intenci�n
	                                              % (deseo que el agente se compromete a lograr)
	                                              % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	write('New Intention: '), write(NewInt), write(', since '), writeln(NewExpl), nl,

	retractall(intention(_)),  % (Estrategicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	   % ante la b�squeda de una intenci�n alternativa (por no haberse encontrado un plan
	                           % para la anterior), la intenci�n anterior se elimina y se asserta la nueva.)

	assert(intention(NewInt)),                    % Recordar la nueva intenci�n seleccionada.
	assert(plan([NewInt])).


deliberate:-
	intention(Int),
	write('Current Intention: '), writeln(Int), nl.
	% Caso contrario, se contin�a con la intenci�n y plan corrientes


deliberate:-            % Si llega ac� significa que fall� el next_primitive_action al planificar la siguiente acci�n
	                % de alto nivel de un plan factible. Ejecuto nuevamente el delibarate.
	deliberate.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%  2.1. COMPUTING DESIRES  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%% Metas / Deseos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% desire(-Desire, -Explanation)
%
% Retorna un deseo Desire (actualmente activo de acuerdo a las creencias
% del agente). Asociado al deseo se retorna una explicaci�n,
% especificando las razones por las cuales Desire se considera
% actualmente un deseo. En su forma m�s b�sica Explanation puede ser un
% string con una descripci�n narrada (breve) de dichas razones,
% que luego puede ser impresa por pantalla.

% Deseo descansar
%
% Si la vida del agente es menor a 100, desea descansar

desire(rest, 'want to have a nap'):-
	property([agent, me], life, St),
	St < 150.


% Deseo robar a otro agente
%
% Si recuerdo que vi un agente, ir a robarle es una meta

desire(steal_agent([agent, AgName], ItemList), 'I wanna steal from an agent') :-
        at([agent, AgName], _Pos),
	AgName \= me,
	property([agent, AgName], life, AgSt),
	property([agent, me], life, St),
	AgSt < St,
	St > 150,
	findall(ItName, has([agent, AgName], [_Item, ItName]), ItemList),
	ItemList \= [].


% Deseo agarrar oro o pocion
%
% Si recuerdo que hay oro o una pocion en el piso, agarrar ese oro o pocion
% es una meta

desire(get([Item, ItName]), 'I wanna grab gold/potions!') :-
        (Item = potion; Item = gold),
        at([Item, ItName], _PosIt).
		

% Deseo saquear una tumba
%
% Si me encuentro con una tumba, deseo saquearla

desire(break([grave, GrName]), "I wanna break into a grave") :-
	at([grave, GrName], _PosGr),
	has([agent, me], [potion,_]),
	has([grave, GrName], [gold,_]).


% Deseo explorar
%
% Si el agente no tiene algo mas importante que hacer, explora el mapa

desire(explore, 'I wanna go explore'):-
	findall(        Pos,
                (
                        node(Pos,_,Connections),                        
                        not(visitados(Pos)),
                        not(interior(Connections))
                ),
                        SinExplorar
               
               ),
	SinExplorar \= [].

% Deseo moverme
%
% Para no aburrirse, camina sin rumbo (en usa de esas se cruza otro agente!)

desire(move_at_random, 'I wanna keep moving!').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% high_priority(-HPDesire, Explanation).
%
% Determina si existe un deseo HPDesire de alta prioridad, es
% decir, tal que implica abandonar la intenci�n actual. En caso de
% existir tal deseo de alta prioridad, lo retorna.
%
% An�logamente al predicado desire/2, asociado al deseo HPDesire de alta
% prioridad se retorna una explicaci�n, especificando las
% razones por las cuales HPDesire se considera un deseo que
% debe adoptarse inmediatamente como intenci�n.
% En su forma m�s b�sica Explanation puede ser un string con una
% descripci�n narrada (breve) de dichas razones, que luego puede ser
% impresa por pantalla.

:- dynamic high_priority/2.

% Deseo de alta prioridad - Descansar

high_priority(rest, 'I need to rest') :-
	property([agent, me], life, St),
	St < 150, % running low of life...
	once(at([inn, _HName], _Pos)). % se conoce al menos una posada

high_priority(rest, 'Since I am around, I could take a nap') :-
	atPos([agent, me], Pos),
	atPos([inn, _InnName], Pos).


% Deseo de alta prioridad - Atacar agente

high_priority(attack_agent([agent, AgName]), 'I am being attacked! Attacking back...') :-
        atPos([agent, AgName], AgPos),
        atPos([agent, me], Pos),
        AgName \= me,
	property([agent, AgName], life, AgSt),
	AgSt > 0,
        pos_in_attack_range(Pos, AgPos).


% Deseo de alta prioridad - Robarle a otro agente

high_priority(steal_agent([agent, AgName], ItemList), 'I saw an agent, gonna steal from him!') :-
        at([agent, AgName], _Pos),
	AgName \= me,
	property([agent, AgName], life, AgSt),
	property([agent, me], life, St),
	AgSt < St,
	St > 150,
	findall(ItName, has([agent, AgName], [_Item, ItName]), ItemList),
	ItemList \= [].

% Deseo de alta prioridad - Agarrar un item

high_priority(get([Item, ItName]), 'I saw an item, gonna pick it up...') :-
        at([agent, me], Pos),
        at([Item, ItName], Pos),
        (Item = gold; Item = potion).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%  2.2. SELECTING INTENTIONS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                             %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% select_intention(-Intention, -Explanation, +CurrentDesires).
%
% Selecciona uno de los deseos corrientes del agente como intenci�n.
%
% En su forma m�s b�sica, define un orden de prioridad entre los deseos
% del agente, seleccionando como intenci�n al primero en este orden
% de prioridad.
%
% Tiene m�ltiples soluciones. Esto es adrede: si
% posteriormente a la selecci�n de una intenci�n (por parte de este
% predicado) no se encuentra plan para alcanzarla, entonces se obtendr�
% la siguiente intenci�n (mediante backtracking), hasta encontrar una
% viable, o agotarlas a todas.


% Seleccionar intencion - Descansar
%
% Dado que el nivel de stamina es relativamente bajo, se decide ir
% descansar antes de abordar otro deseo.

select_intention(rest, 'going to take a nap before...', Desires):-
	member(rest, Desires).


% Seleccionar intencion - Robarle a otro agente

select_intention(steal_agent([agent, AgName], ItemList), 'going to steal an agent', Desires) :-
	member(steal_agent([agent, AgName], ItemList), Desires),
        findall(Item, (has([agent, AgName], Item)), ItemList).


% Seleccionar intencion - Saquear una tumba

select_intention(break([grave, GrName1]), 'going to break into a grave', Desires) :-
        member(break([grave, GrName1]), Desires),
	buscar_deseo_cercano(Closer, Desires),
        at([grave, GrName1], Closer).


% Seleccionar intencion - Agarrar item
%
% De todos los posibles objetos tirados en el suelo que el agente desea tener,
% selecciono como intenci�n obtener aquel que se encuentra m�s cerca.

select_intention(get([Item, ItName]), 'closest item I want to get', Desires) :-
        member(get([Item, ItName]), Desires),
	at([agent, me], Pos),
        at([Item, ItName], Pos).

select_intention(get([Item, ItName]), 'item in my sight I want to get', Desires):-
	member(get([Item, ItName]), Desires),
	buscar_deseo_cercano(Closer, Desires),
        at([Item, ItName], Closer).


% Seleccionar intencion - Explorar
		
select_intention(explore, 'gonna explore! looking for new challenges...', Desires) :-
	member(explore, Desires).


% Seleccionar intencion - Moverse aleatoriamente

select_intention(move_at_random, 'nothing to do, gonna take a walk', Desires) :-
        member(move_at_random, Desires).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% achieved(+Intention).
%
% Determina si la intenci�n Intention fue alcanzada. Esto es, si
% se verifica de acuerdo a las creencias del agente.

% Logrado si el agente esta en la posicion

achieved(goto(Pos)):-
	at([agent, me], Pos).


% Logrado si el agente tiene el item

achieved(get(Item)):-
	has([agent, me], Item).


% Logrado si el agente esta descansado

achieved(rest):-
	property([agent, me], life, St),
	property([agent, me], lifeTotal, MaxSt),
	AlmostMaxSt is MaxSt - 10,
	St > AlmostMaxSt.


% Logrado si el agente tiene todos los items de la tumba

achieved(break([grave, GrName])) :-
        not(has([grave, GrName], [gold, _])).


% Logrado si el agente esta inconsiente o fuera del rango de ataque

achieved(attack_agent([agent, AgName])) :-
        property([agent, AgName], life, St),
        St = 0.

achieved(attack_agent([agent, AgName])) :-
        atPos([agent, me], Pos),
        atPos([agent, AgName], AgPos),
        AgName \= me,
        not(pos_in_attack_range(Pos, AgPos)).


% Logrado si el agente tiene todos los items que tenia otro agente

achieved(steal_agent([agent, _AgName], AgItemList)) :-
        findall(Item, (has([agent, me], Item)), ItemList),
        subset(ItemList, AgItemList).


% Logrado si el agente esta en nuestro rango de ataque

achieved(chase_agent([agent, AgName])) :-
        atPos([agent, AgName], AgPos),
        atPos([agent, me], Pos),
        AgName \= me,
        pos_in_attack_range(Pos, AgPos).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      3. PLANNING         %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       & EXECUTION        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planning_and_execution(-Action)
%
% Obtiene la siguiente acci�n primitiva Action correspondiente al plan
% actual, removi�ndola del plan. N�tese que la siguiente acci�n
% del plan podr�a ser de alto nivel (no primitiva), en cuyo caso debe
% planificarse hasta llegar al nivel de acciones primitivas.
% (Ver next_primitive_action/3).

planning_and_execution(Action):-

	retract(plan(Plan)),
	% Ejecutar siguiente acci�n del plan.
	write('Following plan: '), writeln(Plan), nl,
	next_primitive_action(Plan, Action, RestOfPlan),
	write('Next action: '), writeln(Action),
	assert(plan(RestOfPlan)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planify(+HLAction, -Plan)
%
% Define una librer�a de Planes, es decir, un mapeo de acciones de alto
% nivel (en particular, intenciones) a planes involucrando acciones de
% menor nivel.
%
% Dada una acci�n HLAction de alto nivel, retorna un plan (involucrando
% acciones de menor nivel) cuya ejecuci�n equivalga al efecto de la
% acci�n HLAction.
%
% Debe definirse una regla de este predicado por cada acci�n de alto
% nivel considerada por el agente (inclu�da las intenciones, que
% constituyen las acciones de m�s alto nivel).
%
% La planificaci�n de HLAction puede realizarse,
% seg�n el tipo de la acci�n, de las siguientes maneras:
%
%   a) simplemente especificando en forma "est�tica" la secuencia
%      (lista) de acciones de menor nivel cuyo efecto equivale al de
%       HLAction.
%
%   b) empleando un algoritmo de b�squeda (por ejemplo el implementado
%      para la etapa 2, permitiendo planificar el desplazamiento a una
%      posici�n determinada)
%
%   c) empleando el algoritmo de planeamiento STRIPS (cuya
%      implementaci�n es provista por la c�tedra), adecuado para
%      realizar planificaciones de metas m�s complejas, como obtener un
%      tesoro que se encuentra en una tumba.
%
%
% La opci�n a admite la especificaci�n de planes recursivos, donde en
% particular, la �ltima acci�n del plan para HLAction es la propia
% HLAction. Esto permite, por ejemplo, especificar que la
% planificaci�n de HLAction es [Action, HLAction], donde Action es
% una acci�n que acerca al agente al cumplimiento de HLAction. Cuando
% esa siguiente acci�n sea ejecutada y consumida, el agente vuelve a
% considerar y planificar HLAction, obteniendo la siguiente acci�n, y
% as� siguiendo.
%
% Esta estrategia es ideal para intenciones/acciones que no pueden
% planificarse por completo en un dado instante, resultando apropiado
% establecer en cada turno cual es la siguiente acci�n para lograrlas
% (por ejemplo, perseguir a un agente para saquearlo).
%
% Este plan recursivo se corta cuando los efectos de HLAction se logran
% (achieved(HLAction)), o cuando falla la planificaci�n de HLAction,
% reflejando que ya no existe plan para la misma.
%
% IMPORTANTE: Este predicado entregar� soluciones alternativas (por
% demanda), correspondientes a planes alternativos para la meta
% considerada. Analizar seg�n la acci�n que se est� planificando,
% si es deseable brindar soluciones alternativas.

% Planificaci�n para ir a descansar

planify(rest, Plan):- 
    Plan = [search_inn, stay].


% Planificaci�n recursiva. En este caso permite repetir indefinidamente una acci�n (noop)
% hasta que la intenci�n de alto nivel corriente (rest) sea lograda (achieved/1).
% Esto se hizo as� dado que resulta m�s simple que predecir de antemano cuantos turnos 
% exactamente debe permanecer el agente para recargarse por completo (n�tese que el agente
% podr�a incluso sufrir ataques mientras est� en la posada, siendo imposible planificar de
% antemano cuantos turnos debe permanecer en la posada para reponerse por completo)

planify(stay, [noop , stay]). 


% Planicaci�n para buscar una posada

planify(search_inn, Plan):-
% Esto deber�a funcionar, es el c�digo que usamos en la entrega anterior
    	findall(InnPos,
                (
                        at([inn,IName], InnPos),
                        node(InnPos, InnVector, _),
                        at([agent, me], MyPos),
                        node(MyPos, MyVector, _),
                        entity_descr([inn, IName], Propiedades),
                        member([forbidden_entry, EntradaProhibida], Propiedades),
                        distance(MyVector, InnVector, InnDist),
                        entrada_habilitada(EntradaProhibida, InnDist)
                ),
            	Posadas),
    	buscar_plan_desplazamiento(Posadas, Plan, _Destino).


% Planificaci�n para obtener item que esta en el suelo

% Caso en que el agente est� en la misma posici�n que el objeto
planify(get(Item),Plan):-
    	at(Item, ItPos),
    	at([agent, me], ItPos),
    	Plan = [pickup(Item)],!.

% Caso en que el agente tiene que ir hasta la posici�n en la que esta el objeto
planify(get(Item), Plan):- 
	at(Item, ItPos),
	Plan = [goto(ItPos), pickup(Item)].


% Planificaci�n para desplazarse a un destino dado

planify(goto(PosDest), Plan):- 
	buscar_plan_desplazamiento([PosDest], Plan, _MetaLograda), !. % Evita la b�squeda de soluciones alternativas para un plan de desplazamiento.


% Planificaci�n para sabotear una tumba

planify(break([grave, GrName]), Plan) :-
        has([agent, me], [potion, PtName]),
        at([grave, GrName], GrPos),
	Plan = [goto(GrPos), cast_spell(open([grave, GrName], [potion, PtName]))].


% Planificaciones de robo, acecho y ataque a otros agentes

planify(steal_agent([agent, AgName], ItemList), Plan) :-
        property([agent, AgName], life, St),
        St = 0,
        findall(get(Item), (member(Item, ItemList)), Actions),
        append([chase_agent([agent, AgName]), attack_agent([agent, AgName])], Actions, Plan).


planify(chase_agent([agent, AgName]), Plan) :-
        atPos([agent, AgName], AgPos),
        atPos([agent, me], Pos),
        not(pos_in_attack_range(Pos, AgPos)),
        node(AgPos, _, Connections),
        random_member(NextPos, Connections),
        Plan = [goto(NextPos), chase_agent([agent, AgName])].


planify(attack_agent([agent, AgName]), Plan) :-
        property([agent, me], life, St),
        St > 150,
        property([agent, AgName], life, AgSt),
        AgSt > 0,
        atPos([agent, me], Pos),
        atPos([agent, AgName], AgPos),
        AgName \= me,
        pos_in_attack_range(Pos, AgPos),
        Plan = [attack([agent, AgName]), attack_agent([agent, AgName])].


% Planificaci�n para explorar

planify(explore, Plan) :- 
	findall(        Pos,
                (
                        node(Pos,_,Connections),                        
                        not(visitados(Pos)),
                        not(interior(Connections))
                ),
                        SinExplorar
               
               ),
	SinExplorar \= [],
        retractall(ucs),
        assert(ucs :- true),
        % Se efectua una busqueda UCS con las metas obtenidas
        buscar_plan_desplazamiento(SinExplorar, Plan, _Destino),
	retractall(ucs),
        assert(ucs :- false).                 


% Planificaci�n para moverse aleatoriamente

planify(move_at_random, Plan):-
	findall(Node, node(Node, _, _), PossibleDestPos),
	random_member(DestPos, PossibleDestPos), % Selecciona aleatoriamente una posici�n destino.
				                 % <<<CHOICE POINT>>> (Posibilidad de backtracking)
	Plan = [goto(DestPos)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_primitive_action(+Plan, -NextAction, -RemainingPlan)
%
% Selecciona y retorna la siguiente acci�n primitiva del plan de alto
% nivel, adem�s del devolver el plan restante.
%
% Planteo Recursivo:
%
% Sea Plan = [A_1, A_2, ..., A_n]
%
% CB: Si A_1 es primitiva, entonces NextAction = A_1 y RemainingPlan =
% [A_2, ..., A_n].
%
% CR: Si A_1 es de alto nivel, se hallar� mediante planify/2 una
% secuencia de acciones de menor nivel [A_1.1, A_1.2, ..., A_1.k], o
% (sub)plan, para A_1, dando lugar a una versi�n refinada de Plan:
%
%          PlanRef = [A_1.1, A_1.2, ..., A_1.k, A_2, ..., A_n]
%
% Luego se obtiene recursivamente la siguinte acci�n, pero esta vez para
% PlanRef.
%
% CR 2: Los efectos de A_1 se cumplen en el estado actual del mundo.
% Luego se obtiene recursivamente la siguinte acci�n, pero esta vez para
% [A_2, ..., A_n].
%
% Observaci�n: A modo de mantener registro de la descomposici�n de
% acciones de alto nivel en subplanes, la estructura empleada por este
% predicado para representar planes incolucra el uso de "marcadores".
% Concretamente, en CR, PranRef = [A_1.1, A_1.2, ..., A_1.k, [A_1], A_2,
% ..., A_n] donde [A_1] es un marcador indiciando que las acciones que
% se encuentran a la izquierda corresponden al sub-plan para lograr A_1.
% Luego, el prop�sito del predicado remove_executed_ancestors/2 empleado
% en CB y CR 2 es eliminar los marcadores de acciones de alto nivel
% cuyo sub-plan asociado fue ejecutado por completo.


% CR 2:

next_primitive_action([Action | RestOfPlan], NextAction, RemainingPlan):-
	% Este caso permite, por ejemplo, terminar exitosamente un programa para una HLAction
	% cuando �sta ya fue lograda (debe especificarse achieved/1 para HLAction).

	clause(achieved(Action), _), % Existe especificaci�n de cu�ndo Action se considera lograda.

	achieved(Action), % Action fue lograda.
	!,
	write('Action '), write(Action), write(' achieved.'),nl,
	remove_executed_ancestors(RestOfPlan, CleanRestOfPlan),
	next_primitive_action(CleanRestOfPlan, NextAction, RemainingPlan).


% CB:

next_primitive_action([Action | RemainingPlan], Action, CleanRemainingPlan):-
	primitive(Action),
	remove_executed_ancestors(RemainingPlan, CleanRemainingPlan),
	!.

% CR:

next_primitive_action([HLAction | RestOfPlan], Action, RemainingPlan):-


        if_fails_do(

	clause(planify(HLAction, _SubPlan), _), % Planificaci�n definida para HLAction.

		    throw_exception((
			  write(HLAction),
			  write(' is undefined. Declare it as primitive or planify it.')
			 ))
		   ),
        !,

	(

	     planify(HLAction, SubPlan)	 % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	     ;

	     (write('Planning for '), write(HLAction), write(' failed.'), nl, fail)
	                                 % Si definitivamente no encuentra plan para la intenci�n seleccionada,
	                                 % luego de buscar planes alternativos (backtracking sobre planify(HLAction, SubPlan)),
				         % selecciona otra intenci�n mediante backtracking en deliberate/0.

	),

	(   last_element(HLAction, SubPlan),
	    append(SubPlan, RestOfPlan, LowerLevelPlan) % Se evita introducir el marcador de super-accion
							% si la acci�n de alto nivel coincide con la �ltima del subplan.
	;
	    append(SubPlan, [[HLAction]|RestOfPlan], LowerLevelPlan)
	),

	%'High-level action ' HLAction ' expanded into '
	%write(HLAction), write(' -- expanded into -> '), write(SubPlan),nl,
	writeln('          -- expanded into -> '), nl,
	write(LowerLevelPlan), nl, nl,

	next_primitive_action(LowerLevelPlan, Action, RemainingPlan).

% Observaci�n: si en particular Subplan es [] (esto puede
% ocurrir cuando los efectos de HLAction ya valen en
% el estado actual del mundo) entonces ejecuta la siguiente acci�n de
% RestOfPlan.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% remove_executed_ancestors(+Plan, -CleanPlan)
%
%

remove_executed_ancestors([[_]| Rest], Clean):-
	!,
	remove_executed_ancestors(Rest, Clean).

remove_executed_ancestors(Clean, Clean).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% primitive(ActionName).
%
% Especifica las acciones primitivas del agente, es decir, aquellas que
% no pueden descomponerse en acciones m�s b�sicas.

primitive(move(_)).
primitive(pickup(_)).
primitive(drop(_)).
primitive(attack(_)).
primitive(cast_spell(_)).
primitive(noop).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% feasible(+Plan).
%
% Determina si el plan jer�rquico Plan es factible de acuerdo a las
% creencias actuales del agente.

feasible(Plan):-
	dynamic_state_rels(Init),
	project(Plan, Init, _Finish).
	% El plan puede ejecutarse con �xito a partir del estado actual. Si alguna de las precondiciones de las
        % acciones del plan ya no se satisfacen (por ejemplo, el tesoro que voy a juntar ya no se encuentra m�s
        % en la posici�n que recordaba), entonces project/3 fallar�, reflejando que el plan no es factible.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Auxiliares

% entrada_habilitada(+ListaNegra, +Distancia)
% Determina si la entrada del agente a una posada esta prohibida
%
% +ListaNegra - Lista de agentes que tienen prohibida la entrada
% a la posada
% +Distancia - Distancia del el agente a la posada
% La entrada esta habilitada - Lista negra de agentes vacia

entrada_habilitada([],_) :- !.

% La entrada esta habilitada - No formo parte de la lista negra

entrada_habilitada(EntradaProhibida,_Distancia) :-
        not(member([me,_ForbiddenUntil],EntradaProhibida)), !.

% La entrada esta inhabilitada - Formo parte de la lista negra
% Se calcula si pasado el tiempo tomado para llegar hacia alli
% tendre nuevamente la entrada habilitada

entrada_habilitada(EntradaProhibida,Distancia) :-
        member([me,ForbiddenUntil],EntradaProhibida),
        time(T),
        T >= ForbiddenUntil,
        TiempoRestante is ForbiddenUntil - T,
        Distancia >= TiempoRestante.


% interior(+Connections)
% Determina si los vecinos de una posicion dada se encuentran en el radio
% de vision del agente. No se exploran posiciones que se sean visibles para el agente,
% de este modo se mejora la eficiencia de la exploracion.
%
% +Connections - Lista de posiciones adyacentes a una posicion dada.

interior([]).

interior([[Ady,_]|Connections]) :- node(Ady,_,_), interior(Connections).


% buscar_deseo_cercano(-Posiciones, +Deseos)
% Determina un plan de desplazamiento desde la posicion actual hasta el item
% (oro o pocion) o tumba que se encuentre mas cercano.
%
% -Posiciones - Posicion del item o tumba mas cercana
% +Deseos - Deseos actuales del agente

buscar_deseo_cercano(Posiciones, Deseos) :-
	findall(GrPos, (member(break([grave, GrName]), Deseos), at([grave, GrName], GrPos)), GravePositions),	
	findall(ItPos,(member(get([Item, ItName]), Deseos), (Item = gold; Item = potion), at([Item, ItName], ItPos)), ItemPositions),
	append(GravePositions, ItemPositions, Positions),
	buscar_plan_desplazamiento(Positions, _Plan, Posiciones), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%        AGENT SETUP       %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%      & REGISTRATION      %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic name/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag
%
% Solicita la registraci�n al juego, y recuerda su nombre.


start_ag:- AgName = dumptrooper,
           agent_init(AgName),
           assert(ag_name(AgName)),
	   agent_reset,
           connect,
           run,
           disconnect.

s:- start_ag.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% start_ag_instance(+InstanceID)

%
% Solicita la registraci�n al juego de una instancia, y recuerda su
% nombre, que ser� el de la versi�n original seguido del InstanceID
% entre par�ntesis.


start_ag_instance(InstanceID):-
                    AgClassName = dumbtrooper,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.


si(InstanceID):- start_ag_instance(InstanceID).
