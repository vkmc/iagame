%% Player-Agent BDI
%

:- [disable_writes].

:- [ag_primitives, module_beliefs_update, module_actions_rep_and_projection, module_path_finding, extras_for_agents].

:- consult(extras_meta_preds).

:- dynamic intention/1, plan/1, ucs/0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%           AGENT	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%       EXECUTION CYCLE	   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%                          %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


run:-
      get_percept(Percept),

      ag_name(_AgName),

      %tell('log.txt'),

      update_beliefs(Percept),

      %display_ag, nl, !,

      deliberate,

      % FUE IMPLEMENTADO DE MANERA QUE SI POSTERIORMENTE FALLA LA OBTENCIÓN DE UN PLAN PARA LA INTENCIÓN
      % EN PRINCIPIO SELECCIONADA, VUELVE A RECONSIDERAR INTENCIÓN.

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
% Se encuentra definido en el módulo 'module_beliefs_update'.

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
% El agente analiza si continuará con su intención actual, considerando
% deseos de alta prioridad, la factibilidad del plan
% para la intencion actual, si la intención actual fue lograda, etc.
% En caso de no continuar con la intención corriente, establece cual
% será la nueva intención analizando los deseos existentes y
% seleccionando uno de ellos.

deliberate:-

	high_priority(HPDesire, Explanation),	 % Si existe un deseo HPDesire de alta prioridad:
						 % ( <<<CHOICE POINT>>> -- Posibilidad de backtracking )

	not(intention(HPDesire)),        % y no es el caso que coincide con la intención actual,

	write('High-priority Desire: '), write(HPDesire), write(', since '), writeln(Explanation), nl,

	retractall(intention(_)),     % (Estratégicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	      %	 ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                              %  para la anterior), la intención anterior se elimina y se hace assert de la nueva).

	assert(intention(HPDesire)),     % se establece HPDesire como intención actual.
	assert(plan([HPDesire])).

deliberate:-       % Si

	(   not(intention(_)),                     % actualmente no hay intención
	    writeln('There is no intention '), nl
	;                                          % o
	    intention(Int),
	    achieved(Int),                         % la intención corriente fue lograda
	    write('Intention '), write(Int), writeln(' achieved.')
	;					   % o

	    plan([]),                              % el plan para para la intención actual fue consumido
	    writeln('Plan consumed.')
	;                                          % o
	    (

	        plan(Plan), Plan \= [], not(feasible(Plan))   % el plan corriente se tornó no factible, o

		;

	        not(plan(_))                                  % no hay plan. Esto ocurre si se descubre que el plan actual es no
	                                                      % factible al intentar obtener, sin éxito, el (sub) plan para la
	                                                      % siguiente acción de alto nivel (falla el next_primitive_action).
	    ),
	    writeln('Current plan became infeasible.'), nl
	),

	!,

	findall(Desire, desire(Desire, _Explanation), Desires),  % Calcular el conjunto de deseos
	write('Desires: '), writeln(Desires),nl,
	select_intention(NewInt, NewExpl, Desires),   % Seleccionar una intención
	                                              % (deseo que el agente se compromete a lograr)
	                                              % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	write('New Intention: '), write(NewInt), write(', since '), writeln(NewExpl), nl,

	retractall(intention(_)),  % (Estrategicamente colocado luego del "choice point", para que,
	retractall(plan(_)),	   % ante la búsqueda de una intención alternativa (por no haberse encontrado un plan
	                           % para la anterior), la intención anterior se elimina y se asserta la nueva.)

	assert(intention(NewInt)),                    % Recordar la nueva intención seleccionada.
	assert(plan([NewInt])).


deliberate:-
	intention(Int),
	write('Current Intention: '), writeln(Int), nl.
	% Caso contrario, se continúa con la intención y plan corrientes


deliberate:-            % Si llega acá significa que falló el next_primitive_action al planificar la siguiente acción
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
% del agente). Asociado al deseo se retorna una explicación,
% especificando las razones por las cuales Desire se considera
% actualmente un deseo. En su forma más básica Explanation puede ser un
% string con una descripción narrada (breve) de dichas razones,
% que luego puede ser impresa por pantalla.

% Deseo robar a otro agente
%
% Si recuerdo que vi un agente, ir a robarle es una meta

desire(steal_agent([agent, AgName], _ItemList), 'Me encantaria pungear a un agente...') :-
        at([agent, AgName], _Pos), AgName \= me.

% Deseo agarrar oro o pocion
%
% Si recuerdo que hay oro o una pocion en el piso, agarrar ese oro o pocion
% es una meta

desire(get([Item, ItName]), 'Quiero agarrar oro/pociones!') :-
        (Item = potion; Item = gold),
        at([Item, ItName], _PosIt).
		

% Deseo saquear una tumba
%
% Si me encuentro con una tumba, deseo saquearla

desire(break([grave, GrName], TrList), "Quiero profanar una tumba") :-
        writeln('DESEO: PROFANAR UNA TUMBA'),
	writeln('DESEO: PROFANAR UNA TUMBA'),
	writeln('DESEO: PROFANAR UNA TUMBA'),
	at([grave, GrName], _PosGr),
        has([agent, me], [potion,_]),
		writeln('TENGO UNA POCION'),
        findall(Gold, (has(grave, Gold)), TrList),
		writeln('DESEO DE PROFANAR TUMBA NO FALLIDO.').

% Deseo descansar
%
% Si la vida del agente es menor a 100, desea descansar

desire(rest, 'Quiero descansar'):-
	property([agent, me], life, St),
	St < 100.

% Deseo explorar
%
% Si el agente no tiene algo mas importante que hacer, explora el mapa

desire(explore, 'Quiero explorar'):-
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

desire(move_at_random, 'Quiero estar siempre en movimiento!').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% high_priority(-HPDesire, Explanation).
%
% Determina si existe un deseo HPDesire de alta prioridad, es
% decir, tal que implica abandonar la intención actual. En caso de
% existir tal deseo de alta prioridad, lo retorna.
%
% Análogamente al predicado desire/2, asociado al deseo HPDesire de alta
% prioridad se retorna una explicación, especificando las
% razones por las cuales HPDesire se considera un deseo que
% debe adoptarse inmediatamente como intención.
% En su forma más básica Explanation puede ser un string con una
% descripción narrada (breve) de dichas razones, que luego puede ser
% impresa por pantalla.
:- dynamic high_priority/2.

% Deseo de alta prioridad - Descansar

high_priority(rest, 'Necesito descansar'):-
	property([agent, me], life, St),
	St < 70, % running low of life...
	once(at([inn, _HName], _Pos)). % se conoce al menos una posada


% Deseo de alta prioridad - Devolver ataque

high_priority(attack_agent([agent, AgName], ItemList), 'Me atacan! Devolviendo ataque...') :-
        property([agent, me], harmed_by, Attackers),
        Attackers \= [],
        member([agent, AgName], Attackers),
        findall(Item, (has([agent, AgName], Item)), ItemList).

% Deseo de alta prioridad - Robarle a otro agente

high_priority(steal_agent([agent, AgName], ItemList), 'Me cruce un agente, voy a robarle!') :-
        at([agemt, AgName], _),
        AgName \= me,
        findall(Item, (has([agent, AgName], Item)), ItemList),
        ItemList \= [].

% Deseo de alta prioridad - Agarrar un item

high_priority(get([Item, ItName]), 'Encontre un item, voy a agarrarlo...') :-
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
% Selecciona uno de los deseos corrientes del agente como intención.
%
% En su forma más básica, define un orden de prioridad entre los deseos
% del agente, seleccionando como intención al primero en este orden
% de prioridad.
%
% Tiene múltiples soluciones. Esto es adrede: si
% posteriormente a la selección de una intención (por parte de este
% predicado) no se encuentra plan para alcanzarla, entonces se obtendrá
% la siguiente intención (mediante backtracking), hasta encontrar una
% viable, o agotarlas a todas.


% Seleccionar intencion - Descansar
%
% Dado que el nivel de stamina es relativamente bajo, se decide ir
% descansar antes de abordar otro deseo.

select_intention(rest, 'Voy a descansar antes de encarar otro deseo', Desires):-
	member(rest, Desires),
	property([agent, me], life, St),
	St < 100.


% Seleccionar intencion - Saquear una tumba

select_intention(break([grave, GrName1], ItemList), 'Voy a saquear una tumba', Desires) :-
		writeln('SELECT INTENTION: PROFANAR UNA TUMBA'),
        findall(GrPos, (member(break([grave, GrName2], ItemList2), Desires), at([grave, GrName2], GrPos)), Positions),
        buscar_plan_desplazamiento(Positions, _Plan, Closer),
        member(break([grave, GrName1], ItemList2), Desires),
        at([grave, GrName1], Closer),
        findall(Item, (has([grave, GrName1], Item)), ItemList),
		writeln('SELECT INTENTION NO FALLIDO: PROFANAR UNA TUMBA').

% Seleccionar intencion - Robarle a otro agente
select_intention(steal_agent([agent, AgName], ItemList), 'Voy a robarle a otro agente', _Desires) :-
        at([agent, AgName], _AgPos),
        AgName \= me,
        findall(Item, (has([agent, AgName], Item)), ItemList).

% Seleccionar intencion - Agarrar item
%
% De todos los posibles objetos tirados en el suelo que el agente desea tener,
% selecciono como intención obtener aquel que se encuentra más cerca.

select_intention(get([Item, ItName]), 'Item más cercano de los que deseo obtener', Desires) :-
        at([agent, me], Pos),
        at([Item, ItName], Pos),
        member(get([Item, ItName]), Desires).

select_intention(get([Item, ItName]), 'Item más cercano de los que deseo obtener', Desires):-
	findall(ItPos,
				(member(get([Item, ItName]), Desires),
                (Item = gold; Item = potion),
				at([Item, ItName], ItPos)),
		Goals), % Obtengo posiciones de todos los objetos meta tirados en el suelo.
	buscar_plan_desplazamiento(Goals, _Plan, Closer),
	member(get([Item, ItName]), Desires),
        at([Item, ItName], Closer).


% Seleccionar intencion - Explorar		
select_intention(explore, 'Voy a explorar! A ver que encuentro...', Desires) :-
	member(explore, Desires).

% Seleccionar intencion - Descansar porque no hay otras metas
%
% Si no existen objetos que deseen obtenerse, y existe el deseo de
% descansar (stamina por debajo de 100), se decide ir a descansar.

select_intention(rest, 'No tengo otra cosa más interesante que hacer, asique me voy a dormir', Desires):-
	member(rest, Desires).

% Seleccionar intencion - Moverse aleatoriamente

select_intention(move_at_random, 'No tengo otra cosa más interesante que hacer, asi que voy a caminar', Desires) :-
        member(move_at_random, Desires).


% TODO Atacar agente?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% achieved(+Intention).
%
% Determina si la intención Intention fue alcanzada. Esto es, si
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

achieved(break([grave, _GrName]), ItemList) :-
        findall(Item, (has([agent, me], Item)), AgentBackpack),
        subset(ItemList, AgentBackpack),
	writeln('PUDE SAQUEAR UNA TUMBA').

% Logrado si el agente tiene todos los items que tenia otro agente

achieved(steal_agent([agent, _AgName], AgItemList)) :-
        findall(Item, (has([agent, me], Item)), ItemList),
        subset(ItemList, AgItemList).

% Logrado si el agente esta en nuestro rango de ataque

achieved(chase_agent([agent, AgName])) :-
        atPos([agent, AgName], AgPos),
        atPos([agent, me], Pos),
        pos_in_attack_range(Pos, AgPos).

% Logrado si el agente esta inconsiente o fuera del rango de ataque

achieved(attack_agent([agent, AgName])) :-
                property([agent, AgName], life, St),
                St = 0,
                atPos([agent, me], Pos),
                atPos([agent, AgName], AgPos),
                findall(InRange, pos_in_attack_range(Pos, InRange), Range),
                not(member(AgPos, Range)).

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
% Obtiene la siguiente acción primitiva Action correspondiente al plan
% actual, removiéndola del plan. Nótese que la siguiente acción
% del plan podría ser de alto nivel (no primitiva), en cuyo caso debe
% planificarse hasta llegar al nivel de acciones primitivas.
% (Ver next_primitive_action/3).

planning_and_execution(Action):-

	retract(plan(Plan)),      % Ejecutar siguiente acción del plan.

	write('Following plan: '), writeln(Plan), nl,
	next_primitive_action(Plan, Action, RestOfPlan),
	write('Next action: '), writeln(Action),
	assert(plan(RestOfPlan)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% planify(+HLAction, -Plan)
%
% Define una librería de Planes, es decir, un mapeo de acciones de alto
% nivel (en particular, intenciones) a planes involucrando acciones de
% menor nivel.
%
% Dada una acción HLAction de alto nivel, retorna un plan (involucrando
% acciones de menor nivel) cuya ejecución equivalga al efecto de la
% acción HLAction.
%
% Debe definirse una regla de este predicado por cada acción de alto
% nivel considerada por el agente (incluída las intenciones, que
% constituyen las acciones de más alto nivel).
%
% La planificación de HLAction puede realizarse,
% según el tipo de la acción, de las siguientes maneras:
%
%   a) simplemente especificando en forma "estática" la secuencia
%      (lista) de acciones de menor nivel cuyo efecto equivale al de
%       HLAction.
%
%   b) empleando un algoritmo de búsqueda (por ejemplo el implementado
%      para la etapa 2, permitiendo planificar el desplazamiento a una
%      posición determinada)
%
%   c) empleando el algoritmo de planeamiento STRIPS (cuya
%      implementación es provista por la cátedra), adecuado para
%      realizar planificaciones de metas más complejas, como obtener un
%      tesoro que se encuentra en una tumba.
%
%
% La opción a admite la especificación de planes recursivos, donde en
% particular, la última acción del plan para HLAction es la propia
% HLAction. Esto permite, por ejemplo, especificar que la
% planificación de HLAction es [Action, HLAction], donde Action es
% una acción que acerca al agente al cumplimiento de HLAction. Cuando
% esa siguiente acción sea ejecutada y consumida, el agente vuelve a
% considerar y planificar HLAction, obteniendo la siguiente acción, y
% así siguiendo.
%
% Esta estrategia es ideal para intenciones/acciones que no pueden
% planificarse por completo en un dado instante, resultando apropiado
% establecer en cada turno cual es la siguiente acción para lograrlas
% (por ejemplo, perseguir a un agente para saquearlo).
%
% Este plan recursivo se corta cuando los efectos de HLAction se logran
% (achieved(HLAction)), o cuando falla la planificación de HLAction,
% reflejando que ya no existe plan para la misma.
%
% IMPORTANTE: Este predicado entregará soluciones alternativas (por
% demanda), correspondientes a planes alternativos para la meta
% considerada. Analizar según la acción que se esté planificando,
% si es deseable brindar soluciones alternativas.

% Planificación para ir a descansar

planify(rest, Plan):- 
    Plan = [search_inn, stay].

% Planificación recursiva. En este caso permite repetir indefinidamente una acción (noop) hasta que la intención de alto nivel corriente (rest) sea lograda (achieved/1). Esto se hizo así dado que resulta más simple que predecir de antemano cuantos turnos exactamente debe permanecer el agente para recargarse por completo (nótese que el agente podría incluso sufrir ataques mientras está en la posada, siendo imposible planificar de antemano cuantos turnos debe permanecer en la posada para reponerse por completo)

planify(stay, [noop , stay]). 

% Planicación para buscar una posada

planify(search_inn, Plan):-
% Esto debería funcionar, es el código que usamos en la entrega anterior
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
    buscar_plan_desplazamiento(Posadas, _, Destino),
    Plan = [goto(Destino)].

% Planificación para obtener item que esta en el suelo

% Caso en que el agente está en la misma posición que el objeto
planify(get(Item),Plan):-
    at(Item, ItPos),
    at([agent, me], ItPos),
    Plan = [pickup(Item)],!.

% Caso en que el agente tiene que ir hasta la posición en la que esta el objeto
planify(get(Item), Plan):- 
	at(Item, ItPos),
	Plan = [goto(ItPos), pickup(Item)].

	
% Planificación para desplazarse a un destino dado

planify(goto(PosDest), Plan):- 
	buscar_plan_desplazamiento([PosDest], Plan, _MetaLograda),
	!. % Evita la búsqueda de soluciones alternativas para un plan de desplazamiento.


%planify(rest, Plan):- % Planificación para desplazarse a un destino dado

	%at([inn, _H], PosH),  % Selecciona una posada para ir a descansar.
				 % <<<CHOICE POINT>>> (Posibilidad de backtracking)

	%Plan = [goto(PosH), stay].

% Planificación para sabotear una tumba

planify(break([grave, GrName], _ItemList), Plan) :-
        has([agent, me], [potion, PtName]),
        at([grave, GrName], GrPos),
	Plan = [goto(GrPos), cast_spell(open([grave, GrName], [potion, PtName]))].


% Planificaciones de robo, acecho y ataque a otros agentes

planify(steal_agent([agent, AgName], ItemList), Plan) :-
        property([agent, AgName], life, St),
        St = 0,
        findall(get(Item), (member(Item, ItemList)), Actions).
        %append([chase_agent([agent, AgName]), attack_agent([agent, AgName])], Actions, Plan).

planify(chase_agent([agent, AgName]), Plan) :-
        atPos([agent, AgName], AgPos),
        atPos([agent, me], Pos),
        not(pos_in_attack_range(Pos, AgPos)),
        node(AgPos, _, Connections),
        random_member(NextPos, Connections),
        writeln('Entre en el POS IN ATTACK RANGE - CHASE'),
        writeln('Entre en el POS IN ATTACK RANGE - CHASE'),
        Plan = [goto(NextPos), chase_agent([agent, AgName])].

planify(attack_agent([agent, AgName]), Plan) :-
        property([agent, AgName], life, St),
        St > 0,
        atPos([agent, me], Pos),
        atPos([agent, AgName], AgPos),
        atPos([inn, _InnName], InnPos),
        Pos \= InnPos,
        pos_in_attack_range(Pos, AgPos),
        writeln('Entre en el POS IN ATTACK RANGE - ATTACK'),
        writeln('Entre en el POS IN ATTACK RANGE - ATTACK'),
        Plan = [attack([agent, AgName]), attack_agent([agent, AgName])].

% Planificación para explorar

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

% Planificación para moverse aleatoriamente

planify(move_at_random, Plan):-
	findall(Node, node(Node, _, _), PossibleDestPos),

	random_member(DestPos, PossibleDestPos), % Selecciona aleatoriamente una posición destino.
				                 % <<<CHOICE POINT>>> (Posibilidad de backtracking)
	Plan = [goto(DestPos)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% next_primitive_action(+Plan, -NextAction, -RemainingPlan)
%
% Selecciona y retorna la siguiente acción primitiva del plan de alto
% nivel, además del devolver el plan restante.
%
% Planteo Recursivo:
%
% Sea Plan = [A_1, A_2, ..., A_n]
%
% CB: Si A_1 es primitiva, entonces NextAction = A_1 y RemainingPlan =
% [A_2, ..., A_n].
%
% CR: Si A_1 es de alto nivel, se hallará mediante planify/2 una
% secuencia de acciones de menor nivel [A_1.1, A_1.2, ..., A_1.k], o
% (sub)plan, para A_1, dando lugar a una versión refinada de Plan:
%
%          PlanRef = [A_1.1, A_1.2, ..., A_1.k, A_2, ..., A_n]
%
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% PlanRef.
%
% CR 2: Los efectos de A_1 se cumplen en el estado actual del mundo.
% Luego se obtiene recursivamente la siguinte acción, pero esta vez para
% [A_2, ..., A_n].
%
% Observación: A modo de mantener registro de la descomposición de
% acciones de alto nivel en subplanes, la estructura empleada por este
% predicado para representar planes incolucra el uso de "marcadores".
% Concretamente, en CR, PranRef = [A_1.1, A_1.2, ..., A_1.k, [A_1], A_2,
% ..., A_n] donde [A_1] es un marcador indiciando que las acciones que
% se encuentran a la izquierda corresponden al sub-plan para lograr A_1.
% Luego, el propósito del predicado remove_executed_ancestors/2 empleado
% en CB y CR 2 es eliminar los marcadores de acciones de alto nivel
% cuyo sub-plan asociado fue ejecutado por completo.


% CR 2:

next_primitive_action([Action | RestOfPlan], NextAction, RemainingPlan):-
	% Este caso permite, por ejemplo, terminar exitosamente un programa para una HLAction
	% cuando ésta ya fue lograda (debe especificarse achieved/1 para HLAction).

	clause(achieved(Action), _), % Existe especificación de cuándo Action se considera lograda.

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

	clause(planify(HLAction, _SubPlan), _), % Planificación definida para HLAction.

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
	                                 % Si definitivamente no encuentra plan para la intención seleccionada,
	                                 % luego de buscar planes alternativos (backtracking sobre planify(HLAction, SubPlan)),
				         % selecciona otra intención mediante backtracking en deliberate/0.

	),

	(   last_element(HLAction, SubPlan),
	    append(SubPlan, RestOfPlan, LowerLevelPlan) % Se evita introducir el marcador de super-accion
							% si la acción de alto nivel coincide con la última del subplan.
	;
	    append(SubPlan, [[HLAction]|RestOfPlan], LowerLevelPlan)
	),

	%'High-level action ' HLAction ' expanded into '
	%write(HLAction), write(' -- expanded into -> '), write(SubPlan),nl,
	writeln('          -- expanded into -> '), nl,
	write(LowerLevelPlan), nl, nl,

	next_primitive_action(LowerLevelPlan, Action, RemainingPlan).

% Observación: si en particular Subplan es [] (esto puede
% ocurrir cuando los efectos de HLAction ya valen en
% el estado actual del mundo) entonces ejecuta la siguiente acción de
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
% no pueden descomponerse en acciones más básicas.

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
% Determina si el plan jerárquico Plan es factible de acuerdo a las
% creencias actuales del agente.

feasible(Plan):-
	dynamic_state_rels(Init),
	project(Plan, Init, _Finish).
	% El plan puede ejecutarse con éxito a partir del estado actual. Si alguna de las precondiciones de las
        % acciones del plan ya no se satisfacen (por ejemplo, el tesoro que voy a juntar ya no se encuentra más
        % en la posición que recordaba), entonces project/3 fallará, reflejando que el plan no es factible.

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
% Solicita la registración al juego, y recuerda su nombre.


start_ag:- AgName = agentBDI,
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
% Solicita la registración al juego de una instancia, y recuerda su
% nombre, que será el de la versión original seguido del InstanceID
% entre paréntesis.


start_ag_instance(InstanceID):-
                    AgClassName = agentBDI,
                    AgInstanceName =.. [AgClassName, InstanceID],
		    agent_init(AgInstanceName),
		    assert(ag_name(AgInstanceName)),
		    agent_reset,
		    connect,
		    run,
		    disconnect.

si(InstanceID):- start_ag_instance(InstanceID).
