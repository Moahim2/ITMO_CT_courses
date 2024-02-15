%%%%%%%%%%%%%%%%
% Учебный план %
%%%%%%%%%%%%%%%%

% База данных фактов

% Дисциплины осеннего семестра первого курса
course(1, fall, 'Прог').
course(1, fall, 'АиСД').
course(1, fall, 'Дискретка').
course(1, fall, 'АрхЭВМ').
course(1, fall, 'МатАн').
course(1, fall, 'ЦифрКульт').
course(1, fall, 'ЛинАл').
course(1, fall, 'ИнЯз').

% Дисциплины весеннего семестра первого курса
course(1, spring, 'Прог').
course(1, spring, 'АиСД').
course(1, spring, 'Дискретка').
course(1, spring, 'С++').
course(1, spring, 'МатАн').
course(1, spring, 'ЛинАл').
course(1, spring, 'ИнЯз').

% Дисциплины осеннего семестра второго курса
course(2, fall, 'МатАн').
course(2, fall, 'АиСД').
course(2, fall, 'Дискретка').
course(2, fall, 'ОС').
course(2, fall, 'С++').
course(2, fall, 'Web').
course(2, fall, 'Kotlin').
course(2, fall, 'Android').
course(2, fall, 'iOS').
course(2, fall, 'ИнЯз').
course(2, fall, 'БЖД').
course(2, fall, 'История').

% Дисциплины весеннего семестра второго курса
course(2, spring, 'МатАн').
course(2, spring, 'АиСД').
course(2, spring, 'Дискретка').
course(2, spring, 'ТеорВер').
course(2, spring, 'МатЛог').
course(2, spring, 'МетОпт').
course(2, spring, 'Java').
'course'(2, spring, 'ИнЯз').

% Запросы фактов

/*
% Читается ли программирование весной первого курса.
?- course(1, spring, 'Прог').
   yes
% Читается ли теория вероятностей весной первого курса.
?- course(1, spring, 'ТеорВер').
   no
% Дисциплины весеннего семестра первого курса
?- course(1, spring, X).
   X / 'Прог'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'С++'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
% Дисциплины первого курса
?- course(1, _, X).
   X / 'Прог'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'АрхЭВМ'
   X / 'МатАн'
   X / 'ЦифрКульт'
   X / 'ЛинАл'
   X / 'ИнЯз'
   X / 'Прог'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'С++'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
% Дисциплины весеннего семестра
?- course(_, spring, X).
   X / 'Прог'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'С++'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
   X / 'МатАн'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'ТеорВер'
   X / 'МатЛог'
   X / 'МетОпт'
   X / 'Java'
   X / 'ИнЯз'
% Когда читается Программирование
?- course(Y, S, 'Прог').
   Y / 1, S / fall
   Y / 1, S / spring
% Дисциплины читаемые в обоих семестрах первого курса
?- course(1, fall, X), course(1, spring, X).
   X / 'Прог'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
% Дисциплины, читаемые во втором или третьем семестрах.
?- course(1, spring, X); course(2, fall, X).
   X / 'Прог'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'С++'
   X / 'МатАн'
   X / 'ЛинАл'
   X / 'ИнЯз'
   X / 'МатАн'
   X / 'АиСД'
   X / 'Дискретка'
   X / 'ОС'
   X / 'С++'
   X / 'Web'
   X / 'Kotlin'
   X / 'Android'
   X / 'iOS'
   X / 'ИнЯз'
   X / 'БЖД'
   X / 'История'
% Дисциплины, читаемые во втором, но не третьем семестрах.
?- course(1, spring, X), \+ course(2, fall, X).
   X / 'Прог'
*/

% Простое правило
fall(Y, C) :- course(Y, fall, C).
/*
?- fall(X, 'МатАн').
   X / 1
   X / 2
?- fall(X, 'Прог').
   X / 1
*/


% Составное правило (конъюнкция)
repeated(C) :- course(Y1, S1, C), course(Y2, S2, C), (Y1, S1) \= (Y2, S2).
/*
?- repeated(C).
?- setof(X, repeated(X), XS).
   XS / ['АиСД','Дискретка','ИнЯз','ЛинАл','МатАн','Прог','С++']
*/

% Альтернативная запись предикатов
repeated1(C) :- course(Y1, S1, C), course(Y2, S2, C), \=((Y1, S1), (Y2, S2)).
repeated2(C) :- course(Y1, S1, C), ,(course(Y2, S2, C), \=((Y1, S1), (Y2, S2))).
repeated3(C) :- ,(course(Y1, S1, C), ,(course(Y2, S2, C), \=((Y1, S1), (Y2, S2)))).
/*
?- setof(X, repeated1(X), XS).
   XS / ['АиСД','Дискретка','ИнЯз','ЛинАл','МатАн','Прог','С++'], 
?- setof(X, repeated2(X), XS).
   XS / ['АиСД','Дискретка','ИнЯз','ЛинАл','МатАн','Прог','С++'], 
?- setof(X, repeated3(X), XS).
   XS / ['АиСД','Дискретка','ИнЯз','ЛинАл','МатАн','Прог','С++'], 
*/

% Составное правило (дизъюнкция)
next(Y, fall, Y, spring).
next(Y, spring, Y1, fall) :- Y1 is Y + 1.
%next(Y, spring, Y1, fall) :- is(Y1, +(Y, 1)).
/*
?- next(2, fall, Y, S).
   Y / 2, S / spring
?- next(1, spring, Y, S).
   Y / 2, S / fall
*/

repeated_next(Y, S, C) :- course(Y, S, C), next(Y, S, NY, NS), course(NY, NS, C).
/*
?- repeated_next(Y, S, 'МатАн').
   Y / 1, S / fall
   Y / 1, S / spring
   Y / 2, S / fall
?- repeated_next(1, fall, C).
   C / 'Прог'
   C / 'АиСД'
   C / 'Дискретка'
   C / 'МатАн'
   C / 'ЛинАл'
   C / 'ИнЯз'
*/

% Ориентированный граф
base(BY, BS, C, DY, DS, C) :- course(BY, BS, C), course(DY, DS, C), next(BY, BS, DY, DS).
base(1, spring, 'МатАн', 2, fall, 'ТеорВер').
base(1, spring, 'Прог', 2, fall, 'Web').
base(1, spring, 'Прог', 2, fall, 'МатЛог').
base(1, fall, 'АрхЭВМ', 1, spring, 'C++').
/*
?- base(1, fall, 'МатАн', Y, S, C).
   Y / 1, S / spring,  C / 'МатАн'
?- base(1, spring, 'МатАн', Y, S, C).
   Y / 2, S / fall, C / 'МатАн'
   Y / 2, S / fall, C / 'ТеорВер'
*/

% Рекурсивный поиск достижимых вершин
base_rec(BY, BS, BC, DY, DS, DC) :- base(BY, BS, BC, DY, DS, DC).
base_rec(BY, BS, BC, DY, DS, DC) :- base(BY, BS, BC, IY, IS, IC), base_rec(IY, IS, IC, DY, DS, DC).
/*
?- base_rec(1, fall, 'МатАн', Y, S, C).
   Y / 1, S / spring, C / 'МатАн'
   Y / 2, S / fall,   C / 'МатАн'
   Y / 2, S / fall,   C / 'ТеорВер'
   Y / 2, S / spring, C / 'МатАн'
*/

% Последний семестр дисциплины (отрицание).
last(Y, S, C) :- course(Y, S, C), \+ repeated_next(Y, S, C).
/*
?- last(Y, S, 'МатАн').
   Y / 2, S / spring
?- last(1, spring, C).
   C / 'Прог'
   C / 'ЛинАл'
*/
