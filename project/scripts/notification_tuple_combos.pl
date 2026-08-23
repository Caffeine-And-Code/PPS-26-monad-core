notification_type(info).
notification_type(error).
notification_type(success).

notification_pair(First, Second) :-
    notification_type(First),
    notification_type(Second).

capitalize(info, 'Info').
capitalize(error, 'Error').
capitalize(success, 'Success').

generate :-
    findall(First-Second, notification_pair(First, Second), Pairs),
    forall(member(F-S, Pairs), (
        capitalize(F,FCap),
        capitalize(S,SCap),
        format("(~w, ~w),~n", [FCap, SCap]))
    ).

:- initialization(generate).