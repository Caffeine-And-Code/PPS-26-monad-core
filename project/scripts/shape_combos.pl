maybe_value(some('4')).
maybe_value(none).

maybe_value_width(some('5')).
maybe_value_width(none).

maybe_value_height(some('6')).
maybe_value_height(none).

shape_field_combo(Radius, Width, Height) :-
    maybe_value(Radius),
    maybe_value_width(Width),
    maybe_value_height(Height).

% converts the prolog output in a Scala-ready output
scala_repr(some(V), Out) :-
    format(atom(Out), 'Some("~w")', [V]).
scala_repr(none, 'None').

generate :-
    findall(R-W-H, shape_field_combo(R, W, H), Combos),
    forall(member(R-W-H, Combos), (
        scala_repr(R, RS),
        scala_repr(W, WS),
        scala_repr(H, HS),
        format("  (~w, ~w, ~w),~n", [RS, WS, HS])
    )).

:- initialization(generate).