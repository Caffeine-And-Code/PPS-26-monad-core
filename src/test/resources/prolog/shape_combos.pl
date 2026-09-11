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
