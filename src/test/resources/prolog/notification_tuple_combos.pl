notification_type(info).
notification_type(error).
notification_type(success).

notification_pair(First, Second) :-
    notification_type(First),
    notification_type(Second).
