%% MUC USERS GET - Save affiliations binary state of a MUC
mucUsersGet(RoomPID) ->
    ?INFO_MSG("Room PID=~p", [RoomPID]),
    {ok, StateData} = gen_fsm:sync_send_all_state_event(RoomPID, get_state),
    %% ErlangTerm -> Binary -> UUENCODE (no binaries inside a vcard)
    base64:encode( term_to_binary(StateData#state.affiliations) ).

%% MUC USERS SET - Restore affiliations binary state of a MUC
mucUsersSet(RoomPID, AffiliationsData) ->
    ?INFO_MSG("Room PID=~p", [RoomPID]),
    {ok, StateData} = gen_fsm:sync_send_all_state_event(RoomPID, get_state),
    NewData = binary_to_term(base64:decode(AffiliationsData)),
    ?INFO_MSG("NewData=~p", [NewData]),
    NewState = StateData#state{affiliations=NewData},
    gen_fsm:sync_send_all_state_event(RoomPID, {change_state, NewState}).
