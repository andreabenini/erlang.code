%% MAKE RELATIVE SOFT LINK - Create a relative soft link
%% @see Erlang does NOT create relative softlink, full path softlink only... this is just an hack
makeRelativeSoftLink(FullFileNameSrc, FullFileNameDst) ->
    file:make_symlink(filename:basename(binary_to_list(FullFileNameSrc)),
                      filename:basename(binary_to_list(FullFileNameDst)) ),
    file:rename(filename:basename(binary_to_list(DefaultUserDst)),
                binary_to_list(DefaultUserDst)).

%% File name handling
%% Extension
filename:extension("/tmp/foo.erl").
%% ".erl"
