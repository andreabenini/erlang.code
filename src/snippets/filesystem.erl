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


%% DELETE DIR - Recursive delete dir
deleteDir(Directory) ->
    case filelib:is_dir(Directory) of
        true -> lists:foreach(fun(D) -> ok = file:del_dir(D) end, deleteDirAllFiles([Directory], []));
        _    -> ok
    end.
deleteDirAllFiles([], EmptyDirs) ->
    EmptyDirs;
deleteDirAllFiles([Dir|T], EmptyDirs) ->
    {ok, FilesInDir} = file:list_dir(Dir),
    {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                    Path = Dir ++ "/" ++ F,
                                    case filelib:is_dir(Path) of
                                        true  -> {Fs, [Path | Ds]};
                                        false -> {[Path | Fs], Ds}
                                    end
                                end, {[],[]}, FilesInDir),
    lists:foreach(fun(F) -> ok = file:delete(F) end, Files),
    deleteDirAllFiles(T ++ Dirs, [Dir | EmptyDirs]).
