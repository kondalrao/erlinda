%%%-------------------------------------------------------------------
%%% File    : fs_boot_smithe.erl
%%% Author  : Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%
%%% @doc <p>A library to create script and boot files. </p>
%%% <strong>FileWide Types and Definitions. *README* or the docs will make 
%%% little sense.</strong>
%%% <pre>
%%% version() - is a version number for an application. These version 
%%%             numbers are always contain atleast one if not more periods 
%%%             and are strings.
%%%
%%%              Type: string()
%%%              Examples: "1.3.21" or "1.10" 
%%% 
%%% libdir() - The path to a directory that contains erlang applications
%%%            Example: "/home/martin/work/otp/lib" which contains the dirs 
%%%            for edoc, xmlrpc... The dirs under this libdir() can either 
%%%            contain a .app file with a version number in it or may be 
%%%            named with a prefix containing no - chars followed by a - and 
%%%            a version()
%%%
%%%           	Example: "/usr/local/lib/erlang/lib which contains stdlib-1.12,
%%%                       mnesia-4.12.4 and so on.
%%%
%%%            These paths may contain the wildcard * which will cause it to 
%%%            evaluate to all dirs including the "empty dir" that match the 
%%%            string.
%%%
%%%             Example: "/home/martin/*" will evaluate to "/home/martin" 
%%%                      and "/home/martin/work" if the only dir under
%%%                      /home/martin is work.
%%%
%%% relsrc() - This is a structure that embodies the source release spec for a
%%%            release. This structure is to be turned into a release spec. 
%%%            This structure can be used by functions in this module to 
%%%            create an valid OTP release file with proper version numbers 
%%%            and formatting. See the example rel.src below. 
%%%
%%% note* ",|" is to be interpreted as "and or"
%%% release_spec() = AppName | {AppName ,| AppVsn ,| AppType ,| IncApps} 
%%%       AppName = atom()
%%%       AppType = permanent | transient | temporary | load | none
%%%       IncApps = [atom()]
%%%
%%% Examples:
%%% rel.src 
%%%
%%% {release, {my_rel, "1.2.31"}
%%%  erts,
%%%  [
%%%   kernel,
%%%   stdlib,
%%%   mnesia,
%%%   {resource_discovery, "2.12.4"}
%%%   gas,
%%%   {galaxy_parser, none},
%%%   gq
%%%  ]
%%% }. 
%%%
%%%
%%% </pre>
%%% @end
%%%
%%% Created : 29 Jan 2004 by Martin J. Logan <martin@dhcp-lom-194-186.erlware.com>
%%%-------------------------------------------------------------------
-module(fs_boot_smithe).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 app_vsn_and_location/1,
         app_paths/2,
         app_paths/1,

         make_rel_file/5,
         make_rel_file/2,

         make_script_and_boot/6,
         make_script_and_boot/3,

         local_app_vsn/2,
	 compare_version_strings/2,

         stage_from_relsrc/3
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------
-record(release_spec, {app_name, version = undefined, app_type = undefined, inc_apps = undefined}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% @doc This function will return a dict where keys are AppName and 
%% values consist of list of  tuples of {version(), LibDir} for example
%% a key might be gas and a value [{"3.4.2", "/usr/local/lib/erlang/lib"}].
%% Each LibDir is checked for existence. If it does not exist it will i
%% not be included in the final list.
%%
%% Note* This function has a precidence order for application vsns. It 
%% first prefers local apps followed by the highest vsn of an installed app.
%%
%% <pre>
%% Types: 
%%  LibDirs = [libdir()] 
%%  AppVsnLocation = dict()
%% </pre>
%% @spec app_vsn_and_location(LibDirs) -> AppVsnLocation
%% @end 
%%--------------------------------------------------------------------
app_vsn_and_location(LibDirs) ->
    AllLibDirs = lists:foldl(fun(LibDir, Acc) -> 
                                   case hd(lists:reverse(LibDir)) of
                                       $* -> [filename:dirname(LibDir)|filelib:wildcard(LibDir)] ++ Acc;
                                       _  -> filelib:wildcard(LibDir) ++ Acc
                                   end
                           end, [], LibDirs),
    lists:foldl(fun(LibDir, Acc) -> app_vsn_and_location2(LibDir, Acc) end, dict:new(), AllLibDirs).

app_vsn_and_location2(LibDir, AccDict) ->
    AbsLibDir            = abs_path(LibDir),
    IgnoreListingsFor    = ["Makefile", "CVS"],
    {ok, LibDirNameList} = file:list_dir(AbsLibDir),
    CleanLibDirNameList  = LibDirNameList -- IgnoreListingsFor,

    LibAppAndVsnFun = fun(AppDir, Dict) -> 
                              case string:tokens(AppDir, "-") of                     
                                  [AppName] ->
                                      case catch local_app_vsn(AbsLibDir, AppName) of
					  {'EXIT', {{badmatch,{error,enoent}}, _}} -> 
                                              Dict;
					  {'EXIT', {{badmatch,{error,enotdir}}, _}} -> 
                                              Dict;
					  {'EXIT', Reason} -> 
                                              io:format("ERROR Could not get local app vsn for ~p reason ~p~n", [AppName, Reason]),
                                              Dict;
	 				  LocalAppVsn -> 
                                              dict:append(list_to_atom(AppName), {LocalAppVsn, AbsLibDir}, Dict)
				      end;
                                  [AppName, Vsn] = FullNameWithDash -> 
                                      case catch list_to_integer(lists:flatten(string:tokens(Vsn, "."))) of
                                          {'EXIT', Reason} -> 
                                              exit({error, {badname, AppDir}});
                                          _Float ->
                                              dict:append(list_to_atom(AppName), {Vsn, AbsLibDir}, Dict)
                                      end;
                                  _ -> 
                                      exit({error, {badname, AppDir}})
                              end
                      end,
    lists:foldl(LibAppAndVsnFun, AccDict, CleanLibDirNameList).


%%--------------------------------------------------------------------
%% @doc This function will return a list of full paths to app dirs within a 
%% local otp build system. Each app path is checked for existence. 
%% If it does not exist it will not be included in the final list.
%%
%% Note* This function has a precidence order for application vsns. It first 
%% prefers local apps followed by the highest vsn of an installed app.
%%
%% <pre>
%% Types: 
%%  LibDirs = [libdir()]
%%  AppPaths = [string()]
%% </pre>
%% @spec app_paths(LibDirs) -> AppPaths
%% @end 
%%--------------------------------------------------------------------
app_paths(LibDirs) ->
    Dict = dict:map(fun(Key, Value) -> lists:sort(fun({V1, P1}, {V2, P2}) -> compare_version_strings(V1, V2) end, Value) end, app_vsn_and_location(LibDirs)),
    [make_path(Path, atom_to_list(AppName), Vsn) || {AppName, [{Vsn, Path}|T]} <- dict:to_list(Dict)].

make_path(AppPath, AppNameString, Vsn) ->
    case filelib:is_dir(AppPath ++ "/" ++ AppNameString) of
        true -> AppPath ++ "/" ++ AppNameString;
        false -> AppPath ++ "/" ++ AppNameString ++ "-" ++ Vsn
    end.
  

%%--------------------------------------------------------------------
%% @doc This function will return a list of full paths to app dirs contained in the 
%% applications specified in IncludedApps within a local otp build system. 
%% Note* if any of the included apps can't be verified then an error will be returned.
%% <pre>
%% Varibles:
%%  IncludedApps - The Applications to be included in AppAndVsns
%%  AppPaths - A list of full paths to .app files for all library apps in the LibDir.
%%              Example: ["/home/martin/work/otp/lib/xmlrpc"]
%%
%% Types: 
%%  LibDirs = [libdir()]
%%  IncludedApps = [atom()]
%%  AppPaths = [string()]
%% </pre>
%% @spec app_paths(LibDirs, IncludedApps) -> AppPaths | {error, {enotfound, MissingIncludedApps}}
%% @end 
%%--------------------------------------------------------------------
app_paths(LibDirs, IncludedApps) ->
    Dict = dict:map(fun(Key, Value) -> lists:sort(fun({V1, P1}, {V2, P2}) -> compare_version_strings(V1, V2) end, Value) end, app_vsn_and_location(LibDirs)),
    AppVsnLocations = dict:to_list(Dict),
    case IncludedApps -- [AppName || {AppName, {Vsn, Path}} <- AppVsnLocations] of
        []    -> [make_path(Path, atom_to_list(AppName), Vsn) || {AppName, [{Vsn, Path}|T]} <- AppVsnLocations, lists:member(AppName, IncludedApps)];
	Apps  -> {error, {enotfound, Apps}}
    end.

%%--------------------------------------------------------------------
%% @doc Creates an otp release file from a list of applications.
%% <pre>
%% Varibles:
%%  RelName - A name for this release. The .rel file created by this function will be titled RelName.rel
%%  RelVsn - A version number for the release.
%%  ErtsSpec - The release spec for the erts application. 
%%  ReleaseSpecs - A list of individual ReleaseSpec's informing the system of how to load
%%                 a particular application within a release. 
%%
%% Sample Invocation:
%%  boot_smithe:make_rel_file(["/usr/local/lib/erlang/*", "/home/martin/work/otp/lib"], 
%%                                "therel", "myrel", "1.2", erts, [kernel, stdlib, sasl, {system_status, none}, insight]).
%%
%%
%% Types:
%%  LibDirs = [libdir()]
%%  RelName = string()
%%  RelVsn = version()
%%  ErtsSpec = erts | {erts,  version()}
%%  ReleaseSpecs = [release_spec()] for release_spec() definition see the top of this file.
%% </pre>
%% @spec make_rel_file(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs) -> ok | exit()
%% @end 
%%--------------------------------------------------------------------
make_rel_file(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs) ->
    AppVsnLocationDict       = app_vsn_and_location(LibDirs),
    SortedAppVsnLocationDict = sort_app_vsn_and_location_dict(AppVsnLocationDict),
    AppAndVsns               = [{AppName, Vsn} || {AppName, [{Vsn, Path}|T]} <- dict:to_list(SortedAppVsnLocationDict), 
                                                  lists:member(AppName, [erts|ReleaseSpecs]) orelse lists:keymember(AppName,1, ReleaseSpecs)],

    CompleteErtsSpec     = flesh_out_erts_release_spec(AppAndVsns, ErtsSpec),
    CompleteReleaseSpecs = tuplize_release_specs(flesh_out_release_specs(AppAndVsns, ReleaseSpecs)),

    {ok, FD} = file:open(RelName ++ ".rel", [write]),
    io:fwrite(FD, "~p.", [{release, {RelName, RelVsn}, CompleteErtsSpec, CompleteReleaseSpecs}]).

%%--------------------------------------------------------------------
%% @doc Creates an otp release file from a file containing relsrc().
%% <pre>
%% Varibles:
%%  RelSrcFilename -  The name of the file containing the relsrc().
%%
%% Sample Invocation:
%%  boot_smithe:make_rel_file(["/usr/local/lib/erlang/*", "/home/martin/work/otp/lib"], "myrel.rel.src").
%%
%%
%% Types:
%%  LibDirs = [libdir()]
%%  RelName = string()
%%  RelVsn = version()
%%  ErtsSpec = erts | {erts,  version()}
%%  ReleaseSpecs = [release_spec()] for release_spec() definition see the top of this file.
%% </pre>
%% @spec make_rel_file(LibDirs, RelSrcFilename) -> {ok, RelName} | exit()
%% @end 
%%--------------------------------------------------------------------
make_rel_file(LibDirs, RelSrcFilename) ->
    {ok, [{release, {RelName, RelVsn}, ErtsSpec, ReleaseSpecs}]} = file:consult(RelSrcFilename),
    ok = fs_boot_smithe:make_rel_file(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs),
    {ok, RelName}.


%%--------------------------------------------------------------------
%% @doc Creates a rel file as well as a script and boot file from a list of applications. Searches the paths that code knows about.
%% <pre>
%% Varibles:
%%  RelName - A name for this release. This name will be translated into
%%            BaseFileName.rel, BaseFileName.script, and BaseFileName.boot.
%%  RelVsn - A version number for the release. 
%%  ErtsSpec - The release spec for the erts application. 
%%  ReleaseSpecs - A list of individual ReleaseSpec's informing the system of how to load
%%                 a particular application within a release. 
%%  SystoolsOpts - Are options to be passed to the sasl application systools:make_script/2 see those docs for type info.
%%
%% Sample Invocation:
%%  boot_smithe:create_release_scripts_from_relsrc(["/usr/local/lib/erlang/*", "/home/martin/work/otp/lib"], "myrel", "1.2", 
%%                                     erts, [kernel, stdlib, sasl, {system_status, none}, insight], [local]).
%%
%% Types:
%%  LibDirs = [libdir()]
%%  RelName = string()
%%  RelVsn = version()
%%  ErtsSpec = erts | {erts ,| AppVsn}
%%  ReleaseSpecs = AppName | {AppName ,| AppVsn ,| AppType ,| IncApps} note* ,| is to be interpreted as "and or"
%%   AppName = atom()
%%   AppType = permanent | transient | temporary | load | none
%%   IncApps = [atom()]
%%  Release = {release, {RelName, RelVsn}, ErtsReleaseSpec, ReleaseSpecs}.  see erlang user guide for sasl. 
%% </pre>
%% @spec make_script_and_boot(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs, SystoolsOpts) -> ok | error |exit()
%% @end 
%%--------------------------------------------------------------------
make_script_and_boot(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs, SystoolsOpts) ->
    ok = make_rel_file(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs),
    EbinPaths = [N ++ "/ebin" || N <- app_paths(LibDirs)],
    code:add_pathsz(EbinPaths),
    systools:make_script(RelName, SystoolsOpts).

    
%%--------------------------------------------------------------------
%% @doc Creates a rel file as well as a script and boot file from a file containing a relsrc(). Searches the paths that code knows about.
%% <pre>
%% Variables:
%%  RelSrcFilename - The name of a file containing a relsrc()
%%
%% Types:
%%  LibDirs = [libdir()]
%%  RelSrcFilename = string() 
%%  Options = [{Key, Value}]
%%   Key = systools_opts | substitutions
%%   Value = term()
%%
%% Options:
%%  systools_opts - Are options to be passes to the sasl application systools:make_script/2 see those docs for type info. Defaults to [local]
%%  substitutions - a list of tuples containg text substitutions that are to be made from tokens in the 
%%                  file. All text that is to be substituted should take the form of text surrounded by % signs. 
%%                  Example in file {filename, %ReleaseName%} and the substitutions tokens would be 
%%                  {substitutions, [{"%RELEASE_NAME%", somefile}]}.
%%  
%% </pre>
%% @spec make_script_and_boot(LibDirs, RelSrcFilename, Options) -> {ok, RelName} | exit()
%% @end 
%%--------------------------------------------------------------------
make_script_and_boot(LibDirs, RelSrcFilename, Options) ->
    {ok, [Rel]} = file:consult(RelSrcFilename),

    %% Get options and set defaults if nessisary.
    SystoolsOpts = fs_lists:get_val(systools_opts, Options, [local]),

    {release, {RelName, RelVsn}, ErtsSpec, ReleaseSpecs} = 
        case fs_lists:get_val(substitutions, Options, []) of
            [] -> 
                Rel;
            Substitutions ->
                %% Check that all  Substitutions are valid - ie they are either a string or an atom and are prefixed and suffixed with a "%" sign.
                true = fs_lists:do_until(fun({Key, Value}) -> is_valid_substitution_key(Key) end, false, Substitutions),
                fs_lib:substitute_among_terms(Rel, Substitutions)
        end,
    
    ok = make_script_and_boot(LibDirs, RelName, RelVsn, ErtsSpec, ReleaseSpecs, SystoolsOpts),
    {ok, RelName}.


%%--------------------------------------------------------------------
%% @doc Places all user applications contained within a relsrc() spec under a specific directory.
%% XXX This version of this function only selects the apps of highest version number and ignores any version number which
%%         may be present in the rel.src file.
%% <pre>
%% Types:
%%  RelSrcFilename = TargetDir = string()
%%  LibDirs = [libdir()]
%% </pre>
%% @spec stage_from_relsrc([LibDir], RelSrcFilename, TargetDir) -> ok | exit()
%% @end 
%%--------------------------------------------------------------------
stage_from_relsrc(LibDirs, RelSrcFilename, TargetDir) ->
    {ok, [{release, {RelName, RelVsn}, ErtsSpec, ReleaseSpecs}]} = file:consult(RelSrcFilename),

    AppVsnLocationDict       = app_vsn_and_location(LibDirs),
    SortedAppVsnLocationDict = sort_app_vsn_and_location_dict(AppVsnLocationDict),
    
    Results = [os:cmd("cp -r " ++ Path ++ "/" ++ atom_to_list(AppName) ++ " " ++ TargetDir ++ "/" ++ atom_to_list(AppName) ++ "-" ++ Vsn) || 
                  {AppName, [{Vsn, Path}|T]} <- dict:to_list(SortedAppVsnLocationDict), 
                  lists:member(AppName, [erts|ReleaseSpecs]) orelse lists:keymember(AppName,1, ReleaseSpecs)],
    lists:foreach(fun([]) -> ok end, Results).
                          
    
    
%%--------------------------------------------------------------------
%% @doc Returns the version number for a single local(resides in a home dir with no version number appended to its base directory) application. 
%% <pre>
%% Example: local_app_vsn("../../", "gas") returns "4.1.2"
%% </pre>
%% @spec local_app_vsn(LibDir, AppNameString) -> version() | exit()
%% @end 
%%--------------------------------------------------------------------
local_app_vsn(LibPath, AppNameString) ->
    {ok, [{application, AppName, KeyValues}]} = file:consult(LibPath ++ "/" ++ AppNameString ++ "/ebin/" ++ AppNameString ++ ".app"),
    {value, {vsn, AppVsn}} = lists:keysearch(vsn, 1, KeyValues),
    AppVsn.


%%--------------------------------------------------------------------
%% @doc Is version string A bigger than version string B?
%% <pre>
%% Example: compare_version_strings("3.2.5", "3.1.6") will return true
%% </pre>
%% @spec compare_version_strings(VsnStringA, VsnStringB) -> bool()
%% @end
%%--------------------------------------------------------------------
compare_version_strings(VsnStringA, VsnStringB) ->
    compare(string:tokens(VsnStringA, "."),string:tokens(VsnStringB, ".")).

compare([StrDig|TA], [StrDig|TB])   -> compare(TA, TB);
compare([StrDigA|TA], [StrDigB|TB]) -> list_to_integer(StrDigA) > list_to_integer(StrDigB);
compare([], [StrDigB|TB])           -> false;
compare([StrDigA|TA], [])           -> true;
compare([], [])                     -> false.

%%====================================================================
%% Internal functions
%%====================================================================
%% Sort a app vsn and location dict in so far as making an application that has more than one version associated with it position the highest version 
%% of the application before lower ones. If two versions are equal but the path of one is under /home and the other not the one in home is prefered.
sort_app_vsn_and_location_dict(Dict) ->
    dict:map(fun(Key, Value) -> 
                     lists:sort(fun({V1, P1}, {V1, P2}) -> "/home" == string:substr(P1, 1, 5);
                                   ({V1, P1}, {V2, P2}) -> compare_version_strings(V1, V2) 
                                end, Value) 
             end, Dict).
    

%% Convert a list of release spec records into a list of actual release spec tuples.
tuplize_release_specs(ReleaseSpecs) ->
    lists:map(fun(ReleaseSpecRecord) ->tuplize_release_spec(ReleaseSpecRecord) end, ReleaseSpecs).
                      
tuplize_release_spec(#release_spec{app_name = AppName, version = Version, app_type = undefined, inc_apps = undefined}) ->
    {AppName, Version};
tuplize_release_spec(#release_spec{app_name = AppName, version = Version, app_type = undefined, inc_apps = IncApps}) ->
    {AppName, Version, IncApps};
tuplize_release_spec(#release_spec{app_name = AppName, version = Version, app_type = AppType, inc_apps = undefined}) ->
    {AppName, Version, AppType};
tuplize_release_spec(#release_spec{app_name = AppName, version = Version, app_type = AppType, inc_apps = IncApps}) ->
    {AppName, Version, AppType, IncApps}.
    

flesh_out_erts_release_spec(AppAndVsns, {Erts, ErtsVsn} = ErtsReleaseSpec) ->
    ErtsReleaseSpec;
flesh_out_erts_release_spec(AppAndVsns, {erts}) ->
    flesh_out_erts_release_spec(AppAndVsns, erts);
flesh_out_erts_release_spec(AppAndVsns, erts) ->
    {value, ErtsReleaseSpec} = lists:keysearch(erts, 1, AppAndVsns),
    ErtsReleaseSpec.


%% Take a raw tuple format release spec and turn it into a proper release_spec record with version number if it is not provided.
flesh_out_release_specs(AppAndVsns, ReleaseSpecs) ->
    lists:map(fun(ReleaseSpec) -> insert_version(AppAndVsns, fill_available_fields(ReleaseSpec)) end, ReleaseSpecs).

fill_available_fields({AppName}) ->
    #release_spec{app_name = AppName};
fill_available_fields(AppName) when is_atom(AppName) ->
    #release_spec{app_name = AppName};
fill_available_fields({AppName, Something}) ->
    update_record(#release_spec{app_name = AppName}, Something);
fill_available_fields({AppName, Something, SomethingElse}) ->
    update_record(update_record(#release_spec{app_name = AppName}, Something), SomethingElse);
fill_available_fields({AppName, Something, SomethingElse, YetAnotherSomething}) ->
    update_record(update_record(update_record(#release_spec{app_name = AppName}, Something), SomethingElse), YetAnotherSomething).

insert_version(AppAndVsns, #release_spec{app_name = AppName, version = undefined} = Record) ->
    {value, {AppName, Vsn}} = 
	case lists:keysearch(AppName, 1, AppAndVsns) of
	    {value, {AppName_, Vsn_}} -> {value, {AppName_, Vsn_}};
	    Error                   -> io:format("ERROR at line ~p in ~p could not find version for ~p among app and versions discoverd: ~p~n", [?LINE, ?MODULE, AppName, AppAndVsns]),
				       timer:sleep(1000),
                                       error
	end,
    Record#release_spec{version = Vsn};
insert_version(AppAndVersions, Record) ->
    Record.

    
%% Fill the proper release_spec field with Something.
%% Returns: NewRecord | exit()
update_record(Record, Something) ->
    case release_spec_type(Something) of
        version  -> Record#release_spec{version  = Something};  
        app_type -> Record#release_spec{app_type = Something};
        inc_apps -> Record#release_spec{inc_apps = Something}
    end.
        
    
%% Returns: app_type | inc_apps | version | exit()
release_spec_type(permanent) -> app_type;
release_spec_type(transient) -> app_type;
release_spec_type(temporary) -> app_type;
release_spec_type(none)      -> app_type;
release_spec_type(load)      -> app_type;

release_spec_type(Something) when is_list(Something) ->
    case catch list_to_integer(lists:flatten(string:tokens(Something, "."))) of
        Number when is_integer(Number) -> version;
        Failed -> 
            if 
                length(Something) > 0 ->
                    case hd(Something) of
                        Hd when is_atom(Hd) -> inc_apps;
                        _Error              -> exit({no_such_release_spec_type, Something})
                    end;
                [] -> 
                    inc_apps
            end
    end.


%% Determine if a substitution atom or string is prefixed and suffixed with the "%" tokens.
is_valid_substitution_key(Key) when is_atom(Key) ->
    is_valid_substitution_key(atom_to_list(Key));
is_valid_substitution_key(Key) when is_list(Key) ->
    case {hd(Key), hd(lists:reverse(Key))} of
        {$%, $%} -> 
            true;
        _ ->
            io:fwrite("fs_boot_smithe:is_valid_substitution_token ERROR Improper substitution key ~p~n", [Key]),
            false
    end;
is_valid_substitution_key(Key) -> 
    false.
    

abs_path(RelativePath) ->
    {ok, InitialPath} = file:get_cwd(),
    file:set_cwd(RelativePath),
    {ok, AbsPath} = file:get_cwd(),
    file:set_cwd(InitialPath),
    AbsPath.



