%%%-------------------------------------------------------------------
%%% @doc <p>A library to create a release bundle. </p>
%%%
%%% <strong>FileWide Types and Definitions. *README* or the docs will make little sense.</strong>
%%% <pre>
%%% version() - is a version number for an application. These version 
%%%             numbers are always contain atleast one if not more periods and are strings.
%%%             Type: string()
%%%             Examples: "1.3.21" or "1.10" 
%%% 
%%% libdir() - The path to a directorythat contains erlang applications
%%%            Example: "/home/martin/work/otp/lib" which contains the dirs for edoc, xmlrpc...
%%%            The dirs under this libdir() can either contain a .app file with a version number in it
%%%            or may be named with a prefix containing no - chars followed by a - and a version()
%%%            Example: "/usr/local/lib/erlang/lib which contains stdlib-1.12, mnesia-4.12.4 and so on.
%%%            These paths may contain the wildcard * which will cause it to evaluate to all dirs including the "empty dir" 
%%%            that match the string.
%%%            Example: "/home/martin/*" will evaluate to "/home/martin" and "/home/martin/work" if the only dir under
%%%            /home/martin is work.
%%%
%%% relsrc() - This is a structure that embodies the src spec for a release. This structure can be used by 
%%%            functions in this module to create an valid OTP release file with proper version numbers and formatting. 
%%%
%%% release_spec() = AppName | {AppName ,| AppVsn ,| AppType ,| IncApps} note* ,| is to be interpreted as "and or"
%%%        AppName = atom()
%%%        AppType = permanent | transient | temporary | load | none
%%%        IncApps = [atom()]
%%%
%%% </pre>
%%% @end
%%%-------------------------------------------------------------------
-module(fs_release_smithe).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 create_release/2
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


%%----------------------------------------------------------
%% @doc Giving the paths to all the libraries that are used by this release package all the libs into a nice little bundle for deployment.
%% <pre>
%% Variables:
%%  LibDirs - List of possible locations for libraries to include. 
%%
%% Types:
%%  LibDirs = [libdir()]
%% </pre>
%% @spec create_release(LibDirs, RelSrcFilename) -> void()
%% @end
%%----------------------------------------------------------
create_release(LibDirs, RelSrcFilename) ->

    %% Create the .rel file from the .rel.src file.
    {ok, RelName}      = fs_boot_smithe:make_rel_file(LibDirs, RelSrcFilename),
    AppVsnLocationDict = fs_boot_smithe:app_vsn_and_location(LibDirs),
    {ok, [{release, {RelName, RelVsn}, ErtsSpec, ReleaseSpecs}]} = file:consult(RelName++ ".rel"),

    %% Create release directory with version included in directoryname.
    BundleDirName = RelName ++ "-" ++  RelVsn,
    os:cmd("rm -rf " ++ BundleDirName),
    file:make_dir(BundleDirName),
    file:make_dir(BundleLibDir = BundleDirName ++ "/lib"),

    %% Create the release-bundle by copying all nessesary applications for this release into our BundleDirName directory.
    lists:foreach(fun(ReleaseSpec)  ->
                          AppName = element(1, ReleaseSpec),
                          AppVsn  = element(2, ReleaseSpec),
                          {ok, VsnLocationList} = dict:find(AppName, AppVsnLocationDict),
                          {value, {AppVsn, AppPath}} = lists:keysearch(AppVsn, 1, VsnLocationList),
                                                %Check doesn't have version
                          case filelib:is_dir(AppPath ++ "/" ++ atom_to_list(AppName)) of
                              true -> os:cmd("cp -r " ++ AppPath ++ "/" ++ atom_to_list(AppName) ++ 
                                             " " ++ BundleLibDir ++ "/" ++ atom_to_list(AppName) ++ "-" ++ AppVsn);

                              false -> os:cmd("cp -r " ++ AppPath ++ "/" ++ atom_to_list(AppName) ++ "-" ++ AppVsn ++" " ++ BundleLibDir)
                          end
    		  end, ReleaseSpecs),
	
    %% Enter the release-bundle directory and create the boot and script file for the bundle.
    {ok, InitialDir} = file:get_cwd(),
    {ok, Bytes}      = file:copy(RelName ++ ".rel", BundleDirName ++ "/" ++ RelName ++ ".rel"),
    file:set_cwd(BundleDirName),
    code:set_path(filelib:wildcard("./lib/*/ebin")),
    systools:make_script(RelName, [local]),
    file:set_cwd(InitialDir).


%%====================================================================
%% Internal functions
%%====================================================================
