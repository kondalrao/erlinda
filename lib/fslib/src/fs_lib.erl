%%%-------------------------------------------------------------------
%%% File    : fs_lib.erl
%%% Author  : Martin J. Logan <martin@mecha.erlware.com>
%%%
%%% @doc  This module holds all the usefull functions that cannot be 
%%%       classified as belonging to any of the other standard library modules.
%%% @end
%%%
%%% Created : 14 Feb 2004 by Martin J. Logan <martin@erlware.com>
%%%-------------------------------------------------------------------
-module(fs_lib).

%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
	 fwrite/2,
	 format/2,
         s_apply/1,
         eval_n_times/2,
         common_reply/1,
         substitute_among_terms/2
        ]).


%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Prints like io:fwrite but the integer lists that would normally 
%%      be shown are rendered as quoted strings.
%% @spec fwrite (Control, Args) -> ok | EXIT
%% @end 
%%--------------------------------------------------------------------
fwrite (Control, Args) ->
    Str = format (Control, Args),
    io:fwrite ("~s", [Str]).

%%--------------------------------------------------------------------
%% @doc Returns a string like io_lib:format but the integer lists that 
%%      would normally be in the result are rendered as quoted strings.
%% @spec format (Control, Args) -> string() | EXIT
%% @end 
%%--------------------------------------------------------------------
format (Control, Args) ->
    RawStr = io_lib:format (Control, Args),
    fs_string:stringize (RawStr).

%%--------------------------------------------------------------------
%% @doc Allows any function to be called from the command line via erl -s 
%% all arguments should be passed in the following manner:
%% erl -s mod func "arg1. " "arg2. "...
%% <pre>
%% Variables: 
%%  MFSList - a list containing the mod func and a list of args to apply comming via erl -s
%%
%% Types:
%%  MFAList = [Mod, Func|Args] Example [file, list_dir, '"/var/log". ']
%% </pre>
%% @spec s_apply(MFAList) -> void()
%% @end 
%%--------------------------------------------------------------------
s_apply([Mod, Func|Args]) -> 
    io:format("fs_lib:s_apply args before parsing: ~w~n", [Args]),
    TokedArgs = lists:map(fun(ArgAtom) -> 
                                  {ok, Toks, Line} = erl_scan:string(atom_to_list(ArgAtom), 1), 
                                  {ok, Term}       = erl_parse:parse_term(Toks), 
                                  Term 
                          end, Args),
    io:format("apply the following: apply(~w, ~w, ~p)~n", [Mod, Func, TokedArgs]),
    Result = (catch apply(Mod, Func, TokedArgs)),
    io:format("Result: ~p~n", [Result]),
    Result.


%%--------------------------------------------------------------------
%% @doc Evaluate a fun n number of times.
%% @spec eval_n_times(Fun, N) -> ok
%% @end
%%--------------------------------------------------------------------
eval_n_times(Fun, 0) ->
    ok;
eval_n_times(Fun, N) ->
    Fun(),
    eval_n_times(Fun, N-1).

%%--------------------------------------------------------------------
%% @doc Simply convert a caught 'EXIT' or a badrpc into an error reason.
%% <pre>
%%  Reply                  CommonReply
%%  -----                  -----------
%%  {'EXIT', Reason} --->  {error, Reason}
%%  {badrpc, Reason} --->  {error, Reason}
%%  {error, Reason}  --->  {error, Reason}
%%  {atomic, Reply}  --->  {ok, Reply}
%%  error            --->  error
%%  Other            --->  Other
%% </pre>
%% @spec common_reply(Reply) -> CommonReply 
%% @end
%%--------------------------------------------------------------------
common_reply({'EXIT', Reason}) -> {error, Reason};
common_reply({badrpc, Reason}) -> {error, Reason};
common_reply({atomic, Reply})  -> {ok, Reply};
common_reply(Reply)            -> Reply.
                    

%%--------------------------------------------------------------------
%% @doc Takes an arbitrary list of terms and replaces all occurences of SubstitutionTokens
%% with the cooresponding Substitute.
%% <pre>
%% Types:
%%  SubstitutionToken - An atom or string that is prefixed and suffixed with a "%" sign.
%%  Substitute - To be placed in the term in place of the SubstitutionToken
%%  Substitutions - A list of SubstitutionToken/Substitute pairs.
%%
%% Example:
%%  The Substitutions [{'%A%', '/var/log'}] to the term [{logdir, '%A'}] will resutl in [{logdir, '/var/log'}]
%%
%% Types:
%%  Substitutions = [Substitution] Example: [{'%A%', '/var/log'}, {"%B%", "/usr/local"}]
%%   Substitution = {SubstitutionToken, Substitute} = {atom(), atom()} | {string(), string()} 
%% </pre>
%% @spec substitute_among_terms(Term, Substitutions) -> NewTerm
%% @end 
%%--------------------------------------------------------------------

%% Top Level Term is Tuple -> iteratable
%%
substitute_among_terms(Term, Substitutions) when is_tuple(Term) ->
    list_to_tuple(substitute_among_terms(tuple_to_list(Term), Substitutions));

%% Top Level Term is List -> iteratable
%%
%% Inner Term is Tuple -> Iteratable
substitute_among_terms([Term|T], Substitutions) when is_tuple(Term) ->
    [list_to_tuple(substitute_among_terms(tuple_to_list(Term), Substitutions))|substitute_among_terms(T, Substitutions)];

%% Inner Term is List -> Iteratable
substitute_among_terms([Term|T] = FullTerm, Substitutions) when is_list(Term) ->
    case fs_lists:is_string(FullTerm) of
        true  -> 
            search_and_replace_string(Term, Substitutions);
        false -> 
            case fs_lists:is_string(Term) of
                true  -> [search_and_replace_string(Term, Substitutions) | substitute_among_terms(T, Substitutions)];
                false -> [substitute_among_terms(Term, Substitutions)|substitute_among_terms(T, Substitutions)]
            end
    end;

%% Inner Term is Atom -> Not iteratable
substitute_among_terms([Term|T], Substitutions) when is_atom(Term) ->
    case lists:keysearch(Term, 1, Substitutions) of
        {value, {MatchTerm, Substitution}} -> [Substitution|substitute_among_terms(T, Substitutions)];
        _Undefined                         -> [Term|substitute_among_terms(T, Substitutions)]
    end;
    
%% Inner Term is Other -> Not iteratable
substitute_among_terms([Term|T], Substitutions) ->
    [Term|substitute_among_terms(T, Substitutions)];
substitute_among_terms([], _) ->
    [];
    
%% Top Level Term is Atom -> Not iteratable
%%
substitute_among_terms(Term, Substitutions) when is_atom(Term) ->
    io:format("Atom Term ~p~n", [Term]),
    case lists:keysearch(Term, 1, Substitutions) of
        {value, {MatchTerm, Substitution}} -> Substitution;
        _Undefined                         -> Term
    end;

%% Top Level Term is Other -> Not iteratable
%%
substitute_among_terms(Term, Substitutions) when is_atom(Term) ->
    io:format("Atom Term ~p~n", [Term]),
    case lists:keysearch(Term, 1, Substitutions) of
        {value, {MatchTerm, Substitution}} -> Substitution;
        _Undefined                         -> Term
    end.
    
    
search_and_replace_string([$%|PotentialMatch], Substitutions) -> 
    {Replaced, Rest} = replace_substring(PotentialMatch, Substitutions),
    Replaced ++ search_and_replace_string(Rest, Substitutions);
search_and_replace_string([H|T], Substitutions) -> 
    [H|search_and_replace_string(T, Substitutions)];
search_and_replace_string([], Substitutions) ->
    [].
                                                
replace_substring(PotentialMatch, Substitutions) ->
    {StrippedMatchTerm, Rest} = lists:splitwith(fun($%) -> false;
                                           (El) -> true 
                                        end, PotentialMatch), 
    case Rest of
        [$%|Unreplaced] -> 
            MatchTerm = [$%|StrippedMatchTerm] ++ "%",
            case lists:keysearch(MatchTerm, 1, Substitutions) of
                {value, {MatchTerm, Substitution}} -> {Substitution, Unreplaced};
                _Undefined                         -> {MatchTerm, Unreplaced}
            end;
        Unreplaced ->
            {StrippedMatchTerm, Rest}
    end.
    


