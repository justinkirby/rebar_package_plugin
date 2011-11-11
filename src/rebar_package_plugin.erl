-module(rebar_package_plugin).

-export([package/2]).

-include_lib("kernel/include/file.hrl").

-define(DEBUG(Msg, Args),
        rebar_log:log(debug, "[~p]  "++ Msg, [?MODULE|Args])).
-define(WARN(Msg, Args),
        rebar_log:log(warn, "[~p]  "++ Msg, [?MODULE|Args])).
-define(INFO(Msg, Args),
        rebar_log:log(info, "[~p]  "++ Msg, [?MODULE|Args])).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).
-define(CONSOLE(Msg, Args),
        io:format(Msg,Args)).



package(Config, _) ->

    %% we in the rel dir?
    case rebar_rel_utils:is_rel_dir() of
        false ->
            ?DEBUG("~s not reldir~n",[rebar_utils:get_cwd()]);
        {true,ReltoolFile} ->
            package_aux(Config, ReltoolFile)
    end.

package_aux(Config, ReltoolFile) ->

    OrigCodePath = code:get_path(),

    %% I stole a lot of code from rebar_upgrade.erl
    {Name, Ver} = rebar_rel_utils:get_reltool_release_info(ReltoolFile),
    ok = run_checks(ReltoolFile, {Name,Ver}),
    ok = setup(ReltoolFile, {Name, Ver}),
    ok = gen_deps(Config,ReltoolFile, {Name, Ver}),

    ok = run_systools({Name,Ver}),

    ReltoolConfig = load_config(ReltoolFile),

    ok = pkg_manifest(Config,ReltoolConfig,Name,Ver),
    ok = mk_tarball(Config,ReltoolConfig, Name,Ver),

    true = code:set_path(OrigCodePath),

    ok.

run_checks(_ReltoolFile, {Name, _}) ->

    NamePath = filename:join([".",Name]),
    true = rebar_utils:prop_check(filelib:is_dir(NamePath),
				  "Release directory does not exist (~p)~n",[NamePath]),
    ok.

setup(_ReltoolFile, {Name,_Ver}) ->
    PathParts = [[Name, "releases","*"],
		 [Name, "lib","*","ebin"]
		],

    {ok, Cwd} = file:get_cwd(),

    AddToPath = fun(P) ->
     			RealPath = Cwd++"/"++P,
     			case filelib:is_dir(RealPath) of
     			    true -> code:add_path(RealPath);
     			    false -> ok
     			end
     		end,

    PathLists = fun(P) ->
     			Path = filename:join(P),
     			PathAdd = filelib:wildcard(Path),
     			lists:foreach(AddToPath,PathAdd)
     		end,

    lists:foreach(PathLists, PathParts),


    ok.
%% we need to 'categorize' the apps in the rel file.
%% making a deps.rel file in priv dir for this app
%% this is mainly for vinst
gen_deps(Config, _ReltoolFile, {Name, Ver}) ->
    PrivDir = filename:join([".",Name,"lib",Name++"-"++Ver,"priv"]),
    DepsRel = filename:join([PrivDir,"deps.rel"]),
    ok = filelib:ensure_dir(DepsRel),

    %% find the Name.rel file in Name/releases/Vsn/Name.rel
    RelFile = filename:join([".",Name,"releases",Ver,Name++".rel"]),

    AllApps = case file:consult(RelFile) of
		  {ok, [{release,_RelAppVsn,_RelErts,RelApps}]} ->
		      RelApps;
		  {error, Reason} ->
		      ?ABORT("Unable to consult ~p~n~p~n",[RelFile,Reason])
	      end,

    Deps = get_deps_list(Config),
    AppList = get_apps_list(Config),
    {TagSys,TagDep,TagApp} = tag_deps(AllApps,Deps,AppList),

    TagDepRel = io_lib:format("%% meta info for vinst~n~p.~n~p.~n~p.~n",[TagSys,TagDep,TagApp]),
    ok = file:write_file(DepsRel,TagDepRel),

    ok.







run_systools({Name, Ver}) ->
%%    systools:make_script(Name),

    {ok, OldCwd} = file:get_cwd(),

    RelDir = filename:join([Name, "releases",Ver]),
    true = filelib:is_dir(RelDir),
    ok = file:set_cwd(RelDir),

    %% create and destroy the tarball.  doing this just to make sure
    %% everything is happy and all the files like name.rel etc.. are
    %% in the proper places
    systools:make_tar(Name,[silent]),
    ok = file:set_cwd(OldCwd),

    SysTar= filename:join([Name,"releases",Ver,Name++".tar.gz"]),
    rebar_file_utils:rm_rf(SysTar),


    Tar = filename:join([Name++"-"++Ver++".tar.gz"]),
    %% {ok, Tarball} = erl_tar:open(Tar,[write,compressed]),
    %% ok = erl_tar:add(Tarball,"lib",[]),
    %% ok = erl_tar:add(Tarball,"releases",[]),

    %% erl_tar:close(Tarball),





    ok = file:set_cwd(OldCwd),

    rebar_config:set_global(rel_pkg,Tar),
    ok.


%% fixup_rel_pkg({Name,Ver},Rel) ->
%%     RelDir = filename:join([Name,"releases",Ver]),
%%     FilesAdd = filelib:wildcard(filename:join([RelDir,"*"])),

%%     {ok, RelPkg} = erl_tar:open(Rel,[write,compressed]),
%%     lists:foreach(fun(F) ->
%% 			  append_to_rel(RelPkg,F)
%% 		  end,FilesAdd),
%%     erl_tar:close(RelPkg).

%% append_to_rel(Rel,File) ->
%%     ok = erl_tar:add(Rel,File,[]).



get_deps_list(Config) ->
    RebarDeps = case get_rebar_key(deps,Config) of
		    undefined -> [];
		    Deps ->
			lists:map(fun({D,_,_}) -> D end,Deps)
		end,
    RebarDeps.
get_apps_list(Config) ->
    Apps = case get_rebar_key(sub_dirs,Config) of
	       undefined -> [];
	       Subs ->
		   NotRel = lists:filter(fun(S) -> case S of "rel" -> false; _ -> true end end, Subs),
		   lists:map(fun(S) -> list_to_atom(lists:last(filename:split(S))) end,NotRel)
	   end,
    Apps.



get_rebar_key(Key,{config,_Path,L}) ->
    case proplists:get_value(Key,L) of
	undefined -> undefined;
	Value -> Value
    end.

tag_deps(Apps,Deps,AppList) ->
    tag_deps(Apps,Deps,AppList,[],[],[]).

tag_deps([],_Deps,_AppList,Sys,Dep,App) ->
    {{sys,Sys},{dep,Dep},{app,App}};

tag_deps([{Name,Vsn}|Rest],Deps,AppList,Sys,Dep,App) ->
    case lists:member(Name,Deps) of
        true ->
            tag_deps(Rest,Deps,AppList,Sys,[{Name,Vsn}|Dep],App);
        false ->
            case lists:member(Name,AppList) of
                true ->
                    tag_deps(Rest,Deps,AppList,Sys,Dep,[{Name,Vsn}|App]);
                false ->
                    tag_deps(Rest,Deps,AppList,[{Name,Vsn}|Sys],Dep,App)
            end
    end;
%% the {name,vsn,load} tuple was added in r14. we don't use it, so strip it
tag_deps([{Name,Vsn,load}|Rest],Deps,AppList,Sys,Dep,App) ->
    tag_deps([{Name,Vsn}|Rest],Deps,AppList,Sys,Dep,App).


%%
%% Load terms from reltool.config
%%
load_config(ReltoolFile) ->
    case file:consult(ReltoolFile) of
        {ok, Terms} ->
            Terms;
        Other ->
            ?ABORT("Failed to load expected config from ~s: ~p\n",
                   [ReltoolFile, Other])
    end.


%%
%% Look for overlay_vars file reference. The user can override this from the
%% command line (i.e. globals), so we check there first and then fall back to
%% what is present in the reltool.config file
%%
overlay_vars(ReltoolConfig) ->
    case rebar_config:get_global(overlay_vars, undefined) of
        undefined ->
            case lists:keyfind(overlay_vars, 1, ReltoolConfig) of
                {overlay_vars, File} ->
                    File;
                false ->
                    undefined
            end;
        File ->
            File
    end.

target_dir(ReltoolConfig) ->
    case rebar_config:get_global(target_dir, undefined) of
        undefined ->
            case lists:keyfind(target_dir, 1, ReltoolConfig) of
                {target_dir, TargetDir} ->
                    filename:absname(TargetDir);
                false ->
                    {sys, SysInfo} = sys_tuple(ReltoolConfig),
                    case lists:keyfind(rel, 1, SysInfo) of
                        {rel, Name, _Vsn, _Apps} ->
                            filename:absname(Name);
                        false ->
                            filename:absname("target")
                    end
            end;
        TargetDir ->
            filename:absname(TargetDir)
    end.
%%
%% Look for the {sys, [...]} tuple in the reltool.config file.
%% Without this present, we can't run reltool.
%%
sys_tuple(ReltoolConfig) ->
    case lists:keyfind(sys, 1, ReltoolConfig) of
        {sys, _} = SysTuple ->
            SysTuple;
        false ->
            ?ABORT("Failed to find {sys, [...]} tuple in reltool.config.", [])
    end.

mk_tarball(_Config, ReltoolConfig,Name,Ver) ->
    TargetDir = target_dir(ReltoolConfig),
    RelDir = rebar_utils:get_cwd(),
    Tarball = filename:join(RelDir, Name++"-"++Ver++".tar.gz"),

    %% validate the dir making sure the filter conditions are met.
    %% crash if we find paths >= 100
    Walker = fun(F, Acc) ->
                     case length(F) of
                         Len when Len >= 100 ->
                             ?ABORT("~s is a path length >= 100 and will not work "
                                    "with erl_tar. Please add to package list in "
                                    "reltool.config.~n",[F]);
                         _Len -> Acc
                     end
             end,
    ok = file:set_cwd(TargetDir),
    filelib:fold_files(".",".*", true, Walker,[]),

    {ok, Tar} = erl_tar:open(Tarball,[write,compressed]),

    {ok, DirList} = file:list_dir("."),
    lists:foreach(fun(D) ->
                          ok = erl_tar:add(Tar, D,[])
                  end, DirList),

    erl_tar:close(Tar),
    ok.





pkg_manifest(_Config, ReltoolConfig,Name,Ver) ->
    TargetDir = target_dir(ReltoolConfig),
    Terms = pkg_terms(ReltoolConfig),


    Manifest = case lists:keyfind(package, 1, ReltoolConfig) of
                   {package, Package} when is_list(Package) ->
                       pkg_execute(Package,Terms,rebar_utils:get_cwd(), TargetDir,[]);
                   false ->
                       ?INFO("No {package, [...]} found in reltool.config.\n",[]),
                       [];
                   _ ->
                       ?ABORT("{package, [...]} entry in reltool.config "
                              "must be a list.\n",[])
               end,

    PrivDir = filename:join([".",Name,"lib",Name++"-"++Ver,"priv"]),
    ok = filelib:ensure_dir(PrivDir),
    ManifestName = filename:join([PrivDir,"manifest"]),

    ManifestData = io_lib:format("%% list of tarballs generated that need to be extracted for complete install.~n~p.~n",[Manifest]),
    ok = file:write_file(ManifestName, ManifestData),

    ok.

pkg_execute([], _Vars,BaseDir, _TargetDir,Manifest) ->
    ok = file:set_cwd(BaseDir),
    Manifest;
pkg_execute([{Name, Path, Dirs,Rm}|Rest],Vars, BaseDir, TargetDir, Manifest) ->
    RealPath = render(Path,Vars),
    FullPath = filename:join(TargetDir,RealPath),
    TarName = atom_to_list(Name)++".tar.gz",

    %% move to RealPath/..
    ok = file:set_cwd(FullPath),
    {ok, Tar} = erl_tar:open(TarName,[write,compressed]),
    lists:foreach(fun(D) ->
                          ok = erl_tar:add(Tar,D,[])
                  end,Dirs),
    erl_tar:close(Tar),

    case Rm of
        true ->
            lists:foreach(fun(D) -> rebar_file_utils:rm_rf(D) end, Dirs);
        false ->
            ok
    end,

    ManEntry = {tar, filename:join(RealPath,TarName),RealPath},

    pkg_execute(Rest,Vars,BaseDir,TargetDir,[ManEntry|Manifest]).



pkg_terms(ReltoolConfig) ->
    %% grabbed from rebar_reltool
    OverlayVars0 = [{erts_vsn, "erts-" ++ erlang:system_info(version)}],

    %% Load up any variables specified by overlay_vars
    OverlayVars = case overlay_vars(ReltoolConfig) of
                      undefined ->
                          dict:from_list(OverlayVars0);
                      File ->
                          case file:consult(File) of
                              {ok, Terms} ->
                                  dict:from_list(OverlayVars0 ++ Terms);
                              {error, Reason2} ->
                                  ?ABORT("Unable to load overlay_vars "
                                         "from ~s: ~p\n",
                                         [File, Reason2])
                          end
                  end,
    OverlayVars.



%%
%% Render a binary to a string, using mustache and the specified context
%%
render(Bin, Context) ->
    ReOpts = [global, {return, list}],
    %% Be sure to escape any double-quotes before rendering...
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", ReOpts),
    Str1 = re:replace(Str0, "\"", "\\\\\"", ReOpts),
    mustache:render(Str1, Context).
