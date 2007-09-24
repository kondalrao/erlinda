%%% -*- mode:erlang -*-
%%%$CVS$
{application, fslib,
 [
  % A breif descrition of the application.
  {description, "Further Standard library modules"},

  % The version number of the application.
  {vsn, "5.1.0"},

  % The modules that the application contains.
  {modules,
   [
	fs_boot_smithe,
	fs_elwrap_h,
	fs_email,
	fs_fprof_util,
	fs_lib,
	fs_lists,
	fs_net,
	fs_release_smithe,
	fs_string,
	fs_test_lib,
	fs_time
   ]},

  % All of the names that an application registers - avoids name clashes.
  {registered, []},

  % Applications that this one depends on.
  {applications,[kernel, stdlib]}
 ]
}.

