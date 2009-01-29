%% dubdub app resource file

{application, dubdub,
 [{description, "a database"},
  {vsn, "0.1.0"},
  {modules, [dubdub_app, dubdub_sup, admin, process_dictionary, randoms, db_sup_or, db_node, db]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {dubdub_app, []}},
  {start_phases, []}
 ]}.