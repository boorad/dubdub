%% dubdub app resource file

{application, dubdub,
 [{description, "a database"},
  {vsn, "0.1.0"},
  {modules, [dubdub_app, dubdub_sup, admin, randoms, db_node, db]},
  {registered, [db_node]},
  {applications, [kernel, stdlib]},
  {mod, {dubdub_app, []}},
  {start_phases, []}
 ]}.