{application, cherly,
  [{description, "Cherly caching library"},
   {mod, {cherly_app, []}},
   {vsn, "?VERSION"},
   {modules, [cherly, cherly_app]},
   {registered, []},
   {applications, [kernel, stdlib]}
  ]}.