{application, prp,
 [
  {description, "prp kata, 2nd"},
  {vsn, "0.1.0"},
  {modules, [prp_app, prp_schema, prp_handler, prp_sup, prp_resource, prp_paper]},
  {registered, [prp_sup]},
  {applications, [
                  kernel,
                  stdlib,
		  cowboy,
		  mnesia
                 ]},
  {mod, { prp_app, []}},
  {env, []}
 ]}.
