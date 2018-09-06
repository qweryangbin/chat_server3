{application, chat_server3,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, { chat_server3_app, []}},
  {applications,
   [kernel,
    stdlib,
    cowboy,
    gpb
   ]},
  {env,[{http_port, 8080}]},
  {modules, []},

  {maintainers, []},
  {licenses, ["Apache 2.0"]},
  {links, []}
 ]}.
