%% This is the application resource file (.app file) for the mochiweb_app,
%% application.
{application, mochiweb_app, 
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [
		mwa_index,
        test
              ]},
   {registered,[]},
   {applications, [kernel, stdlib, mochiweb_otp]},
   {start_phases, []}]}.

