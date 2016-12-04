Camel Talk - Command line Slack-like application

Build Instructions
1. Set the server to the host you want to connect to in main_config.txt
(when running/developing locally can just set to "http://127.0.0.1:8000")
2. Startup the server first - make server
3. Then run the application! - make main

If necessary, run the following commands to install packages. In case
other packages are missing, just use opam to install the prompted
packages on the command line.
opam install cohttp lwt
opam install extlib
