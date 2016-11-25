main:
	ocamlbuild -pkgs oUnit,yojson,str,ANSITerminal,cohttp.lwt main.byte && ./main.byte