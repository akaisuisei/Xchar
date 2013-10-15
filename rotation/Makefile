# TP sdl
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
rotattion: rotattion.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o rotation rotattion.ml
 
clean:
	rm -f *~ *.o *.cm? rotattion
 
# FIN
