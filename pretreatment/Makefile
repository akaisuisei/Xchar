# TP sdl
 
OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
 
tpsdl: pretreatment.ml
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o pretreatment pretreatment.ml
 
clean::
	rm -f *~ *.o *.cm? pretreatment
 
# FIN
