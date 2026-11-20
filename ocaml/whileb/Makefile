OCAMLC = ocamlc

EXEC_SN  = tests_whileb
EXEC_SOS = tests_sos

OBJS_BASE = anacomb.cmo ast.cmo analyseur.cmo

all: $(EXEC_SN) $(EXEC_SOS)

$(EXEC_SN): $(OBJS_BASE) semantique_sn.cmo tests_whileb.ml
	$(OCAMLC) $(OBJS_BASE) semantique_sn.cmo tests_whileb.ml -o $(EXEC_SN)

$(EXEC_SOS): $(OBJS_BASE) etat.cmo interpreteur_sos.cmo tests_sos.ml
	$(OCAMLC) $(OBJS_BASE) etat.cmo interpreteur_sos.cmo tests_sos.ml -o $(EXEC_SOS)

anacomb.cmo: anacomb.ml
	$(OCAMLC) -c anacomb.ml

ast.cmo: ast.ml
	$(OCAMLC) -c ast.ml

analyseur.cmo: analyseur.ml anacomb.cmo ast.cmo
	$(OCAMLC) -c analyseur.ml

semantique_sn.cmo: semantique_sn.ml ast.cmo
	$(OCAMLC) -c semantique_sn.ml

etat.cmi: etat.mli ast.cmo
	$(OCAMLC) -c etat.mli

etat.cmo: etat.ml etat.cmi ast.cmo
	$(OCAMLC) -c etat.ml

interpreteur_sos.cmo: interpreteur_sos.ml ast.cmo etat.cmi
	$(OCAMLC) -c interpreteur_sos.ml

clean:
	rm -f *.cmi *.cmo *~ $(EXEC_SN) $(EXEC_SOS)