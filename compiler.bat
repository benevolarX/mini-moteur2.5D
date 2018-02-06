
touch trigo.mli
touch options.mli
touch point.mli
touch segment.mli
touch bsp.mli
touch physic.mli
touch player.mli
touch parse_lab.mli
touch render.mli

ocamlc -i trigo.ml > trigo.mli
ocamlc -i options.ml > options.mli
ocamlc -i point.ml > point.mli
ocamlc -i segment.ml > segment.mli
ocamlc -i bsp.ml > bsp.mli
ocamlc -i physic.ml > physic.mli
ocamlc -i player.ml > player.mli
ocamlc -i parse_lab.ml > parse_lab.mli
ocamlc -i render.ml > render.mli

ocamlc -c trigo.mli
ocamlc -c options.mli
ocamlc -c point.mli
ocamlc -c segment.mli
ocamlc -c bsp.mli
ocamlc -c physic.mli
ocamlc -c player.mli
ocamlc -c parse_lab.mli
ocamlc -c render.mli

ocamlc -c trigo.ml
ocamlc -c options.ml
ocamlc -c point.ml
ocamlc -c segment.ml
ocamlc -c bsp.ml
ocamlc -c physic.ml
ocamlc -c player.ml
ocamlc -c parse_lab.ml
ocamlc -c render.ml
ocamlc -c main.ml

ocamlc str.cma graphics.cma trigo.cmo options.cmo point.cmo segment.cmo bsp.cmo physic.cmo player.cmo parse_lab.cmo render.cmo main.cmo -o projet_pfa.exe