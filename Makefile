all:
	dune build @install
	dune runtest
