
.PHONY: docs
docs:
	dune build @doc
	open _build/default/_doc/_html/index.html
