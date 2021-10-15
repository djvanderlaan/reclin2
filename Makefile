
.PHONY: build check document install

build: document
	cd work && R CMD build ../

check: build
	cd work && R CMD check --as-cran `ls reclin2*.tar.gz | sort | head -n 1`

document:
	R -e "roxygen2::roxygenise()"

install: build
	R CMD INSTALL `ls work/reclin2*.tar.gz | sort | head -n 1` 

 
