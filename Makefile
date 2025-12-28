PKG ?= stringr

init:
	Rscript -e "install.packages('renv')"
	Rscript -e "renv::init()"


add:
ifndef PKG
	$(error Usage: make add PKG=nom_du_package)
endif
	Rscript -e "renv::install('$(PKG)')"
	Rscript -e "renv::snapshot"
status:
	Rscript -e "renv::status()"
install:
	Rscript -e "renv::activate()"

snapshot:
	Rscript -e "renv::snapshot()"
