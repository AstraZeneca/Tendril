help:
	@echo "make (env|test|unittest|build)"

env:
	Rscript -e "options(repos = \"http://cran.r-project.org/\"); packrat::init(); install.packages(\"knitr\")"

test:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::check()"
	Rscript -e "devtools::test()"

unittest:
	Rscript -e "devtools::test()"

build:
	Rscript -e "devtools::document()"
	Rscript -e "devtools::build('.')"
