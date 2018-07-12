EdmistonDerexLupyan-CogSci18.pdf: slides.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
iterated-problem-solving.pdf: iterated-problem-solving.Rmd iterated-problem-solving.R cogsci.tmpl cogsci.sty references.bib
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
