EdmistonDerexLupyan-CogSci18.pdf: slides.Rmd iterated-problem-solving.R slides.tmpl
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
notes.pdf: slides.Rmd
	sed -e 's/^<!--//' -e 's/^-->//' $< > notes.Rmd
	Rscript -e 'rmarkdown::render("notes.Rmd", output_format = "pdf_document", output_file = "$@")'
iterated-problem-solving.pdf: iterated-problem-solving.Rmd iterated-problem-solving.R cogsci.tmpl cogsci.sty references.bib
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
clean:
	rm -rf *_files *_cache
