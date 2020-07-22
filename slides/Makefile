slides = glm

$(slides).pdf: $(slides).Rmd preamble.tex
	Rscript -e "rmarkdown::render(input = '$(slides).Rmd', output_file = '$(slides).pdf')"
	
clean:
	rm -rf $(slides)_files
	rm -rf $(slides).tex
	rm -rf $(slides).log
	rm -f missfont.log
	
mrproper: clean
	rm -rf $(slides).pdf