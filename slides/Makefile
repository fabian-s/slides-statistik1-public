SLIDES = $(shell find -iname "*.Rmd")

all: texclean main.pdf main-embed.pdf

main.pdf: $(SLIDES)
	Rscript -e 'rmarkdown::render("main.Rmd")'

main-embed.pdf: main.pdf
	# embed fonts:
	gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE=main-embed.pdf -dBATCH main.pdf
	cp main-embed.pdf ~/Dropbox/statistik1-ex/main-embed.pdf

texclean:
	rm -rf *.html
	rm -rf *.md
	rm -rf *.out
	rm -rf *.dvi
	rm -rf *.log
	rm -rf *.aux
	rm -rf *.bbl
	rm -rf *.blg
	rm -rf *.ind
	rm -rf *.idx
	rm -rf *.ilg
	rm -rf *.lof
	rm -rf *.lot
	rm -rf *.toc
	rm -rf *.nav
	rm -rf *.snm
	rm -rf *.vrb
	rm -rf *.synctex.gz
	rm -rf *-concordance.tex

uncache:
	rm -rf *_cache
	rm -rf *_files

spelling: main.tex
	aspell -c -t main.tex -d de
