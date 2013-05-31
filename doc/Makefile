all: doc

# DOCS = staapl.html pic18-interaction.html pic18-forth.pdf
# pic18-synth.pdf forth.html
DOCS = staapl.html 

doc: $(DOCS)
# brood-introduction.html

.SUFFIXES: .scrbl .html .tex .dvi .ps .pdf

.tex.dvi:
	bash latex-bibtex latex $*.tex $*

.dvi.ps:
	dvips $*.dvi

.tex.pdf:
	bash latex-bibtex pdflatex $*.tex $*

.scrbl.html: 
	scribble --html $<

.scrbl.tex:
	scribble --latex $<

clean:
	rm -f staapl.txt staapl.html *~ scribble-common.js scribble.css *.pdf *.aux *.log

RSYNC = rsync --archive --update --delete --numeric-ids --verbose --one-file-system --progress --rsh=ssh

# don't gen docs on webserver
publish: doc
	$(RSYNC) scribble.css scribble-common.js $(DOCS) giebrok:www/zwizwa.be/archive/

