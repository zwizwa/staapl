
# This file resides here to not pollute the collects dir.

DOC = staapl.pdf staapl.html
# DOC = staapl.pdf
# DOC = staapl.html

all: $(DOC)

.SUFFIXES: .html .scrbl .pdf

%.html: ../../staapl/scribblings/%.scrbl
	scribble --html $<

%.pdf: ../../staapl/scribblings/%.scrbl
	scribble --pdf $<


publish: $(DOC)
	scp $(DOC) giebrok:www/zwizwa.be/archive/

clean:
	rm -f *~ *.html *.js *.css *.pdf
