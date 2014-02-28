PANDOC := pandoc -S -f markdown+lhs -t html -c style.css
CONCEAL := sed -f conceal.sed

all: Lenses.html

clean:
	rm -f *.html

%.html: %.lhs
	cat $< | $(CONCEAL) | $(PANDOC) > $@
