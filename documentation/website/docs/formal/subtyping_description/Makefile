DOC=pyre_sub

all: $(DOC).pdf

$(DOC).pdf: $(DOC).tex
	rubber -d $^

watch:
	sh -c "mupdf-gl $(DOC).pdf 2>/dev/null >&2 & jobs -p %1 > .mupdf.pid"
	fswatch $(DOC).tex | xargs -n1 sh -c 'make && echo "Refreshing..." &&  kill -n 1 `cat .mupdf.pid`'

clean:
	rm -f *.aux *.dvi *.log

.PHONY: watch clean
