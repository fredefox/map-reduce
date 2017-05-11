all: report

dist: report archive
	true

report: README.md
	pandoc README.md -o report.pdf \
	  --latex-engine=xelatex \
	  --variable urlcolor=cyan \
	  -V papersize:"a4paper"

archive:
	tar --transform "s/^/map-reduce\//" \
		-cvf map-reduce.tar \
		src/crawl.erl \
		src/dist_map_reduce.erl \
		src/main.erl \
		src/map_reduce.erl \
		src/page_rank.erl \
		src/web.dat \
		BACKLOG.md \
		Makefile \
		report.pdf

preview: report
	xdg-open report.pdf

clean:
	rm src/*.beam
