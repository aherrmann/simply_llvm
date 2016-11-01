.PHONY: all view clean

all: slides.html

view: all
	xdg-open slides.html

clean:
	rm -f slides.html

slides.html: slides.md
	pandoc -t slidy --standalone --self-contained \
		$^ -o $@
