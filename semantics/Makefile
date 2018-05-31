TARGET := mtag

.PHONY: all clean

all: $(TARGET).pdf

clean:
	rm -f $(TARGET).fdb_latexmk $(TARGET).log $(TARGET).aux \
		$(TARGET).fls $(TARGET).pdf

%.pdf: %.tex
	latexmk --pdf $^
