TARGET=report

all: pdf

report.pdf:
	#- bibtex main
	- pdflatex -shell-escape report.tex
	pdflatex -shell-escape report.tex

pdf: report.pdf

clean:
	rm -f *.eps
	rm -f *.aux *.log *.out *.bbl *.blg *~ *.bak $(TARGET).ps $(TARGET).pdf
	rm -f *.brf *.lof *.lot *.toc
	rm -rf _minted-$(TARGET)

# End
