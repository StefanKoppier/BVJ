
default:presentation.pdf

verbose:presentation.md
	pandoc -t beamer presentation.md -o presentation.pdf --pdf-engine=xelatex --verbose --template beamer-template.tex

presentation.pdf:presentation.md
	pandoc -t beamer presentation.md -o presentation.pdf --pdf-engine=xelatex --template beamer-template.tex

