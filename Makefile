## Build a single Rmd file

## targets
TARGET = Article

RMD    = $(TARGET).Rmd
PDF    = $(TARGET).pdf
SLIDY  = $(TARGET).html

.DEFAULT_GOAL := pdf

all: p1 pdf slidy

## will build default output from yaml
## using  rmarkdown::beamer_presentation output doesnt work well may need yaml file
pdf: $(PDF)
$(PDF): $(RMD)
	Rscript -e "rmarkdown::render( '$?' )"
	@echo "Building: $@"
	@echo "Changed:  $?"
	setsid evince    $@ &

## not working perfect
slidy: $(SLIDY)
$(SLIDY): $(RMD)
	Rscript -e "bookdown::render_book( '$?' , 'rmarkdown::html_document')"
	@echo "Building: $@"
	@echo "Changed:  $?"
	# setsid mimeopen  $@ &


p1: DHI_GHI_longterm_trends.R
DHI_GHI_longterm_trends.R: DHI_GHI_longterm_trends.pdf
	Rscript -e "rmarkdown::render( '$?' )"
	@echo "Building: $@"
	@echo "Changed:  $?"
	setsid evince    $@ &

