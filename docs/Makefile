DOCS_SRC_DIR  = $(addprefix , docs)
ifdef FILE
DOCS_FILE = $(DOCS_SRC_DIR)/$(FILE)
else
DOCS_FILE ?= $(foreach sdir, $(DOCS_SRC_DIR), $(wildcard $(sdir)/*.Rmd))
endif

HTML_FILE := $(patsubst %.Rmd, %.html, $(DOCS_FILE))

$(info ************************************)
$(info DOCS Source directory:     $(DOCS_SRC_DIR))
$(info DOCS Source files:         $(DOCS_FILE))
$(info ************************************)

.PHONY: singularity

$(HTML_FILE) : %.html : %.Rmd
	@echo "Compiling documntation for analyses"
	$(info Source = $<; Destination = $@)
	echo "library(rmarkdown); render(\"$<\", output_format = \"html_document\")" | R --no-save --no-restore;

singularity : $(DOCS_FILE)
	@echo "Compilinng documentation for analysis with singularity"
	$(info Source = $<; Destination = $@)
	@echo "module load singularity"
	cd docs; ls -la;
	cd docs; singularity exec -B .:/home/Project ../inshore_fish.sif echo "library(rmarkdown); render(\"$<\", output_format = \"html_document\")" | R --no-save --no-restore;
