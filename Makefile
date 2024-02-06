# help:
# 	@echo "Usage: make -i SRC=<path/file> -> to make a specific file"
# 	@echo "       make -i                 -> to make all altered files"

.PHONY: build run code docs

build:
	docker build . --tag inshore_fish

# Run interactive R session in docker container
R_container:
	docker run --rm -it -v "$(shell pwd)":/home/Project inshore_fish R

code_container:
	docker run --rm -v "$(shell pwd)":/home/Project inshore_fish $(MAKE) -f scripts/Makefile

docs_container:
	docker run --rm -v "$(shell pwd)":/home/Project inshore_fish $(MAKE) -f docs/Makefile

# docker run --rm -v "$(pwd):/home/Project" inshore_fish $(MAKE) -f docs/Makefile

code_local:
	$(MAKE) -f scripts/Makefile

docs_local:
	$(MAKE) -f docs/Makefile

code_singularity:
	$(MAKE) -f scripts/Makefile singularity

docs_singularity:
	@echo "Transfer to docs/Makefile"
	#module load singularity
	singularity exec -B .:/home/Project inshore_fish.sif $(MAKE) -f docs/Makefile
	#i$(MAKE) -f docs/Makefile singularity

clean:
	rm -f *.log *.aux *.md *.out texput.log
