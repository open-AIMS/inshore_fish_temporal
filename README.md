# Inshore Fish Temporal Analyses
by Murray Logan

# About

# Building the docker container image

```{build docker, engine='bash', results='markdown', eval=FALSE}
make build
```

Alternatively,

```{build docker alt, engine='bash', results='markdown', eval=FALSE}
docker build . --tag inshore_fish
```

You can then confirm that the docker image has been built by looking
at a list of all docker images on your local machine.  There should be
a "REPOSITORY" called `inshore_fish` with a "TAG" of `latest`.

# Running R in the docker container (interactively)

```{run docker, engine='bash', results='markdown', eval=FALSE}
make R_container
```

Alternatively,

```{run docker alt, engine='bash', results='markdown', eval=FALSE}
docker run --rm -v -it "$(pwd):/home/Project" inshore_fish R
```

# Running the code in the docker container

```{run docker code container, engine='bash', results='markdown', eval=FALSE}
make code_container
```

Alternatively,

```{run docker code container alt, engine='bash', results='markdown', eval=FALSE}
docker run --rm -v -it "$(pwd):/home/Project" inshore_fish Rscript scripts/00_main.R
```

# Compiling documents in the docker container

```{run docker docs container, engine='bash', results='markdown', eval=FALSE}
make docs_container
```

Alternatively,

```{run docker docs container alt, engine='bash', results='markdown', eval=FALSE}
docker run --rm -v -it "$(pwd):/home/Project" inshore_fish Rscript docs/00_main.Rmd
```

# Running the code locally 

```{run docker code local, engine='bash', results='markdown', eval=FALSE}
make code_local
```

# Compiling the documents locally 

```{run docker docs local, engine='bash', results='markdown', eval=FALSE}
make docs_local
```

# Building singularity

Alternatively,

```{build singularity alt, engine='bash', results='markdown', eval=FALSE}
docker save inshore_fish -o inshore_fish.tar 
singularity build inshore_fish.sif docker-archive://inshore_fish.tar
```

# Running (executing) singularity

Alternatively, from within the scripts folder

```{run singularity alt, engine='bash', results='markdown', eval=FALSE}
singularity exec -B .:/home/Project inshore_fish.sif Rscript 00_main.R
```

# Running (executing) on the HPC
```{run singularity make, engine='bash', results='markdown', eval=FALSE}
module load singularity
singularity exec -B .:/home/Project inshore_fish.sif make -f scripts/Makefile
singularity exec -B .:/home/Project inshore_fish.sif make -f docs/Makefile
```

# Running through slurm on the HPC

From the project root on the HPC

```{slurm, engine='text', results='markdown', eval=FALSE}
#!/bin/bash
#SBATCH --job-name=fish_analysis
#SBATCH --ntasks=31
#SBATCH --partition=cpuq
#SBATCH --mem=200GB
#SBATCH --output=fish_analysis_%j.log

cd scripts
module load singularity
singularity exec -B .:/home/Project ../inshore_fish.sif Rscript 00_main.R 
```


```{run slurm, engine='bash', results='markdown', eval=FALSE}
squeue -la
squeue -o "%.18i %.9P %.8j %.8u %.8T %.10M %.8c %.4C %.9l %.6D %R"

sbatch job.slurm
```
