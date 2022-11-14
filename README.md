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
