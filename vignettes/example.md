# Overview

```r
library(devtools)
install_github("wri/retrieveR")
library(retrieveR)

download_embeddings()
corpus <- make_corpus("policy-documents/")
locations <- create_locations(corpus, "embeddings.bin")
```

```r
interactive_report(country = "Kenya",
                   query = "public participation in governance",
                   data = corpus,
                   embeddings = "embeddings.bin",
                   locations = locations)
```
