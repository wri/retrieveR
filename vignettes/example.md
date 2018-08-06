## Overview

In this example, we analyze 63 policy documents from Kenya, Rwanda, and Malawi to see where and how public participation in governance is supported.


#### Code
```r
library(devtools)
install_github("wri/retrieveR")
library(retrieveR)

download_embeddings()
create_virtualenv()
run_ocr("policy-documents/")
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

#### Interim results

#### Results

