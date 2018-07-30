# retrieveR

## Overview

retrieveR is a system for automating information retrieval from a corpus of documents. 

## Installation

```r
install_github("wri/retrieveR")
```


## Usage

There are three main functions that comprise the bulk of the functionality of retrieveR.

### make_corpus.R

```r
corpus <- make_corpus("path/to/folder")
```

This function wraps a number of helper functions that serve to accomplish the following:
 
+ Split documents by paragraph
+ Remove tables, figures, and citations
+ Subset documents to english
+ Fix spelling errors in a contextually-sensitive manner
+ Bundle common phrases into n-grams
+ Extract page number from each paragraph
+ Assemble a dataframe specifying the paragraph and any relevant metadata

### create_locations.R

```r
locations <- create_locations(corpus, "path/to/embedding")
```

This function uses a pre-trained neural embedding to calculate weights for each paragraph in the corpus. Functions will be included in future versions to download pre-trained embeddings. Currently, the `prep_wordvec` and `create_wordvec` functions must be used to create an embedding.

### interactive_report.R

```
interactive_report(country = "Kenya", query = "barriers to restoration",
  data = corpus, embeddings = "path/to/embedding", locations = locations)
```


