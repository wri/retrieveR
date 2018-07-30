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

The format for querying the corpus and generating a report is interactive and iterative.
After running the above code, retrieveR prompts the user with a set of words and phrases that are determined as relevant. The ones for "barriers to restoration", for instance, are as follows:

```r
barriers                     restoration                  obstacles                    overcome                    
restoration"                 ecological_restoration       restoring                    opportunities               
restoration_projects         forest_restoration           constraints                  bottlenecks                 
interventions                ecological                   restoration_activities       incentives                  
flr                          forest_landscape             forest_landscape_restoration landscape_restoration       
identifying                  restoration_interventions    barrier                      restoration_at              
successful                   restoration_efforts          impediments                  options                     
approaches                   key_success_factors          overcoming                   streambank                  
projects                     economic_incentives          enabling_conditions          strategies                  
landscapes                   barriers                     successful_restoration       forest_and_landscape        
landscape                    challenges                   scale_up                     passive                     
solutions                    potential                    aronson                      scaling_up                  
removing                     identify                    
```

