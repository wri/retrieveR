# retrieveR

## Overview

retrieveR is a system for automating information retrieval from a corpus of documents. 

## Installation

If you have not used devtools before in R, you must install it by running

```r
install.packages("devtools")
```

Devtools relies on the most current version of Rtools to also be [installed](https://cran.r-project.org/bin/windows/Rtools/). I am currently working on a release update that will not depend on Rtools. 

```r
library(devtools)
install_github("wri/retrieveR")
```

If the installation of retrieveR fails because R cannot find Rtools, you can force R to find it by running the below code.

```r
library(devtools)
assignInNamespace("version_info", c(devtools::version_info, list("3.5" = list(version_min = "3.3.0", version_max = "99.99.99", path = "bin"))), "devtools")
```


## Usage

There are three main functions that comprise the bulk of the functionality of retrieveR: `make_corpus`, `create_locations`, and `interactive_report`. 

### make_corpus

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

### create_locations

```r
locations <- create_locations(corpus, "path/to/embeddings.bin")
```

This function uses a pre-trained neural embedding to calculate weights for each paragraph in the corpus. Calling `download_embeddings()` will download a pre-trained embedding to the working directory as `embeddings.bin`. This pre-trained embedding was trained on over 1,000 environmental policy documents from more than 40 nations and 50 NGOs and development aid agencies. The `prep_wordvec` and `create_wordvec` functions may be used to create your own neural embedding.

### interactive_report

```
interactive_report(country = "Kenya", query = "barriers to restoration",
  data = corpus, embeddings = "path/to/embedding", locations = locations)
```

The format for querying the corpus and generating a report is interactive and iterative.
After running the above code, retrieveR prompts the user with a set of words and phrases that are determined as relevant. The ones for "barriers to restoration", for instance, are as follows:

```r
barriers                     restoration                  obstacles                            
restoration"                 ecological_restoration       restoring                          
restoration_projects         forest_restoration           constraints                        
interventions                ecological                   restoration_activities                  
flr                          forest_landscape             forest_landscape_restoration       
identifying                  restoration_interventions    barrier                        
bottlenecks                  restoration_efforts          impediments                      
approaches                   key_success_factors          overcoming                 
projects                     economic_incentives          enabling_conditions              
landscapes                   barriers                     successful_restoration            
landscape                    challenges                   scale_up                     
solutions                    landscape_restoration        overcome                            
removing                     identify                    
```

At this stage, you can add or remove words that you find relevant or not-relevant to your query. It is important to note that adding words to your query at this point will help the algorithm better understand what you are looking for. It is easier to draw a plane in 300 dimensional space with 6 points than it is with 2 or 1.

After finalizing a query, the function returns all paragraphs ranked by their cosine similarity. The final step is to determine the cutoff threshold for inclusion. This varies widely between queries - broad queries have a lower threshold than narrow queries - and thus requires user input. 

To do this, the algorithm begins with a high threshold (only retaining very similar paragraphs). The user is presented with the two paragraphs that are just barely not retained, and then prompted to determine whether they are relevant. If they are, the threshold is lowered and the process is repeated until no relevant paragraphs are missed.

Finally, `interactive_report` makes use of `rmarkdown` and `ggplot2` to create a heatmap of topic density by document and a report listing each relevant paragraph sorted by document and labelled with its page number. 

## Examples

See the vignette [here](https://github.com/wri/retrieveR/blob/master/vignettes/example.md).
