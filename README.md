# retrieveR

## Overview

retrieveR is a system for automating information retrieval from a corpus of documents. 

## Installation instructions

R can be downloaded from [this link](https://cran.r-project.org/bin/windows/base/). Once it is downloaded, open up the 32-bit version (i386, as WRI computers only seem to have 32-bit version of Java). Then, you can proceed to installing the package by running the following lines of code. Copy and paste them one at a time and press enter.

```r
install.packages("devtools")
library(devtools)
install_github("wri/retrieveR")
```

## Downloading data

Next, we load up the package into R using `library`. Depending on your operating system, you then need to run either `install_mac` or `install_windows` - these functions will get the Java dependencies to extract text from images, as well as install the necessary components to run neural networks.

Finally, the `download_example` function will download the example PDFs.

```
library(retrieveR)
install_mac()
install_windows()
download_example()
```

## Prepping documents for querying

The `prep_documents` function will strip text from the PDFs, clean up the results, and calculate neural weights. These can be turned off by specifying `ocr = F`, `clean = F`, or `weights = F`. retrieveR can process html documents by setting `type = "html"`.The function takes a path to the folder of documents - in this case they are stored in a folder called `pdfs`. This pathing is local to the directory that R is running in - this can be printed with `getwd()` and changed with `setwd()`. 

```r
corpus <- prep_documents("pdfs")
```

## Querying documents

The `create_report` function takes the following arguments:

1. query: Query phrase within quotations.
2. data: name that the output of `prep_documents` is stored to.

This uses a pre-trained neural embedding to calculate weights for each paragraph. Calling `download_embeddings()` will download a pre-trained embedding to the working directory as `embeddings.bin`. This pre-trained embedding was trained on over 1,000 environmental policy documents from more than 40 nations and 50 NGOs and development aid agencies. 

The `prep_wordvec` and `create_wordvec` functions may be used to create your own neural embedding, if need be.

```r
create_report(country = "Kenya", query = "barriers to restoration",
  data = corpus)
```

The format for querying the corpus and generating a report is interactive and iterative. RetrieveR prompts the user with a candidate set of relevant words and phrases. The ones for "barriers to restoration", for instance, are:

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

At this stage, you can add words that you find relevant to your query. After finalizing a query, paragraphs are ranked by their cosine similarity. The final step is to determine the cutoff threshold for inclusion. This varies widely between queries - broad queries have a lower threshold than narrow queries - and thus requires user input. 

To do this, the algorithm begins with a high threshold (only retaining very similar paragraphs). The user is presented with the two paragraphs that are just barely not retained, and then prompted to determine whether they are relevant. If they are, the threshold is lowered and the process is repeated until no relevant paragraphs are missed.

Finally, `create_report` makes use of `rmarkdown` and `ggplot2` to create a heatmap of topic density by document and a report listing each relevant paragraph sorted by document and labelled with its page number. 

## Examples

See the vignette [here](http://htmlpreview.github.io/?https://raw.githubusercontent.com/wri/retrieveR/master/demo.html).
