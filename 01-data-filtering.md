---
title: "Data Cleaning"
author: "Fernando Hoces de la Guardia"
output: html_document
editor_options: 
  chunk_output_type: console
---




Copy cleanned data sets (tidy df) from the `SSRP_cleanining` repo into this repo. 

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
tidy_dfs <- list.files(paste0(clean_path,"/processed/2023_5_23/"))
file.copy(paste0(clean_path,"/processed/2023_5_23/",tidy_dfs), 
          paste0("./processed/",tidy_dfs),
          overwrite = TRUE)
```



```
 [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```



### First: basic clean up  
- load data  
- rename the title of a reproduction  
- generate a variable that takes the same value for titles that sound similar  
- ?generate unique identifier for each reproduction  
- ?Identify the maximum number of claims and display items reproduced across all reproduction attempts

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
dis_df <- read_csv("./processed/tidy_di_df.csv") %>%
    mutate(
        paper_title = ifelse(
            test = # Fixing titles
                ( paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
                ),
            yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
            no = paper_title
        ),
        similar_title = phonetic(paper_title)
    ) %>%
    filter(!is.na(repro_score))
```



- Same but to claims-level data

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
claims_df <- read_csv("./processed/tidy_claim_df.csv") %>%
    mutate(paper_title= ifelse(
        test = # Fix Fernando's title
            (
                paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
            ),
            yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
            no = paper_title
        ),
        similar_title = phonetic(paper_title)
    ) # 98 reproductions (no abandoned)
```



- Same to abandoned papers

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
    abandoned_df <- read_csv("./processed/tidy_abandoned_df.csv") %>%
    mutate(paper_title= ifelse(
        test = # Fix Fernando's title
            (
                paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
            ),
            yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
            no = paper_title
        ),
        similar_title = phonetic(paper_title)
    )  # 104 reproductions total
```



# Make sure there's at least one scored Display Item

The rationale for this filter is that papers should have some sort of reproducibility score associated with them to be counted.

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
has_score <- dis_df %>%
    select(reproduction_id) %>%
    unique()

claims_df <- claims_df %>%
    inner_join(has_score,
               by="reproduction_id")
```




# Future Filters

This section explains how to add a filter in the future. Additional features should have a title (e.g. "Make sure there's at least one estimate associated with each claim") followed by a description of the reason we want this filter.

Code for the filter should reduce the number of observations in `claims_df`, `dis_df`, and/or `abandoned_df`, writing over the old df.


