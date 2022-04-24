---
title: "Data Cleaning"
author: "Fernando Hoces de la Guardia"
date: "7/30/2021"
output: html_document
editor_options: 
  chunk_output_type: console
---



<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
tidy_dfs <- list.files(paste0(clean_path,"/processed/"))
file.copy(paste0(clean_path,"/processed/",tidy_dfs), 
          paste0("./processed/",tidy_dfs),
          overwrite = T)
```



```
 [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```



### First: basic clean up  
- load data  
- clean some DOIs  
- rename the title of a reproduction  
- generate a variable that takes the same value for titles that sound similar  
- generate unique identifier for each reproduction  
- Identify the maximum number of claims and display items reproduced across all reproduction attempts

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
dis_df <- read_csv("./processed/tidy_di_df.csv") %>%
    mutate(paper_title= ifelse(
        test = # Fixing titles
            (
                paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
            ),
            yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
            no = paper_title
        ),
        similar_title = phonetic(paper_title)
    ) %>%
    filter(!is.na(repro_score))
```



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



# Make sure there's at least one estimate associated with each claim

The rationale for this filter is that claims should have at least some estimate in the paper associated with them.

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
ests <- read_csv("./processed/tidy_est_df.csv") %>%
    filter(!is.na(spec_est),
           !is.na(claim_N)) %>%
    select(reproducer_id,reproduction_id,claim_N) %>%
    unique()

claims_df <- inner_join(claims_df,ests,
                        by=c("reproducer_id","reproduction_id",
                             "claim_N"))
```



# Future Filters

This section explains how to add a filter in the future. Additional features should have a title (e.g. "Make sure there's at least one estimate associated with each claim") followed by a description of the reason we want this filter.

Code for the filter should reduce the number of observations in `claims_df`, `dis_df`, and/or `abandoned_df`, writing over the old df.
