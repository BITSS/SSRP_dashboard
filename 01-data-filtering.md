---
title: "Data Cleaning"
author: "Fernando Hoces de la Guardia / Seung Yong Sung"
output: html_document
editor_options: 
  chunk_output_type: console
---

## Packages 



# Import Data

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# DI-Claim Level Data
di_cl_df <- read.csv("data/di_cl_df.csv")

# Reproduction Level Data
tidy_all_repro_df <- read.csv("data/tidy_all_repro_df.csv")

# Reproducer Data
tidy_reproducer_df <- read.csv("data/tidy_reproducer_dt.csv")
```

## Basic Data Manipulation
There is little cleaning done here, given data was cleaned for the paper.

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# Check for missing data in ID variables
di_cl_id_check <- di_cl_df %>% 
    filter((is.na(claim_N) | claim_N == "" | is.na(DI_N) | DI_N == "") & type == "FULL") %>% 
    nrow()
stopifnot(di_cl_id_check == 0)

repro_paper_id_check <- tidy_all_repro_df %>% 
    filter(is.na(reproduction_id) | reproduction_id == "" | is.na(doi) | doi == "") %>% 
    nrow() 
stopifnot(repro_paper_id_check == 0)
```

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# How many observations? (Using data from Jan 12, 2025)
nrow(di_cl_df) #903 DI_Claim obs
```

```
[1] 904
```

```r
nrow(tidy_all_repro_df) #321 Reproduction obs
```

```
[1] 322
```

```r
# Counts: DIs

    # DI from completed repo (833)
    di_cl_df %>% 
        filter(type == "FULL") %>%
        select(reproduction_id, claim_N, DI_N) %>%
        unique() %>% 
        nrow()
```

```
[1] 833
```

```r
    # DI from abandoned repo (10)
    di_cl_df %>% 
        filter(type == "ABANDONED") %>%
        select(reproduction_id, claim_N, DI_N) %>%
        unique() %>% 
        nrow()
```

```
[1] 10
```

```r
    # DI from minimum repo (60)
    di_cl_df %>% 
        filter(type == "MINIMUM") %>%
        select(reproduction_id, claim_N, DI_N) %>%
        unique() %>% 
        nrow()  
```

```
[1] 61
```

```r
# Counts: Claims

    # Claims from completed repo (502)
    di_cl_df %>% 
        filter(type == "FULL") %>%
        select(reproduction_id, claim_N) %>%
        unique() %>% 
        nrow()
```

```
[1] 502
```

```r
    # Claims from abandoned repo (10)
    di_cl_df %>% 
        filter(type == "ABANDONED") %>%
        select(reproduction_id, claim_N) %>%
        unique() %>% 
        nrow()    
```

```
[1] 10
```

```r
    # Claims from minimum repo (60)
    di_cl_df %>% 
        filter(type == "MINIMUM") %>%
        select(reproduction_id, claim_N) %>%
        unique() %>% 
        nrow()    
```

```
[1] 61
```

```r
# Counts: Reproductions
    
    # Completed Reproductions (251)
    tidy_all_repro_df %>%
        filter(type == "FULL") %>%
        select(reproduction_id) %>%
        unique() %>% 
        nrow()    
```

```
[1] 251
```

```r
    # Abandoned Reproductions (10)
    tidy_all_repro_df %>%
        filter(type == "ABANDONED") %>%
        select(reproduction_id) %>%
        unique() %>% 
        nrow()       
```

```
[1] 10
```

```r
    # Minimum Reproductions (60)
    tidy_all_repro_df %>%
        filter(type == "MINIMUM") %>%
        select(reproduction_id) %>%
        unique() %>% 
        nrow()   
```

```
[1] 61
```

```r
# Counts: Papers
    
    # Papers from completed reproductions (179)
    tidy_all_repro_df %>%
        filter(type == "FULL") %>%
        select(doi) %>%
        unique() %>% 
        nrow()    
```

```
[1] 179
```

```r
    # Papers from abandoned reproductions (8)
    tidy_all_repro_df %>%
        filter(type == "ABANDONED") %>%
        select(doi) %>%
        unique() %>% 
        nrow()     
```

```
[1] 8
```

```r
    # Papers from minimum reproductions (44)
    tidy_all_repro_df %>%
        filter(type == "MINIMUM") %>%
        select(doi) %>%
        unique() %>% 
        nrow()     
```

```
[1] 45
```

```r
    # Papers from all reproductions (214) - This doesn't need to necessarily be additive of above three above.
    tidy_all_repro_df %>%
        select(doi) %>%
        unique() %>% 
        nrow()    
```

```
[1] 215
```









##### ##### ##### ##### ##### ##### ##### #####
## ARCHIVE OF PREVIOUS DATA MANIPULATION ##
##### ##### ##### ##### ##### ##### ##### #####

## Data: Copy tidy df from the `SSRP_cleanining` repo into `SSRP_dashboard` repo. 

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# clean_path <- Sys.getenv("clean_path") # Use this for other users
# clean_path <- gsub("\\\\", "/", clean_path) # Replaces double-backslash with a single forward slash (only Window users should be affected)

# tidy_dfs <- list.files(paste0(clean_path,"/processed/"))
# file.copy(paste0(clean_path,"/processed/",tidy_dfs),
#           paste0("./processed/",tidy_dfs),
#           overwrite = TRUE)
```

## Data: Import
- Load data  
- Rename the title of a reproduction  
- Generate a variable that takes the same value for titles that sound similar  
- Generate unique identifier for each reproduction  
- Identify the maximum number of claims and display items reproduced across all reproduction attempts

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# dis_df <- read_csv("./processed/tidy_di_df.csv") %>%
#     mutate(
#         paper_title = ifelse(
#             test = # Fixing titles
#                 ( paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
#                 ),
#             yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
#             no = paper_title
#         ),
#         similar_title = phonetic(paper_title)
#     ) %>%
#     filter(!is.na(repro_score))
```

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# claims_df <- read_csv("./processed/tidy_claim_df.csv") %>%
#     mutate(paper_title= ifelse(
#         test = # Fix Fernando's title
#             (
#                 paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
#             ),
#             yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
#             no = paper_title
#         ),
#         similar_title = phonetic(paper_title)
#     ) # 98 reproductions (no abandoned)
```

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
    # abandoned_df <- read_csv("./processed/tidy_abandoned_df.csv") %>%
    # mutate(paper_title= ifelse(
    #     test = # Fix Fernando's title
    #         (
    #             paper_title == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
    #         ),
    #         yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
    #         no = paper_title
    #     ),
    #     similar_title = phonetic(paper_title)
    # )  # 104 reproductions total
```

## Data: Cleaning

Make sure there's at least one scored Display Item.
The rationale for this filter is that papers should have some sort of reproducibility score associated with them to be counted.

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

```r
# has_score <- dis_df %>%
#     select(reproduction_id) %>%
#     unique()
# 
# claims_df <- claims_df %>%
#     inner_join(has_score,
#                by="reproduction_id")
```
