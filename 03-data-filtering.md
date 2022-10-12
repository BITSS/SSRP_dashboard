---
title: "Data Cleaning"
author: "Fernando Hoces de la Guardia"
output: html_document
editor_options: 
  chunk_output_type: console
---



<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
tidy_dfs <- list.files(paste0(clean_path,"/processed/"))
file.copy(paste0(clean_path,"/processed/",tidy_dfs), 
          paste0("./processed/",tidy_dfs),
          overwrite = TRUE)
```



```
 [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
[13] FALSE FALSE
```



### First: basic clean up  
- load data  
- clean some DOIs  
- rename the title of a reproduction  
- generate a variable that takes the same value for titles that sound similar  
- generate unique identifier for each reproduction  
- Identify the maximum number of claims and display items reproduced across all reproduction attempts









