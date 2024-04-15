## -------------------- --------------------
##
## QUICK FIX TO ACHIEVE CONSISTENCY
##   
##
## -------------------- --------------------






## -------------------- --------------------

library(pacman) # pacman repository has p_load
p_load(here, base, readr, data.table, tidyverse, dplyr, ggplot2, ggbreak, ggthemes,
       splitstackshape, here, spatstat, knitr, kableExtra, rmarkdown, tinytex, ezknitr,
       filecontents)

library(shiny)
library(tidyverse)
library(stringdist)

## -------------------- --------------------
##
## Script name: 01_Functions.R
##
##



## -------------------- Function 1: SplitFunction --------------------

## Function: Delete duplicate items in a list within a cell
## (i.e. If a given cell in df contains a list with duplicate items within the list, remove the duplicates
## Hypothetical example: "[J12, K03, J12, K03]" -> "J12, K03")
SplitFunction <- function(x) {
    b <- unlist(strsplit(gsub("\\[|\\]|\\'|\\s", '', x), ',')) # Remove [, ], ', space; Then, split at commas; flatten into vector
    c <- b[!duplicated(b)]         # Remove duplicates within the vector
    return(paste(c, collapse=",")) # Concatenate into single string, placing comma in between each non-duplicate item 
}

## -------------------- Function 2: RemoveNAs --------------------

## Function for removing columns with all NAs
RemoveNAs <- function(df) {
    df <- df %>% select(where(~!all(is.na(.x)))) %>% as.tibble() # In df, remove columns that are completely NA
    return(df)
}

## -------------------- Function 3: str_cleaning --------------------

## Function for cleaning string columns (i.e. capitalize all letters, trim unnecessary spaces)
str_cleaning <- function(df) {
    df <- df %>% 
        mutate(across(where(is.character), str_to_upper),     # Capitalize First Letter of Each Word
               across(where(is.character), str_trim),         # Remove outer spaces
               across(where(is.character), str_squish)) %>%   # Remove repeated inner spaces
        as.tibble()
    return(df)
}

## -------------------- Function 4: clean_journal_names --------------------

# Function for cleaning journal names
clean_journal_names <- function(df) {
    if ("crossref_journal" %in% colnames(df)) {
        df <- df %>%
            mutate(journal = crossref_journal) %>%
            select(-crossref_journal) %>%
            as.tibble()
        return(df)
    }
    if ("journal" %in% colnames(df)) {
        df <- df %>%
            # Remove: "THE", ",", ";", "&AMP", "&", "APPENDICES"
            mutate(journal = sub("THE ", "", journal),
                   journal = sub(",", "", journal),
                   journal = sub(";", "", journal),
                   journal = sub("&AMP", "", journal),
                   journal = sub("&", "", journal),
                   journal = sub("APPENDICES", "", journal)) %>%
            # Remove: Outer and repeated inner spaces
            mutate(journal = str_trim(journal),
                   journal = str_squish(journal)) %>%
            # Unify Journal Names Across Multiple Data Sources (IDEAS/RePeC, SSRP, and Crossref) For Merging Purposes
            # First Three: Remove unnecessary text | Next Six: Writing out full names of journals
            mutate(journal = gsub(".*FOUNDATIONS AND TRENDS.*IN ECONOMETRICS", "FOUNDATIONS AND TRENDS IN ECONOMETRICS",journal),
                   journal = gsub(".*FOUNDATIONS AND TRENDS.*IN MICROECONOMICS", "FOUNDATIONS AND TRENDS IN MICROECONOMICS",journal),
                   journal = gsub(".*FOUNDATIONS AND TRENDS.*IN FINANCE", "FOUNDATIONS AND TRENDS IN FINANCE",journal),
                   journal = ifelse(journal == "AM POLIT SCI REV","AMERICAN POLITICAL SCIENCE REVIEW",journal),
                   journal = ifelse(journal == "CANADIAN J OF ECONOMICS","CANADIAN JOURNAL OF ECONOMICS",journal),
                   journal = ifelse(journal == "INT ORG","INTERNATIONAL ORGANIZATION",journal),
                   journal = ifelse(journal == "J ECON GROWTH","JOURNAL OF ECONOMIC GROWTH",journal),
                   journal = ifelse(journal == "J EXP POLIT SCI","JOURNAL OF EXPERIMENTAL POLITICAL SCIENCE",journal),
                   journal = ifelse(journal == "J. ECON. HIST.","THE JOURNAL OF ECONOMIC HISTORY",journal),
                   journal = ifelse(journal == "JOURNAL OF BUSINESS AND ECONOMIC STATISTICS", "JOURNAL OF BUSINESS ECONOMIC STATISTICS",journal),
                   journal = ifelse(journal == "PROC NATL ACAD SCI USA", "PROCEEDINGS OF THE NATIONAL ACADEMY OF SCIENCES",journal),
                   journal = ifelse(journal == "AMERICAN J POLITICAL SCI", "AMERICAN JOURNAL OF POLITICAL SCIENCE",journal),
                   journal = gsub(".*CANADIAN J OF ECONOMICS.*","CANADIAN JOURNAL OF ECONOMICS",journal)
            ) %>%
            as.tibble()
        return(df)
    }
    else {
        return(df)
    }
}

## -------------------- Function 5: str_to_num --------------------

## Function for converting various columns from string to numeric
str_to_num <- function(df) {
    if ("reproducer_id" %in% colnames(df)) {
        df <- df %>% mutate_at("reproducer_id", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("reproduction_id" %in% colnames(df)) {
        df <- df %>% mutate_at("reproduction_id", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("crossref_cited_by" %in% colnames(df)) {
        df <- df %>% mutate_at("crossref_cited_by", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("crossref_n_authors" %in% colnames(df)) {
        df <- df %>% mutate_at("crossref_n_authors", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("crossref_year" %in% colnames(df)) {
        df <- df %>% mutate_at("crossref_year", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("Rank" %in% colnames(df)) {
        df <- df %>% mutate_at("Rank", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("Factor" %in% colnames(df)) {
        df <- df %>% mutate_at("Factor", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("pub_year" %in% colnames(df)) {
        df <- df %>% mutate_at("pub_year", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("familiarity_level" %in% colnames(df)) {
        df <- df %>% mutate_at("familiarity_level", as.numeric) %>% as_tibble()
        return(df)
    }
    if ("pub_year" %in% colnames(df)) {
        df <- df %>% mutate_at("pub_year", as.numeric) %>% as_tibble()
        return(df)
    }
    else {
        return(df)
    }
}

## -------------------- Function 6: descriptor --------------------

## Function for adding JEL descriptions
descriptor <- function(key){
    description = c(
        "A" = "A: Gen. Econ \\& Teaching",
        "B" = "B: History of Econ Thought, Methodol., Heterodox Approaches",
        "C" = "C: Mathematical and Quantitative Methods",
        "D" = "D: Microeconomics",
        "E" = "E: Macroeconomics and Monetary Economics",
        "F" = "F: International Economics",
        "G" = "G: Financial Economics",
        "H" = "H: Public Economics",
        "I" = "I: Health, Education, and Welfare",
        "J" = "J: Labor and Demographic Economics",
        "K" = "K: Law and Economics",
        "L" = "L: Industrial Organization",
        "M" = "M: Bus. Admin and Bus. Econ \\& Mktg \\& Accg \\& Personnel Econ",
        "N" = "N: Economic History",
        "O" = "O: Economic Dev., Innov., Tech. Change, and Growth",
        "P" = "P: Pol. Econ. and Comp. Economic Systems",
        "Q" = "Q: Ag. and NR Econ \\& Envr. and Ecological Econ",
        "R" = "R: Urban, Rural, Regional, Real Estate, and Trans. Economics",
        "Y" = "Y: Miscellaneous Categories",
        "Z" = "Z: Other Special Topics"
    )
    return(description[key])
}

## -------------------- Function 7: round_df --------------------

## Function for rounding all cells that tare numeric within a dataframe
round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    df[,nums] <- round(df[,nums], digits = digits)
    (df)
}

## -------------------- END --------------------






## -------------------- --------------------
##
## Script name: 02_Import_Data.R
##
##
## -------------------- --------------------



## -------------------- SSRP Original Data --------------------

## Call in Original Data (tidy-dfs) from SSRP
tidy_paper_df <- as_tibble(read_csv(paste0("processed/", "tidy_paper_df.csv"))) %>% filter(reproduction_type == "full")
tidy_abd_paper_df <- as_tibble(read_csv(paste0("processed/", "tidy_abandoned_df.csv")))
tidy_claim_df <- as_tibble(read_csv(paste0("processed/", "tidy_claim_df.csv")))
tidy_di_df <- as_tibble(read_csv(paste0("processed/", "tidy_di_df.csv")))
claim_di_xwalk <- as_tibble(read_csv(paste0("processed/", "claim_di_xwalk.csv")))
reproducer_char <- as_tibble(read_csv(paste0("processed/", "tidy_reproducer_df.csv")))



# (Moved to download stage) 
tidy_paper_df <- tidy_paper_df %>% filter(!(reproducer_id == 238 & reproduction_id == 299))
tidy_abd_paper_df <- tidy_abd_paper_df %>% filter(!(reproducer_id == 238 & reproduction_id == 299))
tidy_claim_df <- tidy_claim_df %>% filter(!(reproducer_id == 238 & reproduction_id == 299))
tidy_di_df <- tidy_di_df %>% filter(!(reproducer_id == 238 & reproduction_id == 299))
claim_di_xwalk <- claim_di_xwalk %>% filter(!(reproducer_id == 238 & reproduction_id == 299))
reproducer_char <- reproducer_char %>% filter(!(reproducer_id == 238 & reproduction_id == 299))

# (Moved to download stage) 
tidy_paper_df <- tidy_paper_df %>% filter(!(reproducer_id == 542 & reproduction_id == 1006))
tidy_abd_paper_df <- tidy_abd_paper_df %>% filter(!(reproducer_id == 542 & reproduction_id == 1006))
tidy_claim_df <- tidy_claim_df %>% filter(!(reproducer_id == 542 & reproduction_id == 1006))
tidy_di_df <- tidy_di_df %>% filter(!(reproducer_id == 542 & reproduction_id == 1006))
claim_di_xwalk <- claim_di_xwalk %>% filter(!(reproducer_id == 542 & reproduction_id == 1006))
reproducer_char <- reproducer_char %>% filter(!(reproducer_id == 542 & reproduction_id == 1006))

# (Moved to download stage) 
tidy_paper_df <- tidy_paper_df %>% filter(!(reproducer_id == 1190 & reproduction_id == 1445))
tidy_abd_paper_df <- tidy_abd_paper_df %>% filter(!(reproducer_id == 1190 & reproduction_id == 1445))
tidy_claim_df <- tidy_claim_df %>% filter(!(reproducer_id == 1190 & reproduction_id == 1445))
tidy_di_df <- tidy_di_df %>% filter(!(reproducer_id == 1190 & reproduction_id == 1445))
claim_di_xwalk <- claim_di_xwalk %>% filter(!(reproducer_id == 1190 & reproduction_id == 1445))
reproducer_char <- reproducer_char %>% filter(!(reproducer_id == 1190 & reproduction_id == 1445))

# (Moved to download stage) 
tidy_paper_df <- tidy_paper_df %>% filter(!(reproducer_id == 1207 & reproduction_id == 1460))
tidy_abd_paper_df <- tidy_abd_paper_df %>% filter(!(reproducer_id == 1207 & reproduction_id == 1460))
tidy_claim_df <- tidy_claim_df %>% filter(!(reproducer_id == 1207 & reproduction_id == 1460))
tidy_di_df <- tidy_di_df %>% filter(!(reproducer_id == 1207 & reproduction_id == 1460))
claim_di_xwalk <- claim_di_xwalk %>% filter(!(reproducer_id == 1207 & reproduction_id == 1460))
reproducer_char <- reproducer_char %>% filter(!(reproducer_id == 1207 & reproduction_id == 1460))

# (Moved to download stage) 
tidy_paper_df <- tidy_paper_df %>% filter(!(reproducer_id == 1234 & reproduction_id == 1752))
tidy_abd_paper_df <- tidy_abd_paper_df %>% filter(!(reproducer_id == 1234 & reproduction_id == 1752))
tidy_claim_df <- tidy_claim_df %>% filter(!(reproducer_id == 1234 & reproduction_id == 1752))
tidy_di_df <- tidy_di_df %>% filter(!(reproducer_id == 1234 & reproduction_id == 1752))
claim_di_xwalk <- claim_di_xwalk %>% filter(!(reproducer_id == 1234 & reproduction_id == 1752))
reproducer_char <- reproducer_char %>% filter(!(reproducer_id == 1234 & reproduction_id == 1752))

# (Moved to download stage) 
tidy_paper_df <- tidy_paper_df %>% filter(!(reproducer_id == 1197 & reproduction_id == 1758))
tidy_abd_paper_df <- tidy_abd_paper_df %>% filter(!(reproducer_id == 1197 & reproduction_id == 1758))
tidy_claim_df <- tidy_claim_df %>% filter(!(reproducer_id == 1197 & reproduction_id == 1758))
tidy_di_df <- tidy_di_df %>% filter(!(reproducer_id == 1197 & reproduction_id == 1758))
claim_di_xwalk <- claim_di_xwalk %>% filter(!(reproducer_id == 1197 & reproduction_id == 1758))
reproducer_char <- reproducer_char %>% filter(!(reproducer_id == 1197 & reproduction_id == 1758))


## -------------------- --------------------
##
## Script name: 03_Clean_Data.R
##
##
## -------------------- --------------------



## ---------------------------------------------------------------
## ----------- Basic Data Cleaning I - Data Extraction -----------
## ---------------------------------------------------------------

## Keep Only Relevant Variables
tidy_paper_df <- tidy_paper_df %>%
    select(reproducer_id, reproduction_id, country_code, doi, 
           journal, pub_year, authors, 
           familiarity_level, package_exists, 
           master_file_exists, master_file_one_click, 
           master_file_no_one_click_reasons, master_file_necessary_software,
           master_file_other_errors, master_file_run_after_corrections) %>%
    unique() %>% 
    as_tibble()

tidy_abd_paper_df <- tidy_abd_paper_df %>%
    select(reproducer_id, reproduction_id, country_code, doi,
           journal, pub_year, authors, package_exists, 
           authors_contacted, authors_response, authors_response_other) %>%
    unique() %>%
    as_tibble()

tidy_di_df <- tidy_di_df %>%
    select(reproducer_id, reproduction_id, doi,
           journal, pub_year, 
           executes, noexecute_reason, noexecute_software, noexecute_other,
           output_same, output_same_exp, output_different_exp, 
           DI_N, DI, repro_score, any_improvments, 
           needs_rscore_update, new_repro_score) %>%
    unique() %>% 
    as_tibble()

tidy_claim_df <- tidy_claim_df %>%
    select(reproducer_id, reproduction_id, doi,
           journal, pub_year,
           claim_N, method_confidence) %>% 
    unique() %>% 
    as_tibble()


## ----------------------------------------------------------------
## ------------- Basic Data Cleaning II - Formatting -------------
## ----------------------------------------------------------------

## Resolve Formatting Issues 
## Apply Identical Formatting Functions Defined in "01_Functions.R" to All dfs

## Set up the dataframe list
dfList <- list(tidy_paper_df = tidy_paper_df, 
               tidy_abd_paper_df = tidy_abd_paper_df,
               tidy_di_df = tidy_di_df,
               tidy_claim_df = tidy_claim_df,
               claim_di_xwalk = claim_di_xwalk,
               reproducer_char = reproducer_char)

## (1) Remove columns that are fully NA
dfList <- lapply(dfList, RemoveNAs)

## (2) Capitalize all strings (avoid issues with matching, duplicates)
dfList <- lapply(dfList, str_cleaning)

## (3) Clean journal names (see user-written function above)
dfList <- lapply(dfList, clean_journal_names)

## (4) Convert numerous variables to numeric (see "01_functions.R" for variable list)
dfList <- lapply(dfList, str_to_num)

## Overwrite original dataframe objects with updated dataframes within dfList
tidy_paper_df <- dfList[["tidy_paper_df"]]
tidy_abd_paper_df <- dfList[["tidy_abd_paper_df"]]
tidy_di_df <- dfList[["tidy_di_df"]]
tidy_claim_df <- dfList[["tidy_claim_df"]]
claim_di_xwalk <- dfList[["claim_di_xwalk"]]
reproducer_char <- dfList[["reproducer_char"]]

## Remove dataframe list
rm(dfList)  

## ----------------------------------------------------------------
## ------- Basic Data Cleaning III - Dropping Reproductions -------
## ----------------------------------------------------------------

## Drop reproduction_id == 210; reproduction_id = 210 is a reproduction exercise of a blog

tidy_paper_df <- tidy_paper_df %>% filter(!is.na(reproduction_id) & reproduction_id != 210)
tidy_di_df <- tidy_di_df %>% filter(!is.na(reproduction_id) & reproduction_id != 210)
tidy_claim_df <- tidy_claim_df %>% filter(!is.na(reproduction_id) & reproduction_id != 210)
claim_di_xwalk <- claim_di_xwalk %>% filter(!is.na(reproduction_id) & reproduction_id != 210)


## -----------------------------------------------------------------
## -------------- Basic Data Cleaning IV - Crosswalks --------------
## -----------------------------------------------------------------

## Create Abandoned Reproduction Crosswalk ()
abandoned_repro <- tidy_abd_paper_df %>%
    select(reproducer_id, reproduction_id, doi) %>%
    mutate(abandoned = 1) %>%
    unique() %>%
    as_tibble()

## ----------------------------------------------------------------
## ---- Basic Data Cleaning V - Combine Completed and Abandoned ----
## ----------------------------------------------------------------

## Create Paper Level Dataframe with both Completed and Abandoned Reproduction Exercises
## Merge should not match anything because a paper cannot be abandoned and be completed
tidy_all_paper_df <- tidy_paper_df %>%
    # Merge "tidy_paper_df" and "tidy_abd_paper_df"
    full_join(tidy_abd_paper_df,
              by = c(
                  "reproducer_id",
                  "reproduction_id",
                  "country_code",
                  "doi",
                  "journal",
                  "pub_year",
                  "authors",
                  "package_exists"
              )
    )

## Drop tidy_paper_df and tidy_abd_paper_df
rm(tidy_paper_df, tidy_abd_paper_df)

## ---------------------------------------------------------------
## -------------------- Basic Data Cleaning VI --------------------
## ---------------------------------------------------------------

## On "tidy_all_paper_df", mark abandoned reproduction exercises
tidy_all_paper_df <- tidy_all_paper_df %>%
    left_join(as.tibble(setDT(abandoned_repro)[,-"doi"]), 
              by = c("reproducer_id", "reproduction_id")) %>%
    relocate(abandoned, .after = reproduction_id) %>% # Relocate abandoned column after reproduction_id
    as_tibble()



## -----------------------------------------------------------------
## -------------------- Basic Data Cleaning IX -------------------
## -----------------------------------------------------------------

## Clean `tidy_all_paper_df` for whether master file runs in one click and reasons for failure

## Remove square brackets and split up reasons for master file failure into multiple columns
var_list <- c("master_file_no_one_click_reasons")

for (i in var_list){
    tidy_all_paper_df[[i]] <- gsub("\\[|\\]|\\'|\\,", '', tidy_all_paper_df[[i]]) # Delete [], comma, apostrophe
    tidy_all_paper_df[[i]] <- gsub('"', '', tidy_all_paper_df[[i]]) # Delete quotations 
    tidy_all_paper_df[[i]] <- gsub('OTHER ERRORS. EXPLAIN.', 'OTHER ERRORS.', tidy_all_paper_df[[i]])
    tidy_all_paper_df <- cSplit(tidy_all_paper_df, i, sep = ".")
}

## Reorder Variables
tidy_all_paper_df <- relocate(tidy_all_paper_df,
                              master_file_no_one_click_reasons_1:master_file_no_one_click_reasons_4,
                              .after = master_file_one_click) %>%
    as_tibble()


## Manual Cleaning of Reasons for Masterfile One-Click Run Failure (read reasons and manually classify into closest options) 
tidy_all_paper_df <- tidy_all_paper_df %>%
    mutate(
        master_file_no_one_click_reasons_2 =
            ifelse(
                reproduction_id == 118 &
                    master_file_no_one_click_reasons_2 == "OTHER ERRORS",
                "THERE ARE PATH/DIRECTORY ERRORS",
                master_file_no_one_click_reasons_2
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 316 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "INVALID SYNTAX",
                master_file_no_one_click_reasons_1
            ),
        master_file_no_one_click_reasons_2 =
            ifelse(
                reproduction_id == 360 &
                    master_file_no_one_click_reasons_2 == "OTHER ERRORS",
                NA,
                master_file_no_one_click_reasons_2
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 457 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "THERE ARE PATH/DIRECTORY ERRORS",
                master_file_no_one_click_reasons_1
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 493 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "THERE ARE PATH/DIRECTORY ERRORS",
                master_file_no_one_click_reasons_1
            ),
        master_file_no_one_click_reasons_2 =
            ifelse(
                reproduction_id == 634 &
                    master_file_no_one_click_reasons_2 == "OTHER ERRORS",
                NA,
                master_file_no_one_click_reasons_2
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 690 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "I NEED TO INSTALL ADDITIONAL PACKAGES LIBRARIES OR UPDATES",
                master_file_no_one_click_reasons_1
            ),
        master_file_no_one_click_reasons_2 =
            ifelse(
                reproduction_id == 691 &
                    master_file_no_one_click_reasons_2 == "OTHER ERRORS",
                "INVALID SYNTAX",
                master_file_no_one_click_reasons_2
            ),
        master_file_no_one_click_reasons_4 =
            ifelse(
                reproduction_id == 702 &
                    master_file_no_one_click_reasons_4 == "OTHER ERRORS",
                "MISSING/Undefined VARIABLE",
                master_file_no_one_click_reasons_4
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 706 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "MISSING/Undefined VARIABLE",
                master_file_no_one_click_reasons_1
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 735 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "I NEED TO INSTALL ADDITIONAL PACKAGES LIBRARIES OR UPDATES",
                master_file_no_one_click_reasons_1
            ),
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 909 &
                    is.na(master_file_no_one_click_reasons_1),
                "MISSING/Undefined VARIABLE",
                master_file_no_one_click_reasons_1
            ),    
        master_file_no_one_click_reasons_1 =
            ifelse(
                reproduction_id == 1070 &
                    master_file_no_one_click_reasons_1 == "OTHER ERRORS",
                "Software Availability / Discrepancy In Versions",
                master_file_no_one_click_reasons_1
            )    
    )


## Consistency Checks for Master File Exist & Master File One Click
## master_file_exists == FALSE & master_file_one_click == TRUE makes no sense
## Setting master_file_exists == TRUE for above cases
tidy_all_paper_df <- tidy_all_paper_df %>%
    mutate(
        master_file_exists =
            ifelse(
                master_file_exists == FALSE & master_file_one_click == TRUE,
                TRUE,
                master_file_exists
            )
    )











## ----------------------------------------------------------------
## -------------------- Basic Data Cleaning X - --------------------
## ----------------------------------------------------------------

## Mark Papers and Reproductions with No DI that has reproducibility scores

## Reproduction Level Crosswalk - Whether it has at least one DI with reproducibility scores
repro_di_score_avail <- tidy_di_df %>%
    select(reproducer_id, reproduction_id, repro_score) %>%
    group_by(reproduction_id) %>%
    mutate(repo_has_repro_score = ifelse(any(!is.na(repro_score)), 1, 0)) %>% # Create dummy var for whether the reproduction exercise has at least one DI reproducibility score
    ungroup() %>%
    select(reproducer_id, reproduction_id, repo_has_repro_score) %>%
    unique()

## Paper Level Crosswalk - Whether it has at least one DI with reproducibility scores
doi_di_score_avail <- tidy_di_df %>%
    select(reproducer_id, reproduction_id, doi, repro_score) %>%
    group_by(doi) %>%
    mutate(doi_has_repro_score = ifelse(any(!is.na(repro_score)), 1, 0)) %>%
    ungroup() %>%
    select(reproducer_id, reproduction_id, doi, doi_has_repro_score) %>%
    unique()


## ---------------------------------------------------------------
## ------------ Basic Data Cleaning XI - Cleaning DIs ------------
## ---------------------------------------------------------------

## Drop DI identified but not assessed in completed reproduction exercises
## (Dropping less than 3% of DIs - but one reproducer is a major culprit. 
## Seems to identified display items, but not assess them nor improve them)
tidy_di_df <- tidy_di_df %>% filter(is.na(repro_score) == 0)


## ----------------------------------------------------------------
## --------------- Basic Data Cleaning XII - Weights ---------------
## ----------------------------------------------------------------


## -------------------- END --------------------
