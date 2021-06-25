#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Barebones App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("method", "Method:",
                        c("Mean" = "mean_rl",
                          "Max" = "max_rl",
                          "Min" = "min_rl")),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)


df <- read_csv("all_submitted_may_25_2021.csv") %>% 
    mutate(clean_doi = str_extract(
        S0_1Q1P1,
        regex("10\\.\\d{4,9}/[-._;()/:a-z0-9]+$",   #Explain regex
              ignore_case = T)
    )) %>%
    mutate(
        S0_1Q1P2 = ifelse(
            test = (
                S0_1Q1P2 == "[Fernando] Railroads of the Raj: Estimating the Impact of Transportation Infrastructure"
            ),
            yes = "Railroads of the Raj: Estimating the Impact of Transportation Infrastructure",
            no = S0_1Q1P2
        )
    ) %>%  
    mutate(similar_title = phonetic(S0_1Q1P2)) %>%       #Assign common identifier to titles that sound similar to each other
    group_by(email, clean_doi) %>%
    mutate(repro_id = cur_group_id()) %>%          #unique identifier for each repro (temp, as it assumes on repro paper paper and reproducer)
    ungroup() 

claims_max <- df %>% select(contains("S2_4Q4_2P")) %>% names() %>% length()
di_max <- df %>% select(contains("S2_4iP")) %>% names() %>% length()

#loading the data
df_spec <- df %>% 
    select(
        repro_id,
        clean_doi,
        similar_title,
        S0_1Q1P2,                       #title
        contains("S1_3Q1P"),            #claims summary
        contains("S1_3Q5P1_r1c"),       #Name of display items for claim 1
        contains("S1_3Q5P2_r1c"),       #...
        contains("S1_3Q5P3_r1c"),
        contains("S1_3Q5P4_r1c"),
        contains("S1_3Q5P5_r1c"),
        contains("S1_3Q5P6_r1c"),
        contains("S1_3Q5P7_r1c"),       #Name of display items for last claim 
    ) %>% 
    rename_at(.vars = paste0("S1_3Q1P", 1:claims_max),
              .funs = ~ paste0("desc_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c1"),
              .funs = ~ paste0("diname.main_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c2"),
              .funs = ~ paste0("diname.altspec1_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c3"),
              .funs = ~ paste0("diname.altspec2_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c4"),
              .funs = ~ paste0("diname.altspec3_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c5"),
              .funs = ~ paste0("diname.altspec4_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c6"),
              .funs = ~ paste0("diname.altspec5_", 1:claims_max)) %>% 
    pivot_longer(#reshape to long
        cols = !c(repro_id, clean_doi, similar_title, S0_1Q1P2),   # do not modify: repro id, title id, title, claim summ
        names_to = c(".value", "claims"), 
        names_sep = "_" ) %>% 
    filter(!is.na(desc)) %>% 
    pivot_longer(
        cols = !c(repro_id, clean_doi, similar_title, S0_1Q1P2, claims, desc),
        names_to = "spec", 
        values_to = "diname"    
    ) %>% 
    mutate(spec = substring(spec, 8)) %>% 
    filter(!is.na(diname)) 
    

df_di  <- df %>%
    rename_at(.vars = paste0("S2_4Q4_2P", 1:di_max),
              .funs = ~ paste0("score_", 1:di_max)) %>% 
    rename_at(.vars = paste0("S2_4iP", 1:di_max),
              .funs = ~ paste0("diname_", 1:di_max)) %>% 
    select(repro_id, contains("diname_"), contains("score_") ) %>% 
    pivot_longer(#reshape to long
        cols = !repro_id,   # do not modify: repro id
        names_to = c(".value", "id"),
        names_sep = "_" ) %>%  
    select(-id) %>% 
    filter(!is.na(diname)) 


df_app <- left_join(df_spec, df_di, 
                    by = c("repro_id", "diname")) %>% 
    filter(!is.na(score)) %>% 
    arrange(similar_title, repro_id, claims)


stat1 <- df_app %>% 
    group_by(similar_title, repro_id, claims, diname) %>%      #repro-claim-di level (min? max? median? it should not matter)
    summarize(claim_di_level = mean(score)) %>% 
    ungroup(diname) %>% 
    summarise(claim_level = mean(claim_di_level)) %>%  #repro-claim level (min? max? median?)
    ungroup(claims) %>% 
    summarize(repro_level = mean(claim_level)) %>% 
    ungroup(repro_id) %>% 
    summarise(paper_level = mean(repro_level)) #repro level 

# distribution of scores of all display items
df_app %>% 
    group_by(similar_title, repro_id, claims, diname) %>%      #repro-claim-di level (min? max? median? it should not matter)
    summarize(claim_di_level = mean(score)) %>% 
    ggplot(aes(x = claim_di_level)) + 
    geom_histogram(binwidth = 1) +
    labs(title = "Distribution of Reproduction Scores: Display Items", 
         x = "Reproducibility Score", 
         y = "Count") 

# distribution of scores of all claims
df_app %>% 
    group_by(similar_title, repro_id, claims, diname) %>%      #repro-claim-di level (min? max? median? it should not matter)
    summarize(claim_di_level = mean(score)) %>% 
    ungroup(diname) %>% 
    summarise(claim_level = mean(claim_di_level)) %>% 
    ggplot(aes(x = claim_level)) + 
    geom_histogram(binwidth = 1) +
    labs(title = "Distribution of Reproduction Scores: All Claims", 
         x = "Reproducibility Score", 
         y = "Count")


df_titles <- df %>% select(similar_title, S0_1Q1P2) %>% group_by(similar_title) %>% 
    slice(1)
# Distribution of paper-level scores
df_app %>% 
    group_by(similar_title, repro_id, claims, diname) %>%      #repro-claim-di level (min? max? median? it should not matter)
    summarize(claim_di_level = mean(score)) %>% 
    ungroup(diname) %>% 
    summarise(claim_level = mean(claim_di_level)) %>%  #repro-claim level (min? max? median?)
    ungroup(claims) %>% 
    summarize(repro_level = mean(claim_level)) %>% 
    ungroup(repro_id) %>% 
    summarise(paper_level = mean(repro_level), 
              paper_level_sd = sd(repro_level), 
              n_repros = n()) %>% 
    left_join(df_titles, by = "similar_title") %>% group_by(similar_title) %>%
    arrange(paper_level) %>%     #repro level 
    ggplot(aes(x = paper_level)) + 
    geom_histogram(binwidth = 1) +
    labs(title = "Distribution of Reproduction Scores: Paper Level", 
         x = "Reproducibility Score", 
         y = "Count")


df_app %>% group_by(similar_title) %>% summarise(num_di = length(unique(repro_id))) %>% View()


if (FALSE) {
    group_by(repro_id) %>%
        mutate(
            mean_rl = mean(rep_level),
            max_rl = max(rep_level),
            min_rl = min(rep_level)
        ) %>% 
        arrange(similar_title, repro_id) %>% 
        select(similar_title, S1_3Q1P1) 
}



# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
            ggplot(df1, aes(x = get(input$method)
                            )) + 
            geom_histogram(binwidth = 1) +
            labs(title = "Distribution of All Reproduction Scores", 
                 x = "Reproducibility Score", 
                 y = "Count") 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


