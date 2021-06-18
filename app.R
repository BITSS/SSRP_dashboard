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


claims_max <- df1 %>% select(contains("S2_4Q4_2P")) %>% names() %>% length()

#loading the data
df1 <- read_csv("all_submitted_may_25_2021.csv") %>% 
    mutate(clean_doi = str_extract(
        S0_1Q1P1,
        regex("10\\.\\d{4,9}/[-._;()/:a-z0-9]+$",   #Explain regex
              ignore_case = T)
    )) %>%
    mutate(similar_title = phonetic(S0_1Q1P2)) %>% 
    group_by(email, clean_doi) %>%
    mutate(repro_id = cur_group_id()) %>%          #unique identifier for each repro
    ungroup() %>%
    select(
        repro_id,
        clean_doi,
        similar_title,
        S0_1Q1P2,
        S1_3Q1P1,
        S2_4iP2,
        contains("S2_4Q4_2P"),
        contains("S1_3Q5P")
    ) %>% 
    select(contains("r1c")) %>%      #keep levels only
    pivot_longer(#reshape to long
        cols = !c(repro_id,clean_doi, similar_title, S0_1Q1P2, S1_3Q1P1),   # do not modify: repro id, title id, title, claim summ
        names_to = c("claims"),
        values_to = "rep_level" ) %>%
    filter(!is.na(rep_level)) %>%
    group_by(repro_id) %>%
    mutate(
        mean_rl = mean(rep_level),
        max_rl = max(rep_level),
        min_rl = min(rep_level)
    ) %>% 
    arrange(similar_title, repro_id) %>% 
    select(similar_title, S1_3Q1P1) 


# Renaming display item names and s
temp1 <- df1 %>% 
    rename_at(.vars = paste0("S2_4Q4_2P", 1:claims_max),
              .funs = ~ paste0("claim_score_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c1"),
              .funs = ~ paste0("claim_di_main_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c2"),
              .funs = ~ paste0("claim_di_alt_spec1_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c3"),
              .funs = ~ paste0("claim_di_alt_spec2_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c4"),
              .funs = ~ paste0("claim_di_alt_spec3_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c5"),
              .funs = ~ paste0("claim_di_alt_spec4_", 1:claims_max)) %>% 
    rename_at(.vars = paste0("S1_3Q5P", 1:claims_max, "_r1c6"),
              .funs = ~ paste0("claim_di_alt_spec5_", 1:claims_max)) %>% 
    select(contains("claim"), contains("S2_4iP1")) %>% select(contains("_1"), contains("S2_4iP1")) %>% View()
    
temp1 <- df1 


df1 <- read_csv("all_submitted_may_25_2021.csv") %>% 
    rename_at(.vars = paste0("S2_4Q4_2P", 1:claims_max),
              .funs = ~ paste0("claim_score_", 1:claims_max)) %>% 
    mutate(clean_doi = str_extract(
        S0_1Q1P1,
        regex("10\\.\\d{4,9}/[-._;()/:a-z0-9]+$",   #Explain regex
              ignore_case = T)
    )) %>%
    mutate(similar_title = phonetic(S0_1Q1P2)) %>% 
    group_by(email, clean_doi) %>%
    mutate(repro_id = cur_group_id()) %>%
    ungroup() %>% 
    rename_at(.vars = paste0("S2_4iP", 1:claims_max, "_r1c6"),
              .funs = ~ paste0("claim_di_alt_spec5_", 1:claims_max)) %>% 
    select(repro_id, contains("claim_score"), contains("S2_4iP") ) %>% View()




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


