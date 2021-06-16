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


#loading the data
df1 <- read_csv("all_submitted_may_25_2021.csv") %>% 
    mutate(clean_doi = str_extract(
        S0_1Q1P1,
        regex("10\\.\\d{4,9}/[-._;()/:a-z0-9]+$",   #Explain regex
              ignore_case = T)
    )) %>%
    group_by(email, clean_doi) %>%
    mutate(repro_id = cur_group_id()) %>%          #unique identifier for each repro
    ungroup() %>%
    select(repro_id, contains("S2_4Q4_2P")) %>%     #keep levels only
    pivot_longer(#reshape to long
        cols = !repro_id,
        names_to = "claims",
        values_to = "rep_level") %>%
    filter(!is.na(rep_level)) %>%
    group_by(repro_id) %>%
    mutate(
        mean_rl = mean(rep_level),
        max_rl = max(rep_level),
        min_rl = min(rep_level)
    )

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


