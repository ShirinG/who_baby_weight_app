library(shiny)
library(shinythemes)

library(readr)
library(tidyverse)
library(plotly)
library(modelr)
library(lubridate)

source("functions/combine_measures.R")
source("functions/elapsed_months.R")

# Define UI for data upload app ----
ui <- fluidPage(
    theme = shinytheme("paper"),
    
    # App title ----
    titlePanel("WHO Baby Weight Reference"),
    
    helpText("Compare your baby's weight with WHO references for boys and girls. Upload your weight measurements below:"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
        
            radioButtons("age_range", "Age range",
                         choices = c('Weeks 1 through 13' = "0_13",
                                     'Years 1 through 5' = "0_5"),
                         selected = "0_5"),
            
            radioButtons("weight_in", "Weight in",
                         choices = c(Gramm = "g",
                                     Kilogramm = "kg"),
                         selected = "g"),
            
            radioButtons("gender", "Gender",
                         choices = c(Boy = "boy",
                                     Girl = "girl"),
                         selected = "boy")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Data", 
                                 verbatimTextOutput("input_instructions"),
                                 DT::dataTableOutput("contents"),
                                 uiOutput("example_csv")),
                        tabPanel("Curve", 
                                 plotlyOutput("plot_curve"),
                                 htmlOutput("correlation")),
                        tabPanel("Barchart", 
                                 plotlyOutput("plot_bar"),
                                 htmlOutput("weight_diff"))
            )
        )
    ),
    
    hr(),
    div(class = "footer",
        includeHTML("footer.html")
    )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
    
    url <- a("example CSV file with dummy data.", href="https://github.com/ShirinG/who_baby_weight_app/blob/master/data/weight_measures.csv")
    
    output$example_csv <- renderUI({
        tagList("Follow this link to download an", url)
    })
    
    output$input_instructions <- renderText({ 
        "Your weight measurements need to have the following format:\n
        - CSV format with comma, semicolon or tab separator\n
        - two columns: column 1 titled date in format '%d.%m.%Y' (e.g. 28.02.2019)\n
        - column 2 titled weight with measurements in Gramm or Kilogramm\n
        - the first row has to contain birth date and birth weight"
    })
    
    output$contents <- DT::renderDataTable({
        
        req(input$file1)
        
        weight_measures <- read_delim(input$file1$datapath,
                         delim = input$sep
                       )
                       
        return(weight_measures)
        
    })
    
    weight_measures <- reactive({
        req(input$file1)
        
        weight_measures <- read_delim(input$file1$datapath,
                                      delim = input$sep,
                                      col_types = list(col_date(format = "%d.%m.%Y"), col_double())
        )
        
    })
    
    combine_measures_who_final <- reactive({

        p_0_5 <- read_csv("data/p_0_5.csv")
        p_0_13 <- read_csv("data/p_0_13.csv")
        
        combine_measures_who_final <- combine_measures_who(weight_measures(), 
                                                           p_0_13, p_0_5,
                                                           age_range = input$age_range,
                                                           weight_in = input$weight_in,
                                                           gender = input$gender)
    })
    
    weight_measures_all <- reactive({
        weight_measures <- weight_measures()
        
        # add missing dates for calculating weight change per week of life
        reference_date <- weight_measures$date[[1]]
        end_date <- weight_measures$date[[nrow(weight_measures)]]
        
        all_dates <- seq(from = reference_date, to = end_date, by = "day") %>%
            as_tibble()
        colnames(all_dates) = "date"
        
        weight_measures_all <- weight_measures %>%
            full_join(all_dates, by = "date")
        
        ## approximate missing values
        weight_measures_all <- weight_measures_all %>%
            mutate(weight_approx = approx(weight, n = nrow(.))[[2]])
        
        ## add running week number
        nos <- rep(1:ceiling(nrow(weight_measures_all)/7), each = 7)
        weight_measures_all$week <- nos[1:nrow(weight_measures_all)]
        
        ## calculate sum over week
        weight_measures_all <- weight_measures_all %>%
            mutate(diff_day = c(0, diff(weight_approx, lag = 1)),
                   diff_week = c(rep(0, 7), diff(weight_approx, lag = 7)))
        
        # calculate number of elapsed months btw dates
        weight_measures_all <- weight_measures_all %>%
            mutate(month = elapsed_months(date, reference_date))
        
        #may I suggest using case_when instead? to me it seems more readable than a bunch of nested ifelse statements!
        weight_measures_all <- weight_measures_all %>%
            mutate(color = case_when(
                month <= 2 & diff_week >= 170 ~ "ok",
                month == 3 & diff_week >= 110 ~ "ok",
                month == 4 & diff_week >= 110 ~ "ok",
                month == 5 & diff_week >= 70 ~ "ok",
                month == 6 & diff_week >= 70 ~ "ok",
                month >= 7 & diff_week >= 40 ~ "ok",
                TRUE ~ "low"))
    })
    
    starting_p <- reactive({
        ## find reference percentile
        reference_date <- weight_measures()$date[[1]]
        starting_p <- combine_measures_who_final() %>% 
            filter(date == reference_date) %>%
            select(starting_p) %>%
            .[[1]] %>%
            .[1]
    })
    
    test_curves <- reactive({
        test_curves <- combine_measures_who_final() %>%
            filter(percentile == !!paste(starting_p())) %>%
            left_join(select(weight_measures_all(), date, weight_approx), by = "date")
    })
    
    output$plot_curve <- renderPlotly({
        
        gender <- ifelse(input$gender == "boy", "boys", "girls")
        age <- ifelse(input$age_range == "0_13", "weeks 1 through 13", "years 1 through 5")
        
        plot_final_curve <- combine_measures_who_final() %>%
            ggplot(aes(date, weight,
                       linetype = ref,
                       color = percentile)) +
            geom_line() +
            geom_point() +
            labs(x = "Date",
                 y = "Weight in kg",
                 title = paste("WHO child growth standard percentiles shown for", gender, age)) +
            theme_bw() +
            theme(legend.position = "bottom")
        
        ggplotly(plot_final_curve)
    })
    
    output$correlation <- renderUI({ 
        paste("Your reference percentile is:", starting_p(), 
              "\n. Correlation between your measurements and your reference percentile is:",
              round(cor(test_curves()$weight, test_curves()$weight_approx, use = 'complete.obs'), digits = 5))
    })
    
    output$plot_bar <- renderPlotly({

        plot_final_bar <- weight_measures_all() %>%
            ggplot(aes(x = date, y = diff_week, fill = color)) +
                geom_bar(stat = "identity") +
                scale_fill_brewer(palette = "Set1") +
                labs(x = "Date",
                     y = "Weight difference in gramm",
                     fill = "Weight gain",
                     title = "Weekly weight differences: (Approximated) weight for each day compared to weight at 7 days prior") +
                theme_bw()
        
        ggplotly(plot_final_bar)
    })
    
    output$weight_diff <- renderUI({ 
        url <- a("this German site about breastfeeding", href="https://www.stillkinder.de/gewicht-und-wachstum-von-gestillten-kindern/")
        tagList("Bar colors show whether the weekly weight gain is above (blue) or below (red) the required minimum for BREASTFED babies given by",
                url,
                ". In months 1 and 2, the minimum weight gain should be: 170 g, in months 3 and 4: 110 g, in months 5 and 6: 70 g and from month 7 on: 40 g.")
   })
    
}
# Run the app ----
shinyApp(ui, server)

# deploy
# rsconnect::deployApp()
