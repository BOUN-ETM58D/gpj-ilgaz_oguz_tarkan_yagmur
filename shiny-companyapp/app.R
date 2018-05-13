library(shiny)
library(ggplot2)
library(tidyverse)
library(DT)
library(readxl)
library(rsconnect)

insurance <- read_excel("shiny_assignment_data.xlsx")

insurance <- insurance %>% mutate(date=lubridate::parse_date_time(date, orders = "dmY"))
insurance <- insurance %>% arrange(pension_fund_company, date)
insurance <- insurance %>% mutate(unit_fund = fund_size_participants/n_of_participants)
insurance <- insurance %>% mutate(expense = contribution-size_total) 
insurance <- insurance %>% mutate(expense_ratio = expense/contribution)

all_companies <- sort(unique(insurance$pension_fund_company))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Pension Funds Company Suggestor"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId="pension_fund_company",
                        label= "Select Company:",
                        choices= all_companies ,
                        selected = "Aegon Emeklilik ve Hayat",
                        multiple = TRUE,
                        selectize = TRUE),
            sliderInput("date",
                        "Date",
                        min = as.Date("2017-01-01", "%Y-%m-%d"),
                        max = as.Date("2018-03-31", "%Y-%m-%d"),
                        value = as.Date(c("2017-02-01","2018-01-31")),timeFormat="%Y-%m-%d")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput(outputId = "unitfund"),
            plotOutput(outputId = "expenseratio")
        )
    )
)

# Server
server <- function(input, output) {
     
    # Create unit fund plot
    output$unitfund <- renderPlot({
        insurance <- insurance %>% 
            filter(pension_fund_company %in% input$pension_fund_company) %>%
            filter(date >= input$date[1] & date <= input$date[2])
        ggplot(data=insurance, aes(date,unit_fund, color=pension_fund_company)) + geom_line() +
            labs(title="Unit Fund Comparision", x="Date", y="Unit Fund", color="Company") + 
            theme_bw()
    })

    # Create expense ratio plot
    output$expenseratio <- renderPlot({
        insurance <- insurance %>% 
            filter(pension_fund_company %in% input$pension_fund_company) %>%
            filter(date >= input$date[1] & date <= input$date[2])
        ggplot(data=insurance, aes(date,expense_ratio, color=pension_fund_company)) + geom_line() +
            labs(title="Expense Ratio Comparision", x="Date", y="Expense Ratio", color="Company") + 
            theme_bw()
    })
    
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
runApp(".")
