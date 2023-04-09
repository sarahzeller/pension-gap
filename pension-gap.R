library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Pension Gap Calculator"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        inputId = "years",
        label = "Retirement years:",
        value = 30,
        min = 1,
        max = 50,
        ticks = FALSE
      ),
      numericInput(
        inputId = "monthly_pay",
        label = "Monthly Pay-out (today's value):",
        value = 2000
      ),
      numericInput(
        inputId = "inflation_rate",
        label = "Yearly Inflation Rate (%):",
        value = 2.63
      ),
      numericInput(
        inputId = "retirement_year",
        label = "Year of Retirement:",
        value = 2050
      )
    ),
    mainPanel(
      h4("Total Sum of Money:"),
      textOutput(outputId = "total_money"),
      plotOutput(outputId = "pay_plot")
    )
  )
)




library(ggplot2)

server <- function(input, output) {
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
  output$total_money <- renderText({
    retirement_year <- input$retirement_year
    years_to_retirement <- retirement_year - current_year
    
    yearly_pay_today <- input$monthly_pay * 12
    yearly_pay_at_retirement <- yearly_pay_today * (1 + input$inflation_rate / 100) ^ years_to_retirement
    yearly_pay_in_retirement <- yearly_pay_at_retirement * (1 + input$inflation_rate / 100) ^ (1:(input$years) - 1)
    
    total <- sum(yearly_pay_in_retirement)
    total %>% 
      round(2) %>% 
      format(big.mark = " ", decimal.mark = ",", nsmall = 2, trim = TRUE, scientific = FALSE) %>% 
      paste("€", ., sep = " ")
  })
  
  output$pay_plot <- renderPlot({
    retirement_year <- input$retirement_year
    years <- seq(retirement_year, retirement_year + input$years - 1)
    monthly_pay <- input$monthly_pay
    inflation_rate <- input$inflation_rate
    current_year <- as.numeric(format(Sys.time(), "%Y"))
    
    monthly_pay_adj <- monthly_pay * (1 + inflation_rate / 100)^((years - retirement_year) + (retirement_year - current_year)) 
    
    df <- data.frame(year = years, monthly_pay = monthly_pay_adj)
    
    ggplot(df, aes(x = year, y = monthly_pay)) +
      geom_bar(stat = "identity", fill = "darkblue") +
      geom_hline(yintercept = monthly_pay, linetype = "dashed", color = "red") +
      labs(x = "", y = "Monthly Pay-out (Euros)", title = "Monthly Pay-out Adjusted for Inflation") +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)
