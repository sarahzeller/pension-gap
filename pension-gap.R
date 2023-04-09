library(shiny)
library(tidyverse)

library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Retirement Planner"),
  tabsetPanel(
    tabPanel("Pension Gap Calculator",
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
                 h4("Total needed:"),
                 textOutput(outputId = "total_money"),
                 h5("This is the estimated amount of money you will need for retirement."),
                 h4("Monthly pay-outs over time"),
                 h5("This graph shows the adjusted pay-outs over your retirement."),
                 plotOutput(outputId = "pay_plot")
               )
             )
    ),
    tabPanel("ETF Calculator",
             sidebarLayout(
               sidebarPanel(
                 numericInput(
                   inputId = "monthly_investment",
                   label = "Monthly Investment Amount:",
                   value = 100
                 ),
                 numericInput(
                   inputId = "annual_growth_rate",
                   label = "Expected Annual Growth Rate (%):",
                   value = 5
                 )
               ),
               mainPanel(
                 h4("Total ETF Value at Retirement:"),
                 textOutput(outputId = "future_etf"),
                 h5("This is the total value of your ETF investments at the year of retirement."),
                 h4("Net Worth After Taxes:"),
                 textOutput(outputId = "net_worth"),
                 h5("This is the amount of money you would have after selling your ETF investments and paying capital gains taxes."),
                 h4("Amount of ETFs to sell in retirement year 1:"),
                 textOutput(outputId = "sell_etf_year1")
               )
             )
    )
  )
)



server <- function(input, output) {
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  
  # First panel calculations
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
    
    monthly_pay_adj <- monthly_pay * (1 + inflation_rate / 100)^((years - retirement_year) + (retirement_year - current_year)) 
    
    df <- data.frame(year = years, monthly_pay = monthly_pay_adj)
    
    ggplot(df, aes(x = year, y = monthly_pay)) +
      geom_bar(stat = "identity", fill = "cornflowerblue") +
      geom_hline(yintercept = monthly_pay, linetype = "dashed", color = "red") +
      labs(x = "", y = "Monthly Pay-out (Euros)", title = "Monthly Pay-out Adjusted for Inflation") +
      theme_minimal()
  })
  
  # Second panel calculations
  etf_value <- reactive({
    annual_investment <- input$monthly_investment * 12
    years_to_invest <- input$retirement_year - current_year
    growth_rate <- input$annual_growth_rate / 100
    future_value <- annual_investment * ((1 + growth_rate)^years_to_invest - 1) / growth_rate * (1 + growth_rate)
  })
  
  output$future_etf <- renderText({
    future_value <- etf_value()
    future_value %>% 
      round(2) %>% 
      format(big.mark = " ", decimal.mark = ",", nsmall = 2, trim = TRUE, scientific = FALSE) %>% 
      paste("€", ., sep = " ")
  })
    
  output$net_worth <- renderText({
      future_value <- etf_value()
     
      net_worth <- future_value * (1 - 0.25)
      net_worth %>% 
        round(2) %>% 
        format(big.mark = " ", decimal.mark = ",", nsmall = 2, trim = TRUE, scientific = FALSE) %>% 
        paste("€", ., sep = " ")
    })
  
  output$sell_etf_year1 <- renderText({
    future_value <- etf_value()
    sell_amount <- future_value / input$years
    
    # Format and return the result
    sell_amount %>% 
      round(2) %>% 
      format(big.mark = " ", decimal.mark = ",", nsmall = 2, trim = TRUE, scientific = FALSE) %>% 
      paste("€", ., sep = " ")
  })
  
}
  

shinyApp(ui = ui, server = server)
