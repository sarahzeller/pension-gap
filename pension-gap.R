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
                   value = 1500
                 ),
                 numericInput(
                   inputId = "inflation_rate",
                   label = "Yearly Inflation Rate (%):",
                   value = 2.63
                 ),
                 numericInput(
                   inputId = "retirement_year",
                   label = "Year of Retirement:",
                   value = 2060
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
                 ),
                 numericInput(
                   inputId = "dividends",
                   label = "Expected Annual Dividends (%):",
                   value = 1.3
                 ),
                 numericInput(
                   inputId = "etf_payout",
                   label = "Monthly ETF Pay-out (%):",
                   value = 3
                 )
               ),
               mainPanel(
                 h4("Total ETF Value at Retirement:"),
                 textOutput(outputId = "future_etf"),
                 h5("This is the total value of your ETF investments at the year of retirement."),
                 h4("Net Worth After Taxes:"),
                 textOutput(outputId = "net_worth"),
                 h5("This is the amount of money you would have after selling your ETF investments and paying capital gains taxes."),
                 h4("Total dividends in retirement year 1:"),
                 textOutput(outputId = "dividends_year_1"),
                 h4("Payouts during retirement"),
                 plotOutput(outputId = "payout_retirement"),
                 h4("ETF value in retirement"),
                 plotOutput(outputId = "etf_value_retirement")
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
  
  output$dividends_year_1 <- renderText({
    future_value <- etf_value()
    dividends <- future_value * input$dividends/100
    
    # Format and return the result
    dividends %>% 
      round(2) %>% 
      format(big.mark = " ", decimal.mark = ",", nsmall = 2, trim = TRUE, scientific = FALSE) %>% 
      paste("€", ., sep = " ")
  })
  
  output$payout_retirement <- renderPlot({
    etf_retirement_y1 <- etf_value()
    payout <- input$etf_payout / 100
    dividends <- input$dividends / 100
    growth_rate <- (input$annual_growth_rate - dividends) / 100
    years <- input$years
    retirement_year <- input$retirement_year
    inflation_rate <- input$inflation_rate / 100
    monthly_pay <- input$monthly_pay
    
    retirement_needs <- data.frame(
      year = retirement_year:(retirement_year + years -1)) |> 
      mutate(need = monthly_pay * 12* (1 + inflation_rate)^((year - retirement_year) + (retirement_year - current_year))
    )
    
    payout_in_retirement <- data.frame(
      year = input$retirement_year:(input$retirement_year + years -1),
      etf_value = etf_retirement_y1 * (1 - payout + growth_rate)^(0:(years - 1))
    ) |> 
      mutate(dividend = etf_value * dividends,
             payout = etf_value * payout) |> 
      pivot_longer(cols = c("dividend", "payout"),
                   names_to = "type")
    
    ggplot() +
      geom_bar(data = payout_in_retirement,
               aes(x = year, y = value, fill = type),
               position = "stack", stat = "identity") +
      geom_line(data = retirement_needs,
                aes(x = year, y = need)) +
      theme_classic()
  })
  
  output$etf_value_retirement <- renderPlot({
    etf_retirement_y1 <- etf_value()
    payout <- input$etf_payout / 100
    growth_rate <- (input$annual_growth_rate - input$dividends) / 100
    retirement_years <- input$years
    
    etf_in_retirement <- data.frame(
      year = input$retirement_year:(input$retirement_year + retirement_years - 1),
      value_after_payout = etf_retirement_y1 * (1 - payout + growth_rate)^(0:(retirement_years - 1))
    )
    
    ggplot(etf_in_retirement, aes(x = year, y = value_after_payout)) +
      geom_col() +
      theme_classic() +
      labs(y = "ETF portfolio value")
    
  })
  
}
  

shinyApp(ui = ui, server = server)
