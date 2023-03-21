library(shiny)
library(shinythemes)

ui <- fluidPage(title = 'Insurance Premium Calculator', theme = shinytheme('cerulean'),
                tags$head(
                  tags$style(HTML(".my-verbatext-output {
                                        padding-top: 2%;
                                        
                                    }
                                    .my-sub-header{
                                        font-family: Arial, Helvetica, sans-serif;
                                        font-size: 130%;
                                    }
                                    .centered-btn {
                                        display: flex;
                                        justify-content: center;
                                        padding-top: 4%;
                                    }
                                    .mainPanelDiv{
                                        background-color: #e6faff;
                                    }
                                    body{
                                        padding: 10px;
                                        padding-top: 3px;
                                        padding-bottom: 20px;
                                    }
                                  ")
                             )
                ),
                div(style = "text-align: center;", titlePanel('Insurance Premium Calculator')),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    width = 14, class = "mainPanelDiv",
                    fluidRow(
                      column(width = 4,
                             numericInput('asset_value', 'Asset Size (in millions):', 
                                          value = 2, min = 0, step = 0.01)),
                      column(width = 4,
                             numericInput("limit", "Limit (in millions)", value = 0.05,
                                          step = 0.01, min = 0))
                    ),
                    
                    wellPanel(style = "background-color: cornsilk;",
                    fluidRow(
                      column(width = 4,
                             strong('NATURE OF OPERATIONS'), class = "my-sub-header")
                    ),
                    br(),
                    fluidRow(
                      column(width = 4,
                             selectInput("nature", "Risk Level",
                                         choices = c("Low Risk", "Average Risk", "Moderate Risk", "High Risk"))),
                      column(width = 4,
                             verbatimTextOutput("suggested_nature_range"), class = "my-verbatext-output"),
                      column(width = 4,
                             numericInput("nature_of_operations_modifier", "Nature of Operations Modifier", value = 0.75, step = 0.01, 
                                          min = 0.75, max = 0.99))
                    )
                    ),
                    wellPanel(style = "background-color: cornsilk;",
                    fluidRow(
                      column(width = 4,
                             strong('YEARS IN BUSINESS'), class = "my-sub-header")
                    ),
                    br(),
                    fluidRow(
                      column(width = 4,
                             selectInput("years", "No. of Years",
                                         choices = c("Greater than or equal to 3", "Less than 3"))),
                      column(width = 4,
                             verbatimTextOutput("suggested_no_of_years"), class = "my-verbatext-output"),
                      column(width = 4,
                             numericInput("years_modifier", "Years in Business Modifier", value = 0.80, step = 0.01, min = 0.80, 
                                          max = 1.00))
                    )
                    ),
                    #------------------- BALANCE SHEET ---------------------------------------------
                    wellPanel(style = "background-color: cornsilk;",
                    fluidRow(
                      column(width = 4,
                             strong('QUALITY OF BALANCE SHEET'), class = "my-sub-header")
                    ),
                    br(),
                    fluidRow(
                      column(width = 4,
                             selectInput("balance_sheet", "Quality Level",
                                         choices = c("Strong", "Above Average", "Average", "Below Average"))),
                      column(width = 4,
                             verbatimTextOutput("suggested_quality_range"), class = "my-verbatext-output"),
                      column(width = 4,
                             numericInput("balance_sheet_modifier", "Quality of Balance Sheet Modifier", value = 0.60, step = 0.01, min = 0.60, 
                                          max = 0.88))
                    )
                    ),
                    # Checkbox for additional coverage
                    strong(style = "text-align: center;", checkboxInput("add_coverage", "Purchase Additional Coverage", value = FALSE)),
                    
                    div(class = "centered-btn",
                      actionButton("button", "Calculate Premium", class = "btn btn-primary")
                    )
                  ),
                  
                  mainPanel(width = 12, style="text-align: center;",
                        fluidRow(
                          h3('The Premium Amount for the above policy details is:')
                        ),
                        fluidRow(style = "text-align: center;",
                                 column(width = 4),
                                 column(width = 4, style = "text-align: center;",
                                        wellPanel(
                                          verbatimTextOutput('premium_amount')
                                        )
                                 )
                        )
                  )
                )
  
)

server <- function(input, output, session) {
  
  calculated_premium_amount <- reactiveValues(amount = 0)
  
  asset_data_table <- data.frame(
                                  asset <- c(2.00, 7.50, 15.00, 20.00, 25.00, 50.00, 100.00),
                                  base_premium <- c(2800, 2900, 3000, 3144, 3260, 3654, 3906)
                                )
  
  limit_data_table <- data.frame(
                                  limit <- c(0.00, 0.05, 0.25, 0.50, 1.00, 5.00, 10.00, 25.00),
                                  premium <- c(20, 50, 125, 225, 400, 1000, 1500, 2500)
                                )
  
  linear_interpolation <- function(value, table){
    
    if (table == "asset"){
      column1 <- asset_data_table$asset
      column2 <- asset_data_table$base_premium
    }
    else{
      column1 <- limit_data_table$limit
      column2 <- limit_data_table$premium
    }
    
    x1 <- max(column1[column1 <= value])
    x2 <- min(column1[column1 >= value])
    
    y1 <- column2[column1 == x1]
    y2 <- column2[column1 == x2]
    
    slope <- (y2 - y1) / (x2 - x1)
    intercept <- y1 - slope * x1
    
    calculated_value <- slope * value + intercept
    
    return(calculated_value)
  }
  
  calculate_premium <- function(asset, limit, nop, years, balance_sheet, coverage){
    
    if (asset %in% asset_data_table$asset){
      base_premium <- asset_data_table$base_premium[asset_data_table$asset == asset]
    }
    else{
      base_premium <- linear_interpolation(asset, "asset")
    }
    
    limit_adjustment_factor <- (limit*10^4/10^6)^0.15
    
    rating_modification_factors <- nop*years*balance_sheet
    
    if (coverage){
      if (limit %in% limit_data_table$limit){
        additional_premium <- limit_data_table$premium[limit_data_table$limit == limit]
      }
      else{
        additional_premium <- linear_interpolation(limit, "limit")
      }
    }
    else{
      additional_premium <- 0
    }
    
    return (paste('£',round((base_premium*limit_adjustment_factor*rating_modification_factors)+additional_premium, 2)))
  }
  
  # Update display range section and respective modifier input element for nature of operations
  observeEvent(input$nature, {
    selected_range <- input$nature
    
    if (selected_range == "Low Risk") {
      min <- 0.75
      value_to_be_displayed <- 0.75
      max <- 0.99
    } else if (selected_range == "Average Risk") {
      min <- 1.00
      value_to_be_displayed <- 1.00
      max <- 1.25
    } else if (selected_range == "Moderate Risk") {
      min <- 1.26
      value_to_be_displayed <- 1.26
      max <- 1.50
    } else{
      min <- 1.51
      value_to_be_displayed <- 1.51
      max <- 1.75
    }
    
    output$suggested_nature_range <- renderText({
      sprintf(paste("Suggested Range is %g to %g", collapse = ""), min, max)
    })
    updateNumericInput(session, "nature_of_operations_modifier", min = min, max = max, value = value_to_be_displayed, step = 0.01)
  })
  
  # Update display range section and respective modifier input element for number of years in business
  observeEvent(input$years, {
    selected_year_range <- input$years
    
    if (selected_year_range == "Greater than or equal to 3") {
      min <- 0.80
      value_to_be_displayed <- 0.80
      max <- 1.00
    } else{
      min <- 1.01
      value_to_be_displayed <- 1.01
      max <- 1.30
    }
    
    output$suggested_no_of_years <- renderText({
      sprintf(paste("Suggested Range is %g to %g", collapse = ""), min, max)
    })
    updateNumericInput(session, "years_modifier", min = min, max = max, value = value_to_be_displayed, step = 0.01)
  })
  
  # Update display range section and respective modifier input element for nature of operations
  observeEvent(input$balance_sheet, {
    selected_quality_range <- input$balance_sheet
    
    if (selected_quality_range == "Strong") {
      min <- 0.60
      value_to_be_displayed <- 0.60
      max <- 0.88
    } else if (selected_quality_range == "Above Average") {
      min <- 0.89
      value_to_be_displayed <- 0.89
      max <- 1.16
    } else if (selected_quality_range == "Average") {
      min <- 1.27
      value_to_be_displayed <- 1.27
      max <- 1.44
    } else{
      min <- 1.45
      value_to_be_displayed <- 1.45
      max <- 1.72
    }
    
    output$suggested_quality_range <- renderText({
      sprintf(paste("Suggested Range is %g to %g", collapse = ""), min, max)
    })
    updateNumericInput(session, "balance_sheet_modifier", min = min, max = max, value = value_to_be_displayed, step = 0.01)
  })
  
  observeEvent(input$button, {
    # code to execute when the button is clicked
    output$premium_amount <- renderText({
      calculate_premium(input$asset_value, input$limit, input$nature_of_operations_modifier, input$years_modifier, 
                        input$balance_sheet_modifier, input$add_coverage)
    })
  })
}



shinyApp(ui, server)
