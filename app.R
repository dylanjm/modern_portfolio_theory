#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyquant)
library(DT)
library(corrplot)
library(rportfolios)
library(plotly)


col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))

stock_options <- read.csv("tickers.csv", header = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = "slate.css",
   
   # Application title
   titlePanel("Modern Portfolio Theory"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        tags$head(tags$style(HTML("
    .progress-striped .bar {
                                  background-color: #149bdf;
                                  background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
                                  background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                  background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                  background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                  background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
                                  -webkit-background-size: 40px 40px;
                                  -moz-background-size: 40px 40px;
                                  -o-background-size: 40px 40px;
                                  background-size: 40px 40px;
    }
.shiny-notification {top: 50% !important;
left: 50% !important;
margin-top: -100px !important;
margin-left: -250px !important; 
color: #00334d;
font-size: 20px;
font-style: italic;} "))),
         selectizeInput('tickers',
                        "Create Your Portfolio",
                        choices = stock_options$Symbol,
                        options = list(maxItems = 10, 
                                       placeholder = 'tickers',
                                       openOnFocus = FALSE,
                                       selectOnTab = TRUE,
                                       closeAfterSelect = TRUE),
                        multiple = TRUE),
         
         selectizeInput('index',
                        "Benchmark Index",
                        choices = c('S&P 500'='^GSPC',
                                    'NASDAQ' = '^IXIC',
                                    'Dow Jones' = '^DJI')
                        ),
         
         dateRangeInput("daterange", 
                        "Date range:",
                        start = "2017-01-01",
                        end   = Sys.Date()),
         
         div(actionButton("generate", "Generate Portfolio"), class = 'text-center'),
         hr(),
         tags$div("Thank you for using this interactive portfolio builder! 
                  This is an educational tool meant to teach students about 
                  modern portfolio theory and risk adjusted returns.",
                  class = "about"),
         br(), br(),
         tags$div(tags$a(href = "https://dylanjm.github.io/",
                         tags$small(tags$em("Dylan McDowell - 2017"))), br(),
                  tags$a(href = "https://creativecommons.org/licenses/by/4.0/",
                         tags$small(tags$em("liscensed under "), icon("creative-commons"))),
                  class = 'my-name'),
         width = 3
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput("frontier"),
                           dataTableOutput("maxsharpe"),
                           dataTableOutput("minvar")),
          tabPanel("Summary", htmltools::h4('Correlation Coefficient Matrix'),
                              plotOutput("corr"),
                              dataTableOutput("stats"),
                              dataTableOutput("annual"),
                              dataTableOutput("capm")),
          tabPanel("Data", dataTableOutput("stocktab")),
          tabPanel("Info")
        ),
        width = 9
      )
   )
)

# Define server logic
server <- function(input, output) {
  
  observeEvent(input$generate, {
    
    stock.data <- tq_get(input$tickers,
                         get = "stock.prices",
                         from = input$daterange[1],
                         to = input$daterange[2]) %>%
      select(symbol, date, adjusted)
    
    performance.data <- stock.data %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly",
                   type       = "log",
                   col_rename = "Ra")
    
    index.data <- tq_get(input$index,
                         get = "stock.prices",
                         from = input$daterange[1],
                         to = input$daterange[2]) %>%
      tq_transmute(select     = adjusted, 
                   mutate_fun = periodReturn, 
                   period     = "monthly",
                   type       = "log",
                   col_rename = "Rb")
    
    RaRb <- left_join(performance.data, index.data, by = c("date" = "date"))
    
    portfolio.simulation <- performance.data %>%
      tq_repeat_df(n = 500)
    
    weights <- unlist(flatten(replicate(500, random.longonly(length(input$tickers)), simplify = FALSE)))
  
    stocks <- input$tickers
    
    weights.table <-  tibble(stocks) %>%
      tq_repeat_df(n = 500) %>%
      bind_cols(tibble(weights)) %>%
      group_by(portfolio)
    
    withProgress(message = paste('Optimizing Portfolio',"(This may take a minute)", sep = "\n"), value = 1, {
      
    portfolio.sim.monthly <- portfolio.simulation %>%
      tq_portfolio(assets_col  = symbol, 
                   returns_col = Ra, 
                   weights     = weights.table, 
                   col_rename  = "Ra")
    
    RaRb.multiple.portfolio <- portfolio.sim.monthly
    
    test <- RaRb.multiple.portfolio %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns) %>%
      rename("Returns" = "AnnualizedReturn", "Standard Deviation" = "AnnualizedStdDev", 
             "SR" = "AnnualizedSharpe(Rf=0%)")
    
    weights.spread <- spread(weights.table, key = "stocks", value = "weights")
    
    test.full <- left_join(test, weights.spread, by = "portfolio")
    
    print(test.full)
    
    output$frontier <- renderPlotly({
     g <- test %>%
       ggplot(aes(x=`Standard Deviation`,y=Returns, color = SR)) +
       geom_point() + 
       scale_colour_gradient(low = "#a60126", high = "#323896") +
       labs(title = "Monte Carlo Simulation ~ Efficient Frontier",
             subtitle= paste("Optimized Portfolio for:", input$tickers)) + 
       scale_y_continuous(labels = scales::percent) +
       scale_x_continuous(labels = scales::percent) +
       theme_bw()
     ggplotly(g)
    })
    
    output$maxsharpe <- renderDataTable({
      datatable(test.full[which.max(test.full$SR),],rownames = FALSE,
                caption = htmltools::tags$caption(htmltools::h4('Max Sharpe Ratio Portfolio')),
                style = 'bootstrap',
                options = list(dom = 't', autoWidth = TRUE)) %>%
        formatPercentage(1:ncol(test.full), digits = 3) %>%
        formatSignif(c("portfolio", "SR"), digits = 3)
    })
    
    output$minvar <- renderDataTable({
      datatable(test.full[which.min(test.full$`Standard Deviation`),],rownames = FALSE,
                caption = htmltools::tags$caption(htmltools::h4('Minimum Variance Portfolio')),
                style = 'bootstrap',
                options = list(dom = 't', autoWidth = TRUE))
    })
    
    })
    
    stat.table <- RaRb %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.Stats) %>%
      select(GeometricMean, Stdev, Skewness, Kurtosis, Minimum, Maximum)
    
    stocks.sharpe <- RaRb %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = SharpeRatio) %>%
      select(`StdDevSharpe(Rf=0%,p=95%)`)
    
    stocks.stats <- left_join(stat.table, stocks.sharpe, by = "symbol")
    
    stocks.annual <- RaRb %>%
      tq_performance(Ra = Ra, Rb = NULL, performance_fun = table.AnnualizedReturns) %>%
      select(AnnualizedReturn, AnnualizedStdDev, `AnnualizedSharpe(Rf=0%)`)
    
    capm.table <- RaRb %>%
      tq_performance(Ra = Ra, Rb = Rb, performance_fun = table.CAPM) %>%
      select(`R-squared`, Alpha, Beta, Correlation, TreynorRatio)
    
    output$stats <- renderDataTable({
      datatable(stocks.stats, rownames = FALSE,
                caption = htmltools::tags$caption(htmltools::h4('Monthly Stock Return Statistics')),
                style = 'bootstrap',
                colnames = c("Symbol", "Geometric Mean", "Std. Dev", 
                             "Skew", "Kurtosis", "Min", "Max", "Sharpe Ratio"),
                options = list(dom = 't', autoWidth = TRUE)) %>%
        formatPercentage(c('GeometricMean','Stdev','Minimum','Maximum'), 3) %>%
        formatSignif("StdDevSharpe(Rf=0%,p=95%)", digits = 3)
    })
    
    output$annual <- renderDataTable({
      datatable(stocks.annual, rownames = FALSE,
                caption = htmltools::tags$caption(htmltools::h4('Annualized Stock Return Statistics')),
                style = 'bootstrap',
                colnames = c("Annualized Returns", "Annualized Std Dev", "Sharpe Ratio"),
                options = list(dom = 't', autoWidth = TRUE)) %>%
        formatPercentage(c("AnnualizedReturn","AnnualizedStdDev"), 3) %>%
        formatSignif("AnnualizedSharpe(Rf=0%)", digits = 3)
    })
    
    output$capm <- renderDataTable({
      datatable(capm.table,rownames = FALSE,
                caption = htmltools::tags$caption(htmltools::h4('CAPM Model Statistics to Benchmark Index')),
                style = 'bootstrap',
                options = list(dom = 't', autoWidth = TRUE))
    })
    
    output$corr <- renderPlot({
      cor.dat <- performance.data %>%
        spread(key="symbol",value="Ra") %>%
        select(-date) %>% 
        cor()
      
      corrplot(cor.dat, method="square",
               order = "hclust", addrect = 2, col = col2(50))
    })
    
    output$stocktab <- renderDataTable({
      datatable(stock.data, rownames = FALSE,
                filter = 'top',
                colnames = c("Ticker", "Date", "Adjusted"),
                style = 'bootstrap',
                options = list(dom = 'ltp', autoWidth = TRUE)) %>%
        formatCurrency("adjusted", "$")
    })
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
