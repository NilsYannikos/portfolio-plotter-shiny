## app.R ##
library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(quantmod)
library(tidyquant)
library(plotly)
library(RColorBrewer)
library(rmarkdown)
library(gridExtra)

ui <- dashboardPage(
  dashboardHeader(title = "Portfolio Plotter"),
  ## Sidebar content
  dashboardSidebar(collapsed = FALSE,
                   sidebarMenu(
                     menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                     menuItem("Data tables", tabName = "Data_tables", icon = icon("th"))
                   )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Dashboard",
              fluidRow(
                column(3, offset = 0, style='padding:40px;',

                       fluidRow(
                         column(6,
                                fileInput("trans_upload", "Upload Transactions")
                         ),
                         column(6,
                                actionButton("generate_summary", "Summary", style = "margin-top: 25px;"),
                         )
                       ),

                       fluidRow(
                         column(12,
                                textOutput("status_text")
                         )
                       ),
                       fluidRow(
                         valueBoxOutput("absoluteGainBox", width = 6),
                         valueBoxOutput("percentageGainBox", width = 6),
                         valueBoxOutput("taxPaid", width = 6),
                         valueBoxOutput("commPaid", width = 6),
                       ),

                       selectInput("g2_select_y","Metric", choices = c("Absolute Gain", "% Gain", "Unrealized", "Realized", "Total Value", "Total Cost", "Total Tax"), selected = "Unrealized", selectize = FALSE),
                       plotlyOutput("Strawberrypie"),

                       selectInput("g3_select_y","Metric", choices = c("Absolute Gain", "% Gain", "Unrealized", "Realized", "Total Value", "Total Cost", "Total Tax"), selectize = FALSE),
                       plotlyOutput("barplotsummary")

                       #dataTableOutput("test123"),

                ),
                column(9, offset = 0, style='padding:40px;',

                       fluidRow(
                         column(12,
                                checkboxGroupInput("g1_select_sym","Stock Symbol", choices = NULL, inline = TRUE),
                         ),
                         column(1,
                                radioButtons(inputId = "abs_perc", label = "Gain", choices = c("€", "%"), selected = "€", inline = TRUE)
                         ),
                         column(6,
                                radioButtons(inputId = "summary_graph_years_slider", label = "Year Range", inline = TRUE, choices = c(1,2,3,4,5), selected=1),
                         ),
                         column(12,
                                plotlyOutput("portfolio_summary_graph", height = "500px")
                         ),
                         column(12,
                                plotlyOutput("portfolio_summary_graph2", height = "500px"),


                         )
                       ),



                ),
              ),





      ),

      # Second tab content
      tabItem(tabName = "Data_tables",
              fluidRow(
                     box(title = "Stocks and transactions", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",

                         fluidRow(
                           column(12,
                                  helpText("Select stock symbol, transaction date, volume, price, and commission fees. Clicking 'Add' or 'Sell' will add a row in the transaction table below. When clicking 'Summary', all stocks listed in the transaction Table will be downloaded from Yahoo Finance and plotted. By clicking 'Load Example', a pre-defined transaction table will be loaded. Transaction tables can be downloaded (Button below transaction table) and later re-uploaded and extended or analyzed."),
                                  splitLayout(
                                    textInput("Stock", "Stock", "AMZN", width = 80),
                                    dateInput("trans_Date","Transaction Date", Sys.Date())
                                  )
                           )

                         ),

                         fluidRow(
                           column(12,
                                  splitLayout(
                                    numericInput("trans_vol","Volume", value = 0),
                                    numericInput("trans_price","Price", value = 0),
                                    numericInput("trans_comm","Comm.", value = 0),
                                    numericInput("tax", "Tax", value = 0))
                           )
                         ),
                         fluidRow(
                           column(12,
                                  splitLayout(cellWidths = c("20%", "20%", "60%"),
                                              actionButton("trans_add", "Add", style = "margin-top: 25px;"),
                                              actionButton("trans_sell", "Sell", style = "margin-top: 25px;")
                                  )
                           )
                         ),
                         fluidRow(
                           column(12,

                                  downloadButton("download_report", "Download Report"))
                         ),




                     ),# Box end


                     #dataTableOutput("test123"),

              ),






              #### Now the piechart box





              #### Second Row - Table
              fluidRow(
                box(title = "Transactions", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    column(12,
                           dataTableOutput("trans_table")),
                    column(4,
                           downloadButton("trans_download", label = "Download")
                    )
                ),

              ),
              #### Third Row - Table
              fluidRow(
                box(title = "Individual stock performance within the portfolio", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    column(12,
                           dataTableOutput("stock_data_table", width = "auto")),
                    column(4,
                           downloadButton("complete_download", label = "Download"))
                )
              ),
              fluidRow(
                box(title = "The whole portfolio performance data (all stocks combined)", width = 12, solidHeader = TRUE, collapsible = TRUE, status = "primary",
                    column(12,
                           dataTableOutput("stock_data_table_united", width = "auto"))
                )
              )

      )
    )
  )
)


server <- function(input, output, session) {


  output$download_report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = function() return("myReport.pdf"),
    content = function(file) {
      src <- normalizePath("report.Rmd")
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, "report.Rmd", overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(data = portfolio_summary_rawdata(),
                     input_summary_graph_years_slider_1 = input$summary_graph_years_slider[1],
                     #input_summary_graph_years_slider_2 = input$summary_graph_years_slider[2],
                     input_g1_select_sym = input$g1_select_sym,
                     input_g2_select_y = input$g2_select_y
      )

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      out <- render("report.Rmd", output_file = file,
                    params = params,
                    envir = new.env(parent = globalenv())

      )
      file.rename(out, file)
    }
  )

  ### Transaction file download
  output$trans_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_tsv(trans_table$table, file)
    }
  )

  output$complete_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write_tsv(portfolio_summary_rawdata(), file)
    }
  )



  status_text <- reactiveValues(text = "Please load example or upload own transaction file")

  ### status_text CLEARED
  output$status_text <- renderText({
    print(status_text$text)
  })

  observeEvent(input$trans_clear,{
    status_text$text <- "Transaction data has been cleared. Click 'Load Example' or 'Browse'."
  })

  observeEvent(input$trans_example,{
    status_text$text <- "Example data loaded and added to transaction table. Please click 'Summary' to create graphs or 'Download' to download the transaction table."
  })

  observeEvent(input$generate_summary,{
    req(trans_table$table$Stock)
    status_text$text <- "Stock data downloaded from Yahoo Finance. Summary graphs and tables are ready for inspection."
  })

  observeEvent(input$trans_upload,{
    status_text$text <- "Data uploaded and added to transaction table. Please click 'Summary'."
  })



  stock_index_rawdata <- eventReactive(input$generate_summary, {
    req(trans_table$table$Stock)
    #market_indices <- c("^IXIC", "^GDAXI", "^DJI", "^HSI", "^FTSE")
    market_indices <- c("^IXIC")
    index_data <- market_indices %>%
      tq_get(get  = "stock.prices",
             from = "2017-01-01",
             to   = Sys.Date()) %>%
      group_by(symbol) %>%
      tq_transmute(select     = adjusted,
                   mutate_fun = periodReturn,
                   period     = "daily",
                   col_rename = "return") %>%
      mutate(return = return * 100) %>%
      rename(Date = date,
             `% Gain` = return,
             Stock = symbol)
    index_data
  })

  ### Generate Portfolio summary raw data (for graphs and data table)
  portfolio_summary_rawdata <- eventReactive(input$generate_summary, {
    req(trans_table$table$Stock)
    Stocks <- unique(trans_table$table$Stock)
    updateCheckboxGroupInput(session, "g1_select_sym", label = NULL, choices = c("All", Stocks),
                             selected = "All", inline = TRUE)
    stock_data <- Stocks %>%
      tq_get(get  = "stock.prices",
             from = "1998-01-01",
             to   = Sys.Date()) %>%
      rename(`total volume` = volume,
             Stock = symbol,
             Date = date)
    # merge stock data and transaction table
    stock_data <- left_join(stock_data, trans_table$table, by = c("Date", "Stock"))

    # calculations
    stock_data <- stock_data %>%
      mutate(buy_price = replace_na(buy_price, replace = 0),
             sell_price = replace_na(sell_price, replace = 0),
             buy_volume = replace_na(buy_volume, replace = 0),
             sell_volume = replace_na(sell_volume, replace = 0),
             cost = replace_na(cost, replace = 0),
             sell_tax = replace_na(sell_tax, replace = 0),
             sell_value = replace_na(sell_value, replace = 0),
             comm = replace_na(comm, replace = 0)) %>%
      group_by(Date, Stock) %>%

      summarize(buy_volume = sum(buy_volume),
                `Total Cost` = sum(cost),
                Realized = sum(sell_value),
                close = mean(close),
                `Total Tax` = sum(sell_tax),
                sell_volume = sum(sell_volume),
                comm = sum(comm)
      ) %>%
      mutate(Unrealized = buy_volume * close) %>%
      ungroup() %>%
      group_by(Stock) %>%
      mutate(current_volume = cumsum(buy_volume-sell_volume),
             `Total Cost` = cumsum(`Total Cost`),
             `Total Tax` = cumsum(`Total Tax`),
             Realized = cumsum(Realized),
             Unrealized = current_volume * close,
             `Total Value` = Realized + Unrealized,
             `Absolute Gain` = `Total Value` - `Total Cost`,
             `% Gain` = `Absolute Gain` / `Total Cost` * 100) %>%
      fill(close, Unrealized, `Total Value`, `Absolute Gain`, `% Gain`, comm) %>% 
      mutate(`% Gain` = replace_na(`% Gain`, 0))


    stock_data
  })

  ### Output stock_data as data table
  output$stock_data_table <- DT::renderDataTable({
    portfolio_summary_rawdata()
  })

  ### Generate Portfolio summary graph
  output$portfolio_summary_graph <- renderPlotly({
    cols <- brewer.pal(12, "Paired")
    data <- portfolio_summary_rawdata() %>%
      filter(Date >= (Sys.Date()- 365 * as.numeric(input$summary_graph_years_slider[1]))) %>%
      filter(if(input$g1_select_sym == "All"){Stock == Stock} else {Stock == input$g1_select_sym}) %>%
      mutate(`% Gain` = round(`% Gain`, digits = 2),
             `Absolute Gain` = round(`Absolute Gain`, digits = 2))

    if(input$abs_perc == "€"){
      g1 <- ggplot(data = data) +
        geom_line(mapping = aes(x = Date, y = `Absolute Gain`, color = Stock)) +
        scale_color_manual(values=cols) +
        ggtitle("Absolute Gain") +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              #panel.grid.major = element_blank(), # get rid of major grid
              #panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        ) +
        ggtitle("Absolute Gain (Per Position)")
      ggplotly(g1, tooltip = c("Date", "Stock", "Absolute Gain")) %>%
        config(displayModeBar = FALSE) %>%
        layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE), paper_bgcolor="rgba(0, 0, 0, 0)")
    } else {
      g1 <- ggplot(data = data) +
        geom_line(mapping = aes(x = Date, y = `% Gain`, color = Stock)) +
        scale_color_manual(values=cols) +
        ggtitle("% Gain") +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              #panel.grid.major = element_blank(), # get rid of major grid
              #panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        ) +
        ggtitle("% Gain (Per Position)")
      ggplotly(g1, tooltip = c("Date", "Stock", "% Gain")) %>%
        layout(paper_bgcolor="rgba(0, 0, 0, 0)") %>% #xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      config(displayModeBar = FALSE) 
    }
  })

  portfolio_summary_rawdata_united <- eventReactive(input$generate_summary, {
    portfolio_summary_rawdata() %>%
      group_by(Date) %>%
      summarize(`Total Cost` = sum(`Total Cost`),
                Realized = sum(Realized),
                `Total Tax` = sum(`Total Tax`),
                comm = sum(comm),
                `Total Value` = sum(`Total Value`),
                `Absolute Gain` = sum(`Absolute Gain`),
                `% Gain` = `Absolute Gain` / `Total Cost` * 100
      ) %>% 
      mutate(`% Gain` = replace_na(`% Gain`, 0))

  })

  ### Output stock_data as data table
  output$stock_data_table_united <- DT::renderDataTable({
    portfolio_summary_rawdata_united()
  })

  ### Generate Portfolio summary graph united
  output$portfolio_summary_graph2 <- renderPlotly({
    cols <- brewer.pal(12, "Paired")
    data <- portfolio_summary_rawdata_united() %>%
      filter(Date >= (Sys.Date()- 365 * as.numeric(input$summary_graph_years_slider[1]))) %>%
      mutate(`% Gain` = round(`% Gain`, digits = 2),
             `Absolute Gain` = round(`Absolute Gain`, digits = 2))
    index_data <- stock_index_rawdata() %>%
      filter(Date >= (Sys.Date()+ 365 * as.numeric(input$summary_graph_years_slider[1]))) %>%
      mutate(`% Gain` = cumsum(`% Gain`))



    if(input$abs_perc == "€"){
      g1 <- ggplot(data = data) +
        geom_area(mapping = aes(x = Date, y = `Absolute Gain`), fill = "#3c8cbc") +
        geom_line(mapping = aes(x = Date, y = `Absolute Gain`), color = "#3c8cbc") +
        ggtitle("Absolute Gain") +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              #panel.grid.major = element_blank(), # get rid of major grid
              #panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent")
        ) + # get rid of legend panel bg)
        ggtitle("Absolute Gain (Portfolio)")
      ggplotly(g1, tooltip = c("Date", "Absolute Gain")) %>%
        config(displayModeBar = FALSE) %>%
        layout(paper_bgcolor="rgba(0, 0, 0, 0)")
      #layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    } else {
      g1 <- ggplot(data = data) +
        geom_area(mapping = aes(x = Date, y = `% Gain`), fill = "#3c8cbc") +
        #geom_line(data = index_data, mapping = aes(x = Date, y = `% Gain`), color = "darkred") +
        ggtitle("% Gain") +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              panel.background = element_rect(fill = "transparent"), # bg of the panel
              plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
              #panel.grid.major = element_blank(), # get rid of major grid
              #panel.grid.minor = element_blank(), # get rid of minor grid
              legend.background = element_rect(fill = "transparent"), # get rid of legend bg
              legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
        ) +
        ggtitle("% Gain (Portfolio)")
      ggplotly(g1, tooltip = c("Date", "% Gain")) %>%
        layout(paper_bgcolor="rgba(0, 0, 0, 0)") %>% 
      config(displayModeBar = FALSE) #%>%
      #layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
    }
  })

  ### Valueboxes Summaries
  valueboxes<- reactiveValues()

  valueboxes$t1 <- tibble(`Absolute Gain` = 0,
                          `Total Cost` = 0,
                          `% Gain` = 0,
                          `Total Tax` = 0
  )

  ### Fill Valuebox data Table for use in valueboxes
  observeEvent(portfolio_summary_rawdata(), {
    valueboxes$t1 <- portfolio_summary_rawdata() %>%
      mutate(comm = cumsum(comm)) %>%
      ungroup() %>%
      fill(c(`Absolute Gain`, `% Gain`, `Total Tax`)) %>%
      filter(Date == max(.$Date)) %>%
      summarize(`Absolute Gain` = round(sum(`Absolute Gain`), digits = 2),
                `Total Cost` = sum(`Total Cost`),
                `Total Tax` = round(sum(`Total Tax`), digits = 2),
                comm = sum(comm)) %>%
      mutate(`% Gain` = round(`Absolute Gain` / `Total Cost` * 100, digits = 2))
  })

  # output$test123 <- renderDataTable({
  #   valueboxes$t1
  # })

  ### Valuebox 1 - Absolute Gain
  output$absoluteGainBox <- renderValueBox({
    if(valueboxes$t1$`Absolute Gain` < 0){
      valueBox(
        paste0(valueboxes$t1$`Absolute Gain`, "€"), "Absolute Gain (After Tax)", icon = icon("coins"),
        color = "red"
      )
    }
    else if(valueboxes$t1$`Absolute Gain` == 0){
      valueBox(
        paste0(valueboxes$t1$`Absolute Gain`, "€"), "Absolute Gain (After Tax)", icon = icon("coins"),
        color = "blue"
      )
    }
    else {
      valueBox(
        paste0(valueboxes$t1$`Absolute Gain`, "€"), "Absolute Gain (After Tax)", icon = icon("coins"),
        color = "green"
      )
    }
  })

  ### Valuebox 2 - Percentage Gain
  output$percentageGainBox <- renderValueBox({
    if(valueboxes$t1$`% Gain` < 0){
      valueBox(
        paste0(valueboxes$t1$`% Gain`, "%"), "% Gain", icon = icon("percent"),
        color = "red"
      )
    } else if(valueboxes$t1$`% Gain` == 0){
      valueBox(
        paste0(valueboxes$t1$`% Gain`, "%"), "% Gain", icon = icon("percent"),
        color = "blue"
      )
    }
    else {
      valueBox(
        paste0(valueboxes$t1$`% Gain`, "%"), "% Gain", icon = icon("percent"),
        color = "green"
      )
    }
  })


  ### Valuebox 3 - Total Tax Paid
  output$taxPaid <- renderValueBox({
    valueBox(
      paste0(valueboxes$t1$`Total Tax`, "€"), "Tax Paid", icon = icon("balance-scale"),
      color = "blue"
    )
  })

  ### Valuebox 4 - Commissions Paid
  output$commPaid <- renderValueBox({
    valueBox(
      paste0(valueboxes$t1$comm, "€"), "Commisions Paid", icon = icon("file-contract"),
      color = "blue"
    )
  })




  ### Generate Portfolio summary pie chart

  output$Strawberrypie <- renderPlotly({
    piechart <- portfolio_summary_rawdata() %>%
      fill(c(close, Unrealized, `Absolute Gain`, `% Gain`)) %>%
      filter(Date == max(.$Date))
    # Basic piechart

    cols <- brewer.pal(12, "Paired")
    fig <- plot_ly(piechart, labels = ~Stock, values = ~get(input$g2_select_y), type = 'pie', marker =list(colors = cols),
                   hovertemplate = "%{label}: %{value:.2f}<extra></extra>") %>%
      config(displayModeBar = FALSE)
    fig <- fig %>% layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          paper_bgcolor="rgba(0, 0, 0, 0)")

    fig <- hide_legend(fig)
  })

  output$barplotsummary <- renderPlotly({
    piechart <- portfolio_summary_rawdata() %>%
      fill(c(close, Unrealized, `Absolute Gain`, `% Gain`)) %>%
      filter(Date == max(portfolio_summary_rawdata()$Date))
    # Basic piechart
    cols <- brewer.pal(12, "Paired")
    fig <- ggplot(data = piechart) +
      geom_col(mapping = aes(x = reorder(Stock, get(input$g3_select_y)), y = get(input$g3_select_y), fill = Stock,text = paste(Stock, ": ", round(get(input$g3_select_y), digits = 2), sep ="") )) +
      theme_classic() +
      theme(legend.position = "none", axis.title.x = element_blank(), axis.title.y = element_blank()) +
      coord_flip() +
      theme(
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.grid.major = element_blank(), # get rid of major grid
        panel.grid.minor = element_blank(), # get rid of minor grid
        legend.background = element_rect(fill = "transparent"), # get rid of legend bg
        legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
      )
    ggplotly(fig, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
  })





  ### Create empty transaction table as reactive value
  trans_table <- reactiveValues(
    table = tibble(Stock = character(),
                   Date = ymd(),
                   comm = numeric(),
                   type = character(),
                   buy_volume = numeric(),
                   buy_price = numeric(),
                   cost = numeric(),
                   sell_volume = numeric(),
                   sell_price = numeric(),
                   sell_tax = numeric(),
                   sell_value = numeric()
    )
  )




  ### Clear transaction table
  observeEvent(input$trans_clear, {
    trans_table$table <- tibble(Stock = character(),
                                Date = ymd(),
                                comm = numeric(),
                                type = character(),
                                buy_volume = numeric(),
                                buy_price = numeric(),
                                cost = numeric(),
                                sell_volume = numeric(),
                                sell_price = numeric(),
                                sell_tax = numeric(),
                                sell_value = numeric()
    )
  })

  ### Upload transaction file
  observeEvent(input$trans_upload, {
    inFile <- input$trans_upload
    if (is.null(inFile))
      return(NULL)
    raw_data <- read_tsv(inFile$datapath) %>%
      mutate(cost = case_when(.$type == "buy" ~ buy_volume * buy_price + comm,
                              .$type == "sell" ~ 0),
             sell_value = case_when(.$type == "buy" ~ 0,
                                    .$type == "sell" ~ sell_volume * sell_price - comm - sell_tax)) %>%
      arrange(Date)
    trans_table$table <- bind_rows(trans_table$table, raw_data)
  })

  ### load test data
  observeEvent(input$trans_example, {
    raw_data <- read_tsv("example_data_AMZN_GOOG_NFLX.txt") %>%
      mutate(cost = case_when(.$type == "buy" ~ buy_volume * buy_price + comm,
                              .$type == "sell" ~ 0),
             sell_value = case_when(.$type == "buy" ~ 0,
                                    .$type == "sell" ~ sell_volume * sell_price - comm))
    trans_table$table <- bind_rows(trans_table$table, raw_data)
  })


  ### Add stock to transaction row: Merge input fields with empty transaction table and create reactive object
  observeEvent(input$trans_add, {
    trans_table$table <- bind_rows(
      trans_table$table,
      tibble(Stock = input$Stock,
             Date = input$trans_Date,
             comm = input$trans_comm,
             type = "buy",
             buy_volume = input$trans_vol,
             buy_price = input$trans_price,
             cost = buy_volume * buy_price + comm,
             sell_volume = 0,
             sell_price = 0,
             sell_tax = 0,
             sell_value = 0)
    )
  })


  ### Sell stock in transaction row:Merge input fields with empty transaction table and create reactive object
  observeEvent(input$trans_sell, {
    trans_table$table <- bind_rows(
      trans_table$table,
      tibble(Stock = input$Stock,
             Date = input$trans_Date,
             comm = input$trans_comm,
             type = "sell",
             buy_volume = 0,
             buy_price = 0,
             cost = 0,
             sell_volume = input$trans_vol,
             sell_price = input$trans_price,
             sell_tax = input$tax,
             sell_value = sell_volume * sell_price - comm - sell_tax
      )
    )
  })


  ### Display transaction table in output
  output$trans_table <- DT::renderDataTable({
    trans_table$table
  })

  ### Collect stock data (get Stock button)
  stock <- eventReactive(input$get_Stock,{
    tq_get <- input$Stock %>%
      tq_get(get  = "stock.prices",
             from = "1984-01-01",
             to   = Sys.Date())
    tq_get %>%
      rename(Stock = symbol)
  })

  output$Stock_overview <- renderPlot({
    ggplot(data = stock()) +
      geom_line(mapping = aes(x = Date, y = adjusted, color = Stock)) +
      theme(legend.position = "none")  #+
    #geom_line(mapping = aes(x = Date, y = cost0, color = Stock))
  })

}
shinyApp(ui, server)