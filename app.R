# Modified Nils
# Load packages ----
library(shiny)
library(quantmod) # download stock data from the internet
library(magrittr) # the pipe 
library(tidyverse) # used for data wrangling, written by Hadley Wickham
library(zoo) # used for dealing with xts objects
library(lubridate)
library(plotly)


# User interface ----
ui <- fluidPage(
  title = "stockVis",
  tableOutput("table1"),
  #textOutput("text1"),
  plotlyOutput("plot1"),
  
  
  fluidRow(
    column(3,
           fileInput("transactions", "1. Choose transactions File",
                multiple = TRUE,
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".txt")),
           
           dateRangeInput("dates_fetch",
                     "2. Select date range to fetch",
                     start = "2014-05-05",
                     end = as.character(Sys.Date())),
           
           actionButton("reload", "3. Reload from Yahoo")
           
           ),
    
    
    
    column(3,
           dateInput("end_date_plot",
                     "4. Select end date for plot",
                     value = as.character(Sys.Date())),
           sliderInput("slidermonths", "Months", min=1, max = 11, value =3),
           sliderInput("slideryears", "Years", min=0, max = 15, value =0)
           ),
    
    column(3,
           selectInput("stockselect", "5. Select stock", choices = NULL)
           ),
    
    
    column(3,
           radioButtons("radio_perc_or_total", h3("Plot type"),
                        choices = list("% Gain" = 1, "€ Gain" = 2), selected = 1),
           
           actionButton("updategraph", "Update graph")
           )


  )
)

# Server logic
server <- function(input, output, session) {


# Hier werden die Daten eingelesen ----------------------------------------

transactions <- reactive({
  inFile <- input$transactions
  if (is.null(inFile))
    return(NULL)
  tbl <- read.table(inFile$datapath, dec=".", header = T)
  return(tbl)

})
  


# Hier kommt der Action button --------------------------------------------

  observeEvent(input$updategraph, {
    merg_df_noxts <- yahoo.data()
    stockselect <- input$stockselect
    enddate <- input$end_date_plot[1]
    startdate <- as.Date(Sys.Date()) %m-% months(3)
    slidermonths <- input$slidermonths
    slideryears <- input$slideryears
    startdate <- as.Date(Sys.Date()) %m-% months(slidermonths) %m-% years(slideryears)
    radio_perc_or_total <- input$radio_perc_or_total
    ncol_merg_df_noxts <- ncol(merg_df_noxts)
    
    if(radio_perc_or_total == 1){
      plot_append <- "_gain_perc"
      vol_append <- "_vol_owned"
      plot_y_label <- "Gain %"
      line_name <- "% Gain" 
      plot_df <-  select(merg_df_noxts, names(merg_df_noxts[,c(1,seq(5,ncol_merg_df_noxts,4),ncol_merg_df_noxts)]))
      vol_df <-  select(merg_df_noxts, names(merg_df_noxts[,c(1,seq(4,ncol_merg_df_noxts,4))]))
      g1 <- gather(plot_df, key = "stock", value = "gain",-date, na.rm = FALSE)
      g1b <- gather(vol_df, key = "stock", value = "vol_owned",-date, na.rm = FALSE)
      
      }
    
    if(radio_perc_or_total == 2){
      plot_append <- "_gain"
      vol_append <- "_vol_owned"
      plot_y_label <- "Absolute gain"
      line_name <- "€ Gain" 
      plot_df <-  select(merg_df_noxts, names(merg_df_noxts[,c(1,seq(2,ncol_merg_df_noxts,4))]))
      vol_df <-  select(merg_df_noxts, names(merg_df_noxts[,c(1,seq(4,ncol_merg_df_noxts,4))]))
      g1 <- gather(plot_df, key = "stock", value = "gain",-date) 
      g1b <- gather(vol_df, key = "stock", value = "vol_owned",-date, na.rm = FALSE)
    }
    
    if(stockselect != "all"){
    g1 <- filter(g1, stock %in% paste(stockselect, plot_append, sep="") & date > startdate & date < enddate)
    g1b <- filter(g1b, stock %in% paste(stockselect, vol_append, sep="") & date > startdate & date < enddate)
    } else{
    g1 <- filter(g1, date > startdate & date < enddate) 
    g1b <- filter(g1b, date > startdate & date < enddate)  
    }

    
    
     # Hier kommt der output ---------------------------------------------------

    output$table1 <- renderTable({
      #tail(merg_df, n = 20)
      #output_table <- merg_df_noxts
      #merg_df2[1] <- as.date(merg_df2[1])
      #output_table$date <- as.Date(output_table$date)
      #tail(merg_df_noxts)
    })

    output$plot1 <- renderPlotly({
      p <- plot_ly(g1, x = ~date) %>% 
        add_trace(y = ~gain, type = 'scatter', mode = 'lines', fill = 'tozeroy', name = line_name) %>% 
        add_trace(data = g1b, x = ~date, y = ~vol_owned, type = 'scatter', mode = 'lines', fill = 'red', yaxis = "y2", fill = 'tozeroy', name = "Shares owned") %>%
        layout(yaxis2 = list(showticklabels = F, overlaying = "y", showgrid = F, side = "right", range = c(0, max(g1b$vol_owned,na.rm = T)*4)))
      p
    })    
    
  })




# Begin Calculations ------------------------------------------------------


  yahoo.data <- eventReactive(input$reload, {
    transactions <- transactions()
    updateSelectInput(session, "stockselect", label = NULL, choices = c("grand_total", "all", levels(transactions[,5])),
                      selected = NULL)
    startdate <- input$dates_fetch[1]
    enddate <- input$dates_fetch[2]
    aN<-length(levels(transactions[,4]))
    
    
    
    # Loop 1 begin (main loop)
    
    for(a in 1:aN){
      loop_stocksym <- levels(transactions[,4])[a] # select stock symbol
      tmpstock_data <- getSymbols(loop_stocksym, #pull stock data
                                  from = startdate,
                                  to = enddate,
                                  src="yahoo",
                                  auto.assign=FALSE) %>% .[,4]
      stocknam <- filter(transactions, 
                         #select stock name from transactions (5th col of transactions)
                         stocksymbol == loop_stocksym) %>%
        select(stockname) %>% 
        .[1,] %>% 
        as.character() 
      sub_transactions<-transactions[transactions[,6]=="buy" & transactions[,5]==stocknam,]
      # subset transactions to stockname and buy
      tot_cost <- rep(0,nrow(tmpstock_data)); tot_cost
      tot_buyvol <- rep(0,nrow(tmpstock_data)); tot_buyvol
      # create empty vectors to be filled during the loop
      # vectors have the same length as the data that was pulled from Yahoo Finance
      
      
      # Loop 1.1 begin (1st loop within main loop)
      
      for(i in 1:nrow(sub_transactions)){ # run the loop for every buy transaction (nrow)
        tmp_price <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[i,1]),
                           0, 
                           sub_transactions[i,3])
        # if date is before purchase, then price = 0. If date is after purchase, 
        # then price = transaction cost price (3rd col of sub_transactions)
        
        tmp_vol <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[i,1]),
                          0,
                          sub_transactions[i,2])
        tmp_comm <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[i,1]),
                           0,
                           sub_transactions[i,7])
        # if date is before purchase, then volume = 0. If date is after purchase, 
        # then volume = transaction volume (2nd col of sub_transactions)
        
        tot_cost <- tot_cost + tmp_price * tmp_vol + tmp_comm; tot_cost
        # add price * vol to the empty vector that was defined before this loop. 
        # Value increases with every iteration of this loop (i.e., with every buy of same stock)
        
        tot_buyvol <- tot_buyvol + tmp_vol; tot_buyvol
        # add vol to the empty vector that was defined before this loop. 
        # Value increases with every iteration of this loop (i.e., with every buy of same stock)
        
        tmpstock_data<-merge(tmpstock_data, tmp_price, tmp_vol)
        # merge those dataframes to the pulled stock data
        
        colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_price"] <- paste("buyprice", 
                                                                              i, sep = "") 
        # rename those colnames of tmp_price by "buyprice" and the corresponding number
        
        colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_vol"] <- paste("buy_volume", 
                                                                             i, sep = "") 
        # same as above, rename by "buy_volume" and the corresponding number
      } # Loop 1.1 end
      
      tmpstock_data <- merge(tmpstock_data, tot_cost, tot_buyvol)
      # merge the created data frames with the stock data from the main loop
      
      # Now we do exactly the same, but with sell data in the next loop.
      tot_sellval <- rep(0,nrow(tmpstock_data)); tot_sellval
      tot_sellvol <- rep(0,nrow(tmpstock_data)); tot_sellvol
      
      sub_transactions<-transactions[transactions[,6]=="sell" & transactions[,5]==stocknam,]
      # subset transactions to "buyorsell" (6th col) = sell"
      
      # Loop 1.2 begin (2nd loop in main loop)
      
      for(k in 1:nrow(sub_transactions)){ 
        tmp_price <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[k,1]), 
                           0, sub_transactions[k,3]) 
        
        tmp_vol <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[k,1]),
                          0, sub_transactions[k,2])
        tmp_comm <- ifelse(index(tmpstock_data) < as.Date(sub_transactions[k,1]),
                           0,
                           sub_transactions[k,7])
        
        tot_sellval <- tot_sellval + tmp_price * tmp_vol - tmp_comm; tot_sellval
        tot_sellvol <- tot_sellvol + tmp_vol; tot_sellvol
        tmpstock_data<-merge(tmpstock_data, tmp_price, tmp_vol) 
        
        colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_price"] <- paste("sellprice", 
                                                                              k, sep = "") 
        
        colnames(tmpstock_data)[colnames(tmpstock_data)=="tmp_vol"] <- paste("sell_volume", 
                                                                             k, sep = "") 
        
      } # Loop 1.2 end 
      
      tmpstock_data <- merge(tmpstock_data, tot_sellval, tot_sellvol)
      # merge the created data frames with the stock data from the main loop
      
      tmpstock_data$vol_owned <- tmpstock_data$tot_buyvol - tmpstock_data$tot_sellvol 
      # volume of stock shares currently owned = bought shares - sold shares
      tmpstock_data$val_owned <- tmpstock_data$vol_owned * tmpstock_data[,1]
      # value owned = volume * price
      tmpstock_data$sum_eq <- tmpstock_data$val_owned + tmpstock_data$tot_sellval
      # the sum of our equity is owned stock + value of sales
      tmpstock_data$gain <- tmpstock_data$val_owned + tmpstock_data$tot_sellval - 
        tmpstock_data$tot_cost
      # our gain is value owned shares + value sold shares - total cost of shares
      
      tmpstock_data$gain_perc <- (tmpstock_data$val_owned + tmpstock_data$tot_sellval -
                                    tmpstock_data$tot_cost)/tmpstock_data$tot_cost*100
      # our gain % is (value owned shares + value sold shares -
      # total cost of shares) / total cost of shares
      
      assign(paste(stocknam, sep = ""), tmpstock_data)
      # save the data as an object with the name of the stock (stocknam)
    } # Loop 1 end
    
    
    
    merg_df <- get(levels(transactions[,5])[1])$gain
    names(merg_df)[1] <- "to_be_deleted" # this way we get a vector of the correct length
    
    bN <- length(levels(transactions[,5]))
    for(c in 1:bN){
      tmp_merg_df <- merge(get(levels(transactions[,5])[c])$gain,
                           get(levels(transactions[,5])[c])$tot_cost,
                           get(levels(transactions[,5])[c])$vol_owned)
      # merge gain and tot_cost into temp dataframe
      tmp_merg_df$gain_perc <- tmp_merg_df$gain / tmp_merg_df$tot_cost *100
      # calculate percent gain in temp dataframe
      names(tmp_merg_df)[1] <- paste(as.character(levels(transactions[,5])[c]),
                                     "gain", sep = "_")
      names(tmp_merg_df)[2] <- paste(as.character(levels(transactions[,5])[c]),
                                     "tot_cost", sep = "_")
      names(tmp_merg_df)[3] <- paste(as.character(levels(transactions[,5])[c]),
                                     "vol_owned", sep = "_") 
      names(tmp_merg_df)[4] <- paste(as.character(levels(transactions[,5])[c]),
                                     "gain_perc", sep = "_")
      merg_df <- merge(tmp_merg_df,merg_df)
    }
    
    merg_df<-merg_df[,1:ncol(merg_df)-1]
    # now delete the last column (I know this is ugly, but for now it has to suffice)
    
    ncol_merg_df <- ncol(merg_df) # how many cols are in merg_df?
    
    merg_df$grand_total_gain <- rowSums(merg_df[,seq(1,ncol_merg_df,4)],na.rm = TRUE) 
    merg_df$grand_tot_cost <- rowSums(merg_df[,seq(2,ncol_merg_df,4)]) 
    merg_df$grand_total_gain_perc <- rowSums(merg_df[,seq(1,ncol_merg_df,4)],na.rm = TRUE)/
      rowSums(merg_df[,seq(2,ncol_merg_df,4)],na.rm = TRUE)*100 
    # calculate grand total gain = gain of all stocks.
    # same principle for grand total cost and gain percentage.
    merg_df_noxts <- data.frame(date=index(merg_df), coredata(merg_df))
    ncol_merg_df_noxts <- ncol(merg_df_noxts)
    # we need to turn xts object into non-xts object, so ggplot can read it
    return(merg_df_noxts)


# Hier endet der output ---------------------------------------------------


  })
# Action button end -------------------------------------------------------



}
# server end


# Run the app
shinyApp(ui, server)
