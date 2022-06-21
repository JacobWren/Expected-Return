library(shiny)
library(downloader)
library(fortunes)
library(tidyverse)
library(data.table)
library(lubridate)
library(AER)
library(purrr)
library(XML)
library(quantmod)
library(stringr)
library(broom)

  
  function(input, output) {
    # NONReactive
    url <- paste("https://longforecast.com/sp-500-index-forecast-2017-2018-2019")
    webpage <- readLines(url)
    html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
    tableNodes <- getNodeSet(html, "//table")
    tab <- readHTMLTable(tableNodes[[1]],
                         header= c("Year", "Mo", "Min", "Max", "Close", "Mo%", "Total%"))
    
    tab <- tab[-1,]
    tab <- tab[13, ]
    tab <- data.frame(tab)
    
    tab$Total. <- str_replace_all(tab$Total., fixed("%"), "")
    ExpectedRet <- as.numeric(tab["Total."])
    
    # NONReactive 2
    rf <- getQuote("^IRX", what=yahooQF("Last Trade (Price Only)"))
    rf <- rf %>%
      select(Last)
    
    rf <- as.numeric(rf["Last"])/100
 
    # EARNINGSREACTIVE
    output$earningEstimates <- renderTable({
      url <- paste0("https://finance.yahoo.com/quote/", input$ticker, "/analysis?p=", input$ticker)
      webpage <- readLines(url)
      html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
      tableNodes <- getNodeSet(html, "//table")
      
      readHTMLTable(tableNodes[[1]])
    })
   
    # GROWTHREACTIVE 
    output$growthEst <- renderTable({
      url <- paste0("https://finance.yahoo.com/quote/", input$ticker, "/analysis?p=", input$ticker)
      webpage <- readLines(url)
      html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
      tableNodes <- getNodeSet(html, "//table")
      readHTMLTable(tableNodes[[6]])
    })
    
my_data <- reactive({ 
    
   if (input$Frequency == "Daily" & input$Model == "5 factor model"){

     s <- "FB"

      # REACTIVECOEF
         stocks_1 <- input$ticker
         p <- ("d")
         for (s in stocks_1) {
           url <- paste0("https://finance.yahoo.com/quote/", s, "/history?period1=1391932800&period2=1552118400&interval=1", p, "&filter=history&frequency=1", p)
           
           webpage <- readLines(url)
           html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
           tableNodes <- getNodeSet(html, "//table")
           
            # ASSIGN TO STOCK NAMED df_1
           assign(s, readHTMLTable(tableNodes[[1]])
           )
           df_1 <- get(s)
           df_1['stock'] <- s
           assign(s, df_1) 
         }
         
         
         # COMBINE ALL STOCK DATA 
         stockdatalist_1 <- cbind(mget(stocks_1))
         stockdata_1 <- do.call(rbind, stockdatalist_1)
         # MOVE STOCK ID TO FIRST COLUMN
         stockdata_1 <- stockdata_1[, c(ncol(stockdata_1), 1:ncol(stockdata_1)-1)]
         
         stockdata_2 <- stockdata_1 %>%
           filter(!grepl("Dividend",Open)) %>%
           filter(!grepl("Stock Split",Open))
         
         stockdata_2 <- stockdata_2[-1,]
         stockdata_2 <- head(stockdata_2,-1)
         
         stockdata_2$`Adj Close**` <- as.numeric(as.character(stockdata_2$`Adj Close**`))           
         
         stockdata_2 <- stockdata_2 %>%
           mutate(Return = (`Adj Close**`/lead(`Adj Close**`) - 1)) 
         stockdata_2 <- head(stockdata_2,-1)
         stockdata_2 <- stockdata_2 %>%
           select(Return,Date)
         
         FamaDaily <- read_csv("https://www.dropbox.com/s/pv2fqgeg24u6u3p/F-F_Research_Data_5_Factors_2x3_daily.CSV?dl=1", skip = 3)
         
         setnames(FamaDaily, "X1", "Date")
         FamaDaily[, 2:ncol(FamaDaily)] <- FamaDaily[, 2:ncol(FamaDaily)]/100
         
         FamaDaily %>%
           mutate_at(vars(-Date), funs(./100))
         
         FamaDaily<- FamaDaily[seq(dim(FamaDaily)[1],1),]
         
         FamaDaily$Date <- parse_date_time(FamaDaily$Date, order = "ymd", tz = "UTC")
         
         FamaDaily$Date <- format(as.Date(FamaDaily$Date, format = "%Y-%m-%d"), "%b %d, %Y")
         
         FamaDaily <- merge(x = FamaDaily, y = stockdata_2, by = "Date")
         FamaDaily <- FamaDaily %>%
           select(-Date,-RF)
         
         FamaDaily <- mutate_all(FamaDaily, function(x) as.numeric(as.character(x)))
         FamaDaily
         
   } else if (input$Frequency == "Monthly" & input$Model == "5 factor model"){
  
      stocks_1 <- input$ticker
      p <- ("mo")
      for (s in stocks_1) {
        url <- paste0("https://finance.yahoo.com/quote/", s, "/history?period1=1391932800&period2=1552118400&interval=1", p, "&filter=history&frequency=1", p)
        
        webpage <- readLines(url)
        html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
        tableNodes <- getNodeSet(html, "//table")
        
         # ASSIGN TO STOCK NAMED df_1
                 assign(s, readHTMLTable(tableNodes[[1]])
                 )
                 df_1 <- get(s)
                 df_1['stock'] <- s
                 assign(s, df_1)
      }
               # COMBINE ALL STOCK DATA
               stockdatalist_1 <- cbind(mget(stocks_1))
               stockdata_1 <- do.call(rbind, stockdatalist_1)
               # MOVE STOCK ID TO FIRST COLUMN
               stockdata_1 <- stockdata_1[, c(ncol(stockdata_1), 1:ncol(stockdata_1)-1)]
        
               stockdata_2 <- stockdata_1 %>%
                 filter(!grepl("Dividend",Open)) %>%
                 filter(!grepl("Stock Split",Open))
        
               stockdata_2 <- stockdata_2[-1,]
               stockdata_2 <- head(stockdata_2,-1)
        
               stockdata_2$`Adj Close**` <- as.numeric(as.character(stockdata_2$`Adj Close**`))
        
               stockdata_2 <- stockdata_2 %>%
                 mutate(Return = (`Adj Close**`/lead(`Adj Close**`) - 1))
               stockdata_2 <- head(stockdata_2,-1)
               stockdata_2 <- stockdata_2 %>%
                 select(Return,Date)

               FamaMonthly <- read_csv("https://www.dropbox.com/s/4qvsgte162t3r19/F-F_Research_Data_5_Factors_2x3.CSV?dl=1", skip = 3)
        
               setnames(FamaMonthly, "X1", "Date")
               FamaMonthly$`Mkt-RF` <- as.numeric(as.character(FamaMonthly$`Mkt-RF`))
               FamaMonthly$SMB <- as.numeric(as.character(FamaMonthly$SMB))
               FamaMonthly$HML <- as.numeric(as.character(FamaMonthly$HML))
               FamaMonthly$RMW <- as.numeric(as.character(FamaMonthly$RMW))
               FamaMonthly$CMA <- as.numeric(as.character(FamaMonthly$CMA))
               FamaMonthly$RF <- as.numeric(as.character(FamaMonthly$RF))
               FamaMonthly[, 2:ncol(FamaMonthly)] <- FamaMonthly[, 2:ncol(FamaMonthly)]/100

               stop_index <- min(which(is.na(FamaMonthly[,2]))) - 1

               FamaMonthly <- FamaMonthly[1:stop_index,]
        
               FamaMonthly %>%
                 mutate_at(vars(-Date), funs(is.na(as.numeric(.)/100)))
        
               FamaMonthly<- FamaMonthly[seq(dim(FamaMonthly)[1],1),]
        
               FamaMonthly$Date <- paste0(FamaMonthly$Date, "01")
               FamaMonthly$Date <- parse_date_time(FamaMonthly$Date, order = "ymd", tz = "UTC")
        
               FamaMonthly$Date <- format(as.Date(FamaMonthly$Date, format = "%Y-%m-%d"), "%b %d, %Y")
        
               FamaMonthly <- merge(x = FamaMonthly, y = stockdata_2, by = "Date")
               FamaMonthly <- FamaMonthly %>%
                 select(-Date,-RF)
        
               FamaMonthly <- mutate_all(FamaMonthly, function(x) as.numeric(as.character(x)))
                FamaMonthly

      }else if(input$Frequency == "Daily" & input$Model == "3 factor model"){

                         stocks_1 <- input$ticker
                         p <- ("d")
                         for (s in stocks_1) {
                           url <- paste0("https://finance.yahoo.com/quote/", s, "/history?period1=1391932800&period2=1552118400&interval=1", p, "&filter=history&frequency=1", p)
                  
                           webpage <- readLines(url)
                           html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
                           tableNodes <- getNodeSet(html, "//table")
                  
                           assign(s, readHTMLTable(tableNodes[[1]])
                           )
                           df_1 <- get(s)
                           df_1['stock'] <- s
                           assign(s, df_1)
                         }
                        
                         stockdatalist_1 <- cbind(mget(stocks_1))
                         stockdata_1 <- do.call(rbind, stockdatalist_1)
                         stockdata_1 <- stockdata_1[, c(ncol(stockdata_1), 1:ncol(stockdata_1)-1)]
                  
                         stockdata_2 <- stockdata_1 %>%
                           filter(!grepl("Dividend",Open)) %>%
                           filter(!grepl("Stock Split",Open))
                  
                         stockdata_2 <- stockdata_2[-1,]
                         stockdata_2 <- head(stockdata_2,-1)
                  
                         stockdata_2$`Adj Close**` <- as.numeric(as.character(stockdata_2$`Adj Close**`))
                  
                         stockdata_2 <- stockdata_2 %>%
                           mutate(Return = (`Adj Close**`/lead(`Adj Close**`) - 1))
                         stockdata_2 <- head(stockdata_2,-1)
                         stockdata_2 <- stockdata_2 %>%
                           select(Return, Date)
                  
                         Fama_3Factor_Daily <- read_csv("https://www.dropbox.com/s/b5n41wr77urgahj/F-F_Research_Data_Factors_daily.CSV?dl=1", skip = 3)
                  
                         setnames(Fama_3Factor_Daily, "X1", "Date")
                         Fama_3Factor_Daily[, 2:ncol(Fama_3Factor_Daily)] <- Fama_3Factor_Daily[, 2:ncol(Fama_3Factor_Daily)]/100
                  
                         stop_index <- min(which(is.na(Fama_3Factor_Daily[,2]))) - 1
                  
                         Fama_3Factor_Daily <- Fama_3Factor_Daily[1:stop_index,]
                  
                         Fama_3Factor_Daily %>%
                           mutate_at(vars(-Date), funs(./100))
                  
                         Fama_3Factor_Daily<- Fama_3Factor_Daily[seq(dim(Fama_3Factor_Daily)[1],1),]

                         Fama_3Factor_Daily$Date <- parse_date_time(Fama_3Factor_Daily$Date, order = "ymd", tz = "UTC")
                  
                         Fama_3Factor_Daily$Date <- format(as.Date(Fama_3Factor_Daily$Date, format = "%Y-%m-%d"), "%b %d, %Y")
                  
                         Fama_3Factor_Daily <- merge(x = Fama_3Factor_Daily, y = stockdata_2, by = "Date")
                         Fama_3Factor_Daily <- Fama_3Factor_Daily %>%
                           select(-Date,-RF)
                  
                         Fama_3Factor_Daily <- mutate_all(Fama_3Factor_Daily, function(x) as.numeric(as.character(x)))
                  
                }else{          
                           stocks_1 <- input$ticker
                           p <- ("mo")
                           for (s in stocks_1) {
                             url <- paste0("https://finance.yahoo.com/quote/", s, "/history?period1=1391932800&period2=1552118400&interval=1", p, "&filter=history&frequency=1", p)
                    
                             webpage <- readLines(url)
                             html <- htmlTreeParse(webpage, useInternalNodes = TRUE, asText = TRUE)
                             tableNodes <- getNodeSet(html, "//table")
                    
                             assign(s, readHTMLTable(tableNodes[[1]])
                             )
                             df_1 <- get(s)
                             df_1['stock'] <- s
                             assign(s, df_1)
                                            }
                 
                  stockdatalist_1 <- cbind(mget(stocks_1))
                  stockdata_1 <- do.call(rbind, stockdatalist_1)

                  stockdata_1 <- stockdata_1[, c(ncol(stockdata_1), 1:ncol(stockdata_1)-1)]
                  
                  stockdata_2 <- stockdata_1 %>%
                    filter(!grepl("Dividend",Open)) %>%
                    filter(!grepl("Stock Split",Open))
                  
                  stockdata_2 <- stockdata_2[-1,]
                  stockdata_2 <- head(stockdata_2,-1)
                  
                  stockdata_2$`Adj Close**` <- as.numeric(as.character(stockdata_2$`Adj Close**`))
                  
                  stockdata_2 <- stockdata_2 %>%
                    mutate(Return = (`Adj Close**`/lead(`Adj Close**`) - 1))
                  stockdata_2 <- head(stockdata_2,-1)
                  stockdata_2 <- stockdata_2 %>%
                    select(Return,Date)

                  Fama_3Factor_Monthly <- read_csv("https://www.dropbox.com/s/wo6aoyq3r1x1jr4/F-F_Research_Data_Factors.CSV?dl=1", skip = 3)
                  
                  setnames(Fama_3Factor_Monthly, "X1", "Date")
                  Fama_3Factor_Monthly[, 2:ncol(Fama_3Factor_Monthly)] <- Fama_3Factor_Monthly[, 2:ncol(Fama_3Factor_Monthly)]/100
                  
                  stop_index <- min(which(is.na(Fama_3Factor_Monthly[,2]))) - 1
                  
                  Fama_3Factor_Monthly <- Fama_3Factor_Monthly[1:stop_index,]
                  
                  Fama_3Factor_Monthly %>%
                    mutate_at(vars(-Date), funs(./100))
                  
                  Fama_3Factor_Monthly<- Fama_3Factor_Monthly[seq(dim(Fama_3Factor_Monthly)[1],1),]
                  
                  Fama_3Factor_Monthly$Date <- paste0(Fama_3Factor_Monthly$Date, "01")
                  Fama_3Factor_Monthly$Date <- parse_date_time(Fama_3Factor_Monthly$Date, order = "ymd", tz = "UTC")
                  
                  Fama_3Factor_Monthly$Date <- format(as.Date(Fama_3Factor_Monthly$Date, format = "%Y-%m-%d"), "%b %d, %Y")
                  
                  Fama_3Factor_Monthly <- merge(x = Fama_3Factor_Monthly, y = stockdata_2, by = "Date")
                  Fama_3Factor_Monthly <- Fama_3Factor_Monthly %>%
                    select(-Date,-RF)
                  
                  Fama_3Factor_Monthly <- mutate_all(Fama_3Factor_Monthly, function(x) as.numeric(as.character(x)))
                  }
}) 

coef <- reactive({    
  
  if(input$Model == "5 factor model"){
         lm(Return ~`Mkt-RF` + SMB + HML + RMW + CMA, data = my_data())
  } else{
    lm(Return ~`Mkt-RF` + SMB + HML, data = my_data())
  }

})     
       
output$Ttest <- renderTable({
      # REACTIVETTEST
         correctvcov <- vcovHC(coef(),"HC1")
         output <- round(coeftest(coef(), vcov = correctvcov),4)
         map_df(list(output), tidy)
})

output$Rates <- renderPrint({
       
       # REACTIVECOEF
       #Coef_Mkt <- summary(coef())$coefficients[2, 1]
       #Coef_SMB <- summary(coef())$coefficients[3, 1]
       #Coef_HML <- summary(coef())$coefficients[4, 1]
       
       if (input$Frequency == "Daily" & input$Model == "5 factor model"){
        
         Coef_Mkt <- summary(coef())$coefficients[2, 1]
         Coef_SMB <- summary(coef())$coefficients[3, 1]
         Coef_HML <- summary(coef())$coefficients[4, 1]  
         Coef_RMW <- summary(coef())$coefficients[5, 1]
         Coef_CMA <- summary(coef())$coefficients[6, 1]
       
         avg_SMBD <- mean(my_data()$SMB)
         avg_HMLD <- mean(my_data()$HML)
         avg_RMWD <- mean(my_data()$RMW)
         avg_CMAD <- mean(my_data()$CMA)

         # FAIR RATE
         
         Fair_Rate <- round(((rf + Coef_Mkt*(ExpectedRet-rf) + Coef_SMB*(avg_SMBD) + 
                      Coef_HML*(avg_HMLD) + Coef_RMW*(avg_RMWD) + Coef_CMA*(avg_CMAD))), 2)
         
         } else if (input$Frequency == "Monthly" & input$Model == "5 factor model"){
           
           Coef_Mkt <- summary(coef())$coefficients[2, 1]
           Coef_SMB <- summary(coef())$coefficients[3, 1]
           Coef_HML <- summary(coef())$coefficients[4, 1]  
           Coef_RMW <- summary(coef())$coefficients[5, 1]
           Coef_CMA <- summary(coef())$coefficients[6, 1]
           
           avg_SMBD <- mean(my_data()$SMB)
           avg_HMLD <- mean(my_data()$HML)
           avg_RMWD <- mean(my_data()$RMW)
           avg_CMAD <- mean(my_data()$CMA)
           
           #FAIR RATE
           
           Fair_Rate <- round(((rf + Coef_Mkt*(ExpectedRet-rf) + Coef_SMB*(avg_SMBD) + 
                         Coef_HML*(avg_HMLD) + Coef_RMW*(avg_RMWD) + Coef_CMA*(avg_CMAD))), 2)
         
       } else{
         
         Coef_Mkt <- summary(coef())$coefficients[2, 1]
         Coef_SMB <- summary(coef())$coefficients[3, 1]
         Coef_HML <- summary(coef())$coefficients[4, 1]  
         
         avg_SMBD <- mean(my_data()$SMB)
         avg_HMLD <- mean(my_data()$HML)
         
         # FAIR RATE
         
         Fair_Rate <- round(((rf + Coef_Mkt*(ExpectedRet-rf) + Coef_SMB*(avg_SMBD) + 
                       Coef_HML*(avg_HMLD))), 2)
         
       }
         
         Fair_Rate <- paste0(Fair_Rate, "%")
       
         ExpectedRet <- paste0(ExpectedRet, "%")

       x <- paste(paste("Expected Market Return", ExpectedRet), paste("Expected Rate of Return", Fair_Rate), sep = "\n")
       cat(x, sep = '\n')
       
   })  
}
