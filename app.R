
######################################################################################################################################

######################################################################################################################################
# Title: DCF Valuation using R 
# Author: Nick Hosler
#
# Purpose: Destroy all bankers
#
# Credits: R Blogger - systematicinvestor and his packages - great resource!
# Link : https://www.r-bloggers.com/company-valuation-using-discounted-cash-flows/
#
# Details: 
# The point of this is to make a script that can spit out a DCF valuation of a given stock for given parameter changes
# with the intent to eventually enable it for shiny, thereby making it universally accessible, and ruining the need 
# for traditional analysts - cause HA! 
#
########################################################################################################################################



#simople app to show intrinsic value based on percepetion 

library(shiny)
library(lubridate)
library(graphics)
library(rsconnect)
library(shinycssloaders)
library(quantmod)
library(debugme)
library(praise)
library(testthat)

# setwd()


source("secret.R")

#this should connect to shiny, will add secret.R to .gitignore
connectToShiny()

# this is where you create references to your inputs 
# will need assumptions for growth periods and such 
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      titlePanel(title = "DCF Valuation App"),
      # Copy the line below to make a text input box
      textInput( inputId = "ticker", label = h3("Enter Valid Ticker Symbol"), value = "AMZN"),
      selectInput(inputId = "growth", label =  h3("Long Run Growth Forecast"), selected = 3,
                  choices = list("1%" = 1,
                                 "2%" = 2,
                                 "3%" = 3,
                                 "4%" = 4,
                                 "5%" = 5)),
      
      selectInput(inputId = "discount", label =  h3("Estimated Discount Rate"), selected = 9,
                  choices = list("7%" =  7,
                                 "8%" =  8,
                                 "9%" =  9,
                                 "10%" = 10,
                                 "11%" = 11,
                                 "12%" = 12)),
      
      p("This app is designed to compute and map the historical DCF valuation for various companies by scraping data off the web from Yahoo Finance"), 
      p("Please note: DCF valuations do not work for firms without positive cash flows"),
      p("Assumptions: Company will grow for the first 3 years at current Growth Rate slowed down by 20% for the next 4 years, and slowed down by a further 20% for the next 3, then you select the growth for the next 10 years"),
      p("To use it, please enter the ticker you desire, Load the Data, and click the COMPUTE button"),
      p("You can update your assumptions, and produce a new DCF chart by clicking COMPUTE again"),
      
      actionButton("load", label = "LOAD DATA"),
      actionButton("do", label = "COMPUTE"), 
      
    tags$head(tags$style(type="text/css", "
             #loadmessage {
                         position: fixed;
                         top: 0px;
                         left: 0px;
                         width: 100%;
                         padding: 5px 0px 5px 0px;
                         text-align: center;
                         font-weight: bold;
                         font-size: 100%;
                         color: #ffffff;
                         background-color: #aabacb;
                         z-index: 105;
                         }
                         ")),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading Data, Please Wait...",id="loadmessage"))),

    
    
    mainPanel(
      
      
      plotOutput("intrinsics") %>% withSpinner(color="#aabacb"),
      plotOutput("fcf"),# %>% withSpinner(color="#aabacb"),
      plotOutput("growth") #%>% withSpinner(color="#aabacb")
     # tableOutput('table') %>% withSpinner(color="#aabacb")
      
      
    )
  )
  
)

server <- function(input, output, session){

  observeEvent(input$load, {
    
    
    
    # Libs 
    tryCatch(library(quantmod),error=function(cond){install.packages('quantmod');library(quantmod)}) 
    
    ###############################################################################
    # Load Systematic Investor Toolbox (SIT)
    # http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
    ###############################################################################
    
    con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
    #source("code.r")
    source(con)
    close(con)
    
    #*****************************************************************
    # Load historical fundamental and pricing data
    #****************************************************************** 
   
    tryCatch({
      
    ticker = toupper(input$ticker)
    ticker = gsub(" ", "", ticker)

    tickers = spl(paste(ticker))
    tickers.temp = spl(paste0('NASDAQ:',ticker))

    print("loading 1")

    # get fundamental data


    data.fund <- new.env()
    for(i in 1:len(tickers)) {
      data.fund[[tickers[i]]] = fund.data(tickers.temp[i], 80, 'annual')

      print("Loading Part ", i)
    }

    print("loading 2")
    # get pricing data
    data <- new.env()
    getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
    for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adjusted=T)
    print("loading 3")
    
    
    # prepare data
    fund = data.fund[[tickers[1]]]
    fund.date = date.fund.data(fund)            
    price = Cl(data[[tickers[1]]]['1995::']) 
       
    tryCatch({
  
  
  observeEvent(input$do, {
    
    # Step 2 
    
    #*****************************************************************
    # Extract Inputs for DCF Valuation
    #******************************************************************                 
    # Free Cash Flows
    FCF = get.fund.data('free cash flow', fund, fund.date)
    
    # Invested Capital
    IC = get.fund.data('invested capital', fund, fund.date)
    
    # Sales
    SALE = get.fund.data('total revenue', fund, fund.date)
    
    # Common Equity
    CEQ = get.fund.data('total equity', fund, fund.date)
    
    # Common Shares Outstanding
    CSHO = get.fund.data('total common shares out', fund, fund.date)
    
    # Growth Rate
    CROIC = FCF/IC
    
    # Average inputs
    check <- tryCatch({              
      g = runMean(CROIC, 5)
      cash = runMean(FCF, 5)
    },error=function(e){return()})
    
    
    if (is.null(check) == FALSE ) {
      
      print("entered")
      g = runMean(CROIC, 5)
      cash = runMean(FCF, 5)
   
    
    # Step 3 
    #*****************************************************************
    # Helper function to compute Intrinsic Value
    #******************************************************************                 
    compute.DCF.IV <- function(cash, eqity, shares, g, R) {
      if( cash <= 0 ) return(NA)
      
      if( len(R) == 1 ) R = rep(R, len(g))
      
      value = eqity + sum(cash * cumprod(1 + g) / cumprod(1 + R))
      return( value / shares )
    }
    
    # Step 4 
    #*****************************************************************
    # Compute Intrinsic Value, assumptions:
    # Company will grow for the first 3 years at current Growth Rate
    # slowed down by 20% for the next 4 years, and slowed down by a further 20% for the next 3 years
    # and finally 3% growth for the next 10 years
    #
    # The Discount Rate is 9%
    #
    # http://www.oldschoolvalue.com/blog/stock-analysis/apple-aapl-valuation/
    #******************************************************************   
    
    
    # Input checking
    # cat(as.integer(input$discount)," input discount")
    # 
    # cat(input$discount, "discout")
    # this is where I would ant to tweek the inputs to generate a decf from any set of assumptions 
    # if I put this on the web it would blow minds 
    dcf.price = NA * g
    i.start = which(!is.na(g))[1] 
    
    for(i in i.start : nrow(g)) {
      # Create Growth Rate scenario:      
      g.scenario = c(rep(g[i],3), rep(g[i],4)*0.8, rep(g[i],3)*0.8*0.8, rep(as.integer(input$growth)/100,10))
      
      # Compute Intrinsic Value
      dcf.price[i] = compute.DCF.IV(cash[i], CEQ[i], CSHO[i], g.scenario, as.integer(input$discount)/100)
    } 
    
    
    
    #*****************************************************************
    # Create Plots
    #****************************************************************** 
    plota(price, type='l', log = 'y', col='blue', main=tickers[1],
          ylim=range(price,dcf.price,na.rm=T))
   
    plota.lines(na.omit(dcf.price), type='s', col='red', lwd=2)
    
    
    plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))   
    
    
    # plota(g, type='b', col='blue', pch=0, main='Growth Rate')
    # 
    # 
    # plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')
    #
    # COmpute this is a logic checker for whether you've pressed action button yet 
   
    #######################################################################################################################################  
    
    output$intrinsics <- renderPlot({
      
      
      plota(price, type='l', log = 'y', col='blue', main=tickers[1],
            ylim=range(price,dcf.price,na.rm=T))
      
      tryCatch({ 
     
      plota.lines(na.omit(dcf.price), type='s', col='red', lwd=2)
      
    },error=function(e){return()} ) 
      
      plota.legend('Close,Intrinsic Value', 'blue,red', list(price, dcf.price))   
      
    })
    
    output$fcf <- renderPlot({
     
      plota(cash, type='b', col='blue', pch=0, main='Free Cash Flows')
     
    })
    
    output$growth <- renderPlot({
      
      plota(g, type='b', col='blue', pch=0, main='Growth Rate')
      
    }) } else if(is.null(check) == TRUE) {
      
      print("exited")
      
      
      showModal(modalDialog(
        title = "Cash Flow Error",
        "Either the stock you entered likely has not generated positive cash flows, or has information that is not available on Yahoo Finance. Program will reload in 10 seconds"))
      Sys.sleep(10)
      session$reload()
    }
      
  }) },error=function(e){
    
    showModal(modalDialog(
      title = "Error",
      "Either the stock you entered is not listed or has not generated positive cash flows, please try again."
    ))
    
    return()} )  },error=function(e){
    
    showModal(modalDialog(
      title = "Error",
      "Either the stock you entered is not listed or has not generated positive cash flows, please try again."
    ))
    
    return()} ) 
    
  })
  
}

shinyApp(ui, server)


