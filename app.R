# BDA400 Assignment 6: Technical Analysis using R — Visualization Phase
# Run: install.packages(c('shiny','ggplot2','quantmod','TTR','dplyr')) ; shiny::runApp('app.R')
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)
library(dplyr)

ui <- fluidPage(
  titlePanel('Stock Portfolio Technical Analysis Dashboard'),
  sidebarLayout(
    sidebarPanel(
      textInput('symbol', 'Yahoo Finance ticker', 'AAPL'),
      dateRangeInput('date_range', 'Date range', start='2023-01-01', end=Sys.Date()),
      selectInput('time_frame', 'Time frame', c('Daily','Weekly','Monthly')),
      selectInput('chart_type', 'Chart type', c('Line','Candlestick','Area')),
      checkboxGroupInput('technical_indicators', 'Technical indicators',
        choices=c('Moving Averages','RSI','MACD'), selected='Moving Averages'),
      numericInput('short_n', 'Short SMA periods', 20, min=2, max=200),
      numericInput('long_n', 'Long SMA periods', 50, min=3, max=300),
      checkboxInput('show_signals', 'Annotate Buy/Sell crossover signals', TRUE),
      actionButton('refresh', 'Fetch / refresh data'),
      helpText('Yahoo Finance connection required. RSI and MACD use separate aligned panels because their scales differ from stock prices.')
    ),
    mainPanel(plotOutput('stock_chart', height='440px'),
              conditionalPanel("input.technical_indicators.includes('RSI')", plotOutput('rsi_chart', height='180px')),
              conditionalPanel("input.technical_indicators.includes('MACD')", plotOutput('macd_chart', height='200px')),
              h4('Most recent trading decision'), textOutput('latest_signal'),
              tableOutput('signal_table'), textOutput('status'))
  )
)

server <- function(input, output, session) {
  raw_stock <- eventReactive(input$refresh, {
    req(nzchar(trimws(input$symbol)), input$date_range)
    validate(need(input$date_range[1] <= input$date_range[2], 'Start date must precede end date.'))
    tryCatch({
      x <- suppressWarnings(getSymbols(toupper(trimws(input$symbol)), src='yahoo',
        from=as.Date(input$date_range[1])-450, to=as.Date(input$date_range[2])+1,
        auto.assign=FALSE))
      validate(need(NROW(x)>0, 'No stock data returned.'))
      x
    }, error=function(e) {validate(need(FALSE, paste('Unable to retrieve Yahoo Finance data:', conditionMessage(e))))})
  }, ignoreNULL=FALSE)

  analysis_data <- reactive({
    validate(need(input$short_n < input$long_n, 'Short SMA must be smaller than long SMA.'))
    x <- raw_stock()
    if(input$time_frame=='Weekly') x <- to.weekly(x, indexAt='endof', drop.time=TRUE)
    if(input$time_frame=='Monthly') x <- to.monthly(x, indexAt='endof', drop.time=TRUE)
    validate(need(NROW(x) >= max(input$long_n, 35), 'Not enough history to calculate the indicators. Choose a longer date range.'))
    close <- as.numeric(Cl(x))
    dates <- as.Date(index(x))
    out <- data.frame(Date=dates, Open=as.numeric(Op(x)), High=as.numeric(Hi(x)),
                      Low=as.numeric(Lo(x)), Close=close)
    out$ShortMA <- as.numeric(SMA(close, n=input$short_n))
    out$LongMA <- as.numeric(SMA(close, n=input$long_n))
    out$RSI <- as.numeric(RSI(close, n=14))
    macd <- MACD(close, nFast=12, nSlow=26, nSig=9, maType='EMA', percent=FALSE)
    out$MACD <- as.numeric(macd[,1]); out$MACDSignal <- as.numeric(macd[,2])
    # A signal occurs only on a CROSSOVER, not on every bar above/below the MAs.
    difference <- out$ShortMA-out$LongMA
    previous <- dplyr::lag(difference)
    out$Signal <- ifelse(!is.na(difference) & !is.na(previous) &
                         previous <= 0 & difference > 0, 'Buy',
                  ifelse(!is.na(difference) & !is.na(previous) &
                         previous >= 0 & difference < 0, 'Sell', 'Hold'))
    out <- out[out$Date>=as.Date(input$date_range[1]) &
               out$Date<=as.Date(input$date_range[2]), , drop=FALSE]
    validate(need(nrow(out)>0, 'No observations in selected date range.'))
    out
  })

  output$stock_chart <- renderPlot({
    d <- analysis_data()
    p <- ggplot(d, aes(x=Date)) + theme_minimal(base_size=12) +
      labs(title=paste(toupper(input$symbol), input$time_frame, 'price'),
           x=NULL, y='Price (USD)')
    if(input$chart_type=='Line') p <- p + geom_line(aes(y=Close), color='#2563eb', linewidth=.7)
    if(input$chart_type=='Area') p <- p + geom_area(aes(y=Close), fill='#93c5fd', alpha=.55) +
      geom_line(aes(y=Close), color='#2563eb')
    if (input$chart_type == "Candlestick") {
      d$Direction <- ifelse(d$Close >= d$Open, "Up", "Down")
      
      p <- p +
        geom_segment(
          data = d,
          aes(x = Date, xend = Date, y = Low, yend = High),
          color = "gray35"
        ) +
        geom_segment(
          data = d,
          aes(x = Date, xend = Date, y = Open, yend = Close,
              color = Direction),
          linewidth = 2.5
        ) +
        scale_color_manual(
          values = c(Down = "#d62728", Up = "#16a34a")
        )
    }
    if('Moving Averages' %in% input$technical_indicators) {
      p <- p + geom_line(aes(y=ShortMA, color='Short SMA'), na.rm=TRUE) +
        geom_line(aes(y=LongMA, color='Long SMA'), na.rm=TRUE) +
        scale_color_manual(values=c('Short SMA'='#f59e0b','Long SMA'='#7c3aed'),name='Overlay')
    }
    if(isTRUE(input$show_signals)) {
      sig <- d[d$Signal!='Hold',,drop=FALSE]
      p <- p + geom_point(data=sig, aes(y=Close, shape=Signal), color='#111827',size=3) +
        geom_text(data=sig,aes(y=Close,label=Signal),vjust=-1,size=3,check_overlap=TRUE) +
        scale_shape_manual(values=c(Buy=24,Sell=25))
    }
    print(p)
  })
  output$rsi_chart <- renderPlot({
    d <- analysis_data()
    ggplot(d,aes(Date,RSI)) + geom_line(color='#0f766e',na.rm=TRUE) +
      geom_hline(yintercept=c(30,70),linetype='dashed',color='#b45309') +
      coord_cartesian(ylim=c(0,100)) + theme_minimal() + labs(title='RSI (14)',x=NULL,y='RSI')
  })
  output$macd_chart <- renderPlot({
    d <- analysis_data()
    ggplot(d,aes(Date)) + geom_line(aes(y=MACD,color='MACD'),na.rm=TRUE) +
      geom_line(aes(y=MACDSignal,color='Signal'),na.rm=TRUE) +
      geom_hline(yintercept=0,linetype='dotted') + theme_minimal() +
      scale_color_manual(values=c(MACD='#2563eb',Signal='#dc2626')) +
      labs(title='MACD (12, 26, 9)',x=NULL,y='MACD',color='Line')
  })
  output$latest_signal <- renderText({
    d <- analysis_data(); paste('Latest bar:',tail(as.character(d$Date),1),
      '| Signal:',tail(d$Signal,1),'| Close:',round(tail(d$Close,1),2))
  })
  output$signal_table <- renderTable({
    d <- analysis_data(); tail(d[d$Signal!='Hold',c('Date','Close','Signal')],10)
  })
  output$status <- renderText('Educational demonstration only; crossover signals are not investment advice.')
}
shinyApp(ui,server)
