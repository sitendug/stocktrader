getRSI<- function(symbol, matype){

  stock<- BatchGetSymbols::BatchGetSymbols(tickers = symbol,
                          first.date = (Sys.Date()-15),
                          last.date=Sys.Date(),
                          freq.data = "daily",
                          do.cache = TRUE,
                          thresh.bad.data = 0)
  stock<- stock[[2]] |> as.data.frame() |> stats::na.omit()
  price<- stock[,"price.adjusted"]
  rsi<- TTR::RSI(price, n= (nrow(stock)-1), maType = matype, wts = stock[,"volume"])
  print(utils::tail(rsi,1))
}
getprojections<- function(symbol,var, forecast, duration){

  first.date = Sys.Date()- duration
  last.date = Sys.Date()-1
  freq.data = "daily"
  tickers <- c(symbol)
  # Download the data
  stocks <- BatchGetSymbols::BatchGetSymbols(tickers = tickers,
                                             first.date = first.date,
                                             last.date = last.date,
                                             freq.data = freq.data,
                                             do.cache = TRUE,
                                             thresh.bad.data = 0)

  stocks<- stocks[[2]] |> as.data.frame() |>
    janitor::clean_names() |>
    dplyr::mutate(ds = ref_date,
                  y = {{var}}) |>
    dplyr::select(ds, y)
  model1 <- prophet::prophet(stocks, daily.seasonality = TRUE, yearly.seasonality = TRUE)
  future1 <- prophet::make_future_dataframe(model1, periods = forecast)
  forecast<- stats::predict(model1, future1)
  #plot the model estimates
  prophet::prophet_plot_components(model1, forecast)
  prophet::dyplot.prophet(model1, forecast)
}
