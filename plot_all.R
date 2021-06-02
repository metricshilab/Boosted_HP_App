#===========================================================
# plot xts
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# By: Chen Yang (chen_yang@link.cuhk.edu.hk)
#     Mei Ziwei (zwmei@link.cuhk.edu.hk)
# Date: 2019-05-23 (by Chen Yang)
# Update: 2021-06-01 (by Mei Ziwei)
#===========================================================

Boostedplot <- function(bt_results, plot_type, 
                        Date = NULL, Frequency = NULL){
  
  
  
  cycle <- bt_results$cycle 
  
  trend_hist <- bt_results$trend_hist
  
  trend <- bt_results$trend
  
  rawdata <- cycle + trend
    
  test_type <- bt_results$test_type
  # 
  if (test_type == "BIC" || test_type == "none"){
    history <- bt_results$IC_hist
    hist_title <- "IC value history"
  }

  if (test_type == "adf"){
    history <- bt_results$adf_p_hist
    hist_title <- "ADF test p-value history"
  }
  
  
  if (Frequency == "none" || is.null(Frequency) || is.null(Date) ){
    Date_series = 1:length(cycle)
  }else{
    Date_series = seq(from = Date, length.out = length(cycle), by = Frequency)
  }

  
  # if(plot_type == "Plain"){
  #   
  #   message("under developing") 
  #   
  # }
  # 
  # if(plot_type == "Time Series"){
  
  
  
  leg <- length(rawdata)
  leg_h <- length(history)
  
  if (plot_type == "trend"){
    
    
    mdf_HP_plot <- melt(trend_hist)
    if (dim(mdf_HP_plot)[2]==1){
      mdf_HP_plot$time = 1:dim(mdf_HP_plot)[1]
      mdf_HP_plot$trend = 1
      }else{colnames(mdf_HP_plot) <- c("time","trend","value")}
    mdf_HP_plot$date <- Date_series[mdf_HP_plot$time]
    ggp = ggplot(mdf_HP_plot) +
      theme_bw() +
      theme(panel.grid=element_blank() 
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank(),
            ,legend.position = "bottom") +
            geom_line(aes(x=date, y=value, group=trend, color = "interation_history"), 
                      size=0.5, alpha=0.5)
    
    Trend <- data.frame(trend = trend, raw = rawdata, date = Date_series)
    ggp = ggp + geom_line(data = Trend, aes(x=date, y=trend, group=NULL, color = "trend"), 
                          size=1, alpha=1)
    ggp = ggp + geom_line(data = Trend, aes(x=date, y=raw, group=NULL, color = "raw_data"), 
                          size=1, alpha=1)
    
  
    pl1 = ggp + 
          scale_colour_manual(name=NULL,
                              values=c(raw_data="red", 
                                       trend="blue", 
                                       interation_history="grey"))+
          labs(title="Trend")

    #as.xts(bx_ADF$adf_p_hist, Sys.Date()+1:leg_p)
    return(pl1)

  }
  
  if (plot_type == "cycle") {
    Cycle <- data.frame(cycle = cycle)
    Cycle$date <- Date_series
    
    
    pl2 <- ggplot(Cycle) +
      theme_bw() +
      theme(panel.grid=element_blank() 
            #axis.text.y = element_blank(),
            #axis.ticks.y = element_blank(),
            ,legend.position = "bottom") +
      geom_line(aes(x=date, y=cycle, group=NULL), 
                size=1, alpha=1, color = "blue")+
      labs(title="Cyclic Component")
    
    
    return(pl2)
  }
  
  
  if (plot_type == "history") {
    # bx <- data.frame(cycle = cycle,raw_data=rawdata)
    # bx <- as.xts(bx, as.Date(Date) + 1:leg)
  
    iter_num = bt_results$iter_num
    mpg <- data.frame(iter_num = 1:iter_num, value = history[1:(iter_num)])
    pl3 <- ggplot(mpg,aes(iter_num,value))+
      geom_point(stat="identity",color="steelblue")+
      geom_line(stat="identity",color="steelblue")+
      labs(title=hist_title)
    # addLegend("topleft",
    #           legend.names = legend_hist, 
    #           lty=c(1, 1), lwd=c(2, 1),
    #           col=c("purple"))
    
    return(pl3)
  }
  

  # }

# 
#   if(plot_type == "SVG"){
# 
#     message("under developing")
#   }
# 
# 
#   if(plot_type == "GGPlot"){
  #   
  #   message("under developing")
  # }
  # 
  # 
  # if(plot_type == "Dynamic"){
  #   
  #   message("under developing")
  # }
  
}