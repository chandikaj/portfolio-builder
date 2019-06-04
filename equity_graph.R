library(plotly)

# Function to create the main equity graph
main_equity_graph <- function(df){
  
  p <- plot_ly()
  
  for (i in 2:ncol(df)){
    
    x = df[, 1]
    y = colnames(df)[i]
    p <- add_trace(p,
                   x = x,
                   y = df[[y]],
                   type = 'scatter',
                   mode = 'lines',
                   name = y)
  }
  
  p <- p %>% layout(xaxis = list(title = "Date", tickangle = 50),
                    yaxis = list(title = "Equity"))
  
 return(p)
}

# Single Equity Graph 
single_cluster_graph = function(MtoM, cumsum, max_cum, lower_band, dd, n){
  p <- plot_ly() 
  p <- add_trace(p = p, data = MtoM, x = cumsum[, 1], y = ~as.numeric(MtoM[n,-1]),
                 type = 'bar', name = 'MtoM')
  
  p <- add_trace(p = p, data = cumsum, x = cumsum[, 1], y = ~cumsum[[n+1]],
                 type='scatter', mode = 'lines', name = 'Ecurve')
  
  p <- add_trace(p = p, data = max_cum, x = cumsum[, 1], y = ~max_cum[[n+1]],
                 type = 'scatter', mode = 'lines', name = 'Ecurve_High')
  
  p <- add_trace(p = p, data = lower_band, x = cumsum[, 1], y = ~lower_band[[n+1]],
                 type = 'scatter', mode = 'lines', name = 'Stop Limit',
                 line = list(width = 3, dash = 'dash'))
  
  p <- add_trace(p = p, data = dd, x = cumsum[, 1], y = ~dd[[n+1]],
                 type = 'scatter', fill = 'tozeroy', mode = 'none', name = 'Drawdown',
                 fillcolor = 'rgba(220, 220, 220, 0.9')
  
  p <- p %>% layout(xaxis = list(title = "Date", tickangle = 50),
                    yaxis = list(title = "Equity"))
  
  return(p)
}

# Subcluster main equity graph 

sub_main_equity_graph <- function(cumsum_main, cumsum_sub, n){
  
  p <- plot_ly()
  
  # cumsum_main = cumsum_main[, -(n+1)]
  
  # # Main clusters traces 
  # for (i in 2:ncol(cumsum_main)){
  #   
  #   x = c(1:nrow(cumsum_main))
  #   y = colnames(cumsum_main)[i]
  #   p <- add_trace(p,
  #                  x = x,
  #                  y = cumsum_main[[y]],
  #                  type = 'scatter',
  #                  mode = 'lines',
  #                  name = y)
  # }
  
  sub_names = paste("Sub", colnames(cumsum_sub), sep = "_")
  
  # Sub clusters traces 
  for (i in 2:ncol(cumsum_sub)){
    
    x = cumsum_sub[, 1]
    y = colnames(cumsum_sub)[i]
    p <- add_trace(p,
                   x = x,
                   y = cumsum_sub[[y]],
                   type = 'scatter',
                   mode = 'lines',
                   name = sub_names[i])
  }
  
  p <- p %>% layout(xaxis = list(title = "Date", tickangle = 50),
                    yaxis = list(title = "Equity"))
  
  return(p)
}

# Output graph 
single_cluster_graph_final = function(MtoM, cumsum, max_cum, lower_band, dd){
  p <- plot_ly() 
  p <- add_trace(p = p, data = MtoM, x = c(1:(nrow(cumsum))), y = ~as.numeric(MtoM),
                 type = 'bar', name = 'MtoM')
  
  p <- add_trace(p = p, data = cumsum, x = c(1:nrow(cumsum)), y = ~cumsum[[1]],
                 type='scatter', mode = 'lines', name = 'Ecurve')
  
  p <- add_trace(p = p, data = max_cum, x = c(1:nrow(cumsum)), y = ~max_cum[[1]],
                 type = 'scatter', mode = 'lines', name = 'Ecurve_High')
  
  p <- add_trace(p = p, data = lower_band, x = c(1:nrow(cumsum)), y = ~lower_band[[1]],
                 type = 'scatter', mode = 'lines', name = 'Stop Limit',
                 line = list(width = 3, dash = 'dash'))
  
  p <- add_trace(p = p, data = dd, x = c(1:nrow(cumsum)), y = ~dd[[1]],
                 type = 'scatter', fill = 'tozeroy', mode = 'none', name = 'Drawdown',
                 fillcolor = 'rgba(220, 220, 220, 0.9')
  
  return(p)
}

# Seperate equity graph final output
output_seperate_graph <- function(cluster_file, data_file){
  
  p <- plot_ly()
  
  for (i in unique(cluster_file$clusters)){
    
    x = data_file[, 1]
    filtered = data_file[, cluster_file[cluster_file$clusters == i, 'strategies'], drop = F]
    summed = apply(filtered, MARGIN = 1, FUN = sum)
    cum_summed = cumsum(summed)
    
    p <- add_trace(p,
                   x = x,
                   y = cum_summed,
                   type = 'scatter',
                   mode = 'lines',
                   name = i)
  }
  
  p <- p %>% layout(xaxis = list(title = "Date", tickangle = 50),
                    yaxis = list(title = "Equity"))
  
  return(p)
}










