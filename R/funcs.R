# color function for reactable cells
colfun <- function(x){
  
  out <- case_when(
    x == 'No Data' ~ 'lightblue', 
    x == 'Green' ~ 'green', 
    x == 'Yellow' ~ 'yellow', 
    x == 'Orange' ~ 'orange', 
    x == 'Red' ~ 'red'
  )
  
  return(out)
  
}

# summary function for gauges
sumfun <- function(mapsel, sumsel, typsel, filt){
  
  # select color
  gaugecols <- list(
    `No Data` = 'lightblue', 
    `Green` = '#00ff00',
    `Yellow` = '#ffff00', 
    `Orange` = 'orange', 
    `Red` = 'red'
  )
  col <- gaugecols[[filt]]
  
  # summary based on selection
  if(sumsel == 'by count'){
    
    maxv <- nrow(mapsel)
    togag <- mapsel %>% 
      filter(score == !!filt) %>% 
      nrow
    
  }
  
  if(sumsel == 'by creek length'){
    
    maxv <- sum(mapsel$`Length (km)`, na.rm = T)
    togag <- mapsel %>% 
      filter(score == !!filt) %>% 
      pull(`Length (km)`) %>% 
      sum(na.rm = T)
    
  }
  
  if(typsel == '%'){
    val <- round(100 * togag / maxv, 1)
    maxv <- 100
    sym <- '%'
  }
  
  if(typsel == 'sum'){
    val <- round(togag, 0)
    maxv <- round(maxv, 0)

    if(sumsel == 'by creek length')
      sym <- 'km'
    if(sumsel == 'by count')
      sym <- ''
  }
  
  out <- gauge(val, min = 0, max = maxv, symbol = sym, gaugeSectors(colors = col), label = filt)
  
  return(out)
  
}