# color function for reactable cells
colfun <- function(x){
  
  out <- case_when(
    x == 'No Data' ~ 'lightblue', 
    x == 'Monitor' ~ '#00ff00', 
    x == 'Caution' ~ 'yellow', 
    x == 'Investigate' ~ 'orange', 
    x == 'Prioritize' ~ 'coral'
  )
  
  return(out)
  
}

# color palette for leaflet
pal_exp <- leaflet::colorFactor(
  palette = c('lightblue', 'green', 'yellow', 'orange', 'coral'),
  levels = c('No Data', 'Monitor', 'Caution', 'Investigate', 'Prioritize')
)

# summary function for gauges
sumfun <- function(mapsel, sumsel, typsel, filt){
  
  # select color
  gaugecols <- list(
    `No Data` = 'lightblue', 
    `Monitor` = '#00ff00',
    `Caution` = '#ffff00', 
    `Investigate` = 'orange', 
    `Prioritize` = 'coral'
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