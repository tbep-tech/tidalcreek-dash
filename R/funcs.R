# color function for reactable cells
colfun <- function(x){
  
  out <- case_when(
    x == 'No Data' ~ 'lightblue', 
    x == 'Target' ~ '#00ff00', 
    x == 'Caution' ~ 'yellow', 
    x == 'Investigate' ~ 'orange', 
    x == 'Act' ~ 'coral'
  )
  
  return(out)
  
}

# color palette for leaflet
pal_exp <- leaflet::colorFactor(
  palette = c('lightblue', 'green', 'yellow', 'orange', 'coral'),
  levels = c('No Data', 'Target', 'Caution', 'Investigate', 'Act')
)

# summary function for gauges
sumfun <- function(mapsel, sumsel, typsel, filt){
  
  # select color
  gaugecols <- list(
    `No Data` = 'lightblue', 
    `Target` = '#00ff00',
    `Caution` = '#ffff00', 
    `Investigate` = 'orange', 
    `Act` = 'coral'
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

# bar plots of tidal creek context indicators
show_tdlcrkindic <- function(selcrk, cntdat, yr){
  
  # data to plot
  toplo <- cntdat %>% 
    filter(id %in% selcrk) %>% 
    mutate(year = factor(year, levels = seq(yr - 10, yr - 1))) %>% 
    tidyr::complete(id, wbid, JEI, class, year, fill = list(CHLAC = 0, DO = 0, TN = 0, chla_tn_ratio = 0, tsi = 0))

  if(nrow(toplo) == 0)
    return()

  p1 <- plot_ly(toplo, x = ~year, y = ~CHLAC, type = 'bar', text = ~round(CHLAC, 1), textposition = 'auto',
                marker = list(color = '#00806E')
                ) %>% 
    layout(
      yaxis = list(title = 'Chla (ug/L)'), 
      xaxis = list(title = ''), 
      showlegend = F
    )
  
  p2 <- plot_ly(toplo, x = ~year, y = ~TN, type = 'bar', text = ~round(TN, 1), textposition = 'auto', 
                marker = list(color = '#00806E')
                ) %>%  
    layout(
      yaxis = list(title = 'TN (mg/L)'), 
      xaxis = list(title = ''), 
      showlegend = F
    )
  
  p3 <- plot_ly(toplo, x = ~year, y = ~chla_tn_ratio, type = 'bar', text = ~round(chla_tn_ratio, 1), textposition = 'auto', 
                marker = list(color = '#00806E')
                ) %>% 
    layout(
      yaxis = list(title = 'Chla:TN'), 
      xaxis = list(title = ''), 
      showlegend = F
    )
  
  p4 <- plot_ly(toplo, x = ~year, y = ~DO, type = 'bar', text = ~round(DO, 1), textposition = 'auto', 
                marker = list(color = '#00806E')
                ) %>%  
    layout(
      yaxis = list(title = 'DO (mg/L)'), 
      xaxis = list(title = ''), 
      showlegend = F
    )
  
  p5 <- plot_ly(toplo, x = ~year, y = ~tsi, type = 'bar', text = ~round(tsi, 0), textposition = 'auto', 
                marker = list(color = '#00806E')
                ) %>%  
    layout(
      yaxis = list(title = 'Florida TSI'), 
      xaxis = list(title = ''), 
      showlegend = F
    )
  
  out <- subplot(p1, p2, p3, p4, p5, shareX = T, titleY = T, nrows = 5)
  
  return(out)
  
  }

