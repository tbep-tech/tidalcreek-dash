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

# add horizontal or vertical line to plotly
# https://stackoverflow.com/questions/55170349/adding-a-horizontal-line-to-a-plotly-bar-graph
plyline <- function(var, thrsel = FALSE, horiz = T) {
  
  # thresholds
  thrs <- list(
    CHLAC = 11, 
    TN = 1.1, 
    chla_tn_ratio = 15, 
    DO = 2, 
    tsi = c(50, 60) # lake, estuary
  )
  
  # value to plot
  ln <- thrs[[var]]
  
  out <- NULL
  
  # create lines
  if(thrsel){
    
    out <- list()
    
    # horizontal
    if(horiz){
      for(i in seq_along(ln)){

        outi <- list(list(
          type = "line", 
          x0 = 0, 
          x1 = 1, 
          xref = "paper",
          y0 = ln[i], 
          y1 = ln[i], 
          line = list(color = 'red', dash = 10)
        ))
        
        out <- c(out, outi)
        
      }

    }
    
    # vertical
    if(!horiz){
      for(i in seq_along(ln)){
        
        outi <- list(list(
          type = "line", 
          x0 = ln[i], 
          x1 = ln[i], 
          yref = "paper",
          y0 = 0, 
          y1 = 1, 
          line = list(color = 'red', dash = 10)
        ))
        
        out <- c(out, outi)
        
      }
      
    }
    
  }
      
  return(out)

}

# add text annotations to horizontal or vertical line to plotly
annline <- function(varin, thrsel = FALSE, horiz = T) {
  
  out <- NULL
  
  # thresholds
  thrs <- list(
    CHLAC = 11, 
    TN = 1.1, 
    chla_tn_ratio = 15, 
    DO = 2, 
    tsi = c(50, 60) # lake, estuary
  )
  
  # annotations
  anns <- list(
    CHLAC = '', 
    TN = '', 
    chla_tn_ratio = '', 
    DO = '', 
    tsi = c('lake', 'estuary') # lake, estuary
  )
  
  # value to plot
  ln <- thrs[[varin]]
  ann <- anns[[varin]]
  
  # create lines
  if(thrsel){
    
    out <- list()

    # horizontal
    if(horiz){
      for(i in seq_along(ln)){
        
        outi <- list(list(
          x = 0,
          y = ln[i],
          text = ann[i],
          xref = "x",
          yref = "y",
          showarrow = F, 
          yanchor = 'top', 
          # xanchor = 'left',
          font = list(color = 'red', size = 14)
        ))
        
        out <- c(out, outi)
        
      }
      
    }
    
    # vertical
    if(!horiz){
      for(i in seq_along(ln)){
        
        outi <- list(list(
          x = ln[i],
          y = 1,
          text = ann[i],
          xref = "x",
          yref = "y",
          showarrow = F, 
          xanchor = 'right', 
          yanchor = 'top',
          textangle = 90,
          font = list(color = 'red', size = 14)
        ))
        
        out <- c(out, outi)
        
      }
      
    }
    
  }
  
  return(out)
  
}

# bar plots of tidal creek context indicators
show_tdlcrkindic <- function(selcrk, cntdat, yr, thrsel = FALSE){
  
  labs <- c('Chla (ug/L)', 'TN (mg/L)', 'Chla:TN', 'DO (mg/L)', 'Florida TSI')
  names(labs) <- c('CHLAC', 'TN', 'chla_tn_ratio', 'DO', 'tsi')
  
  pal_yrs <- leaflet::colorFactor(
    palette = c('#5C4A42', '#427355', '#004F7E'), #RColorBrewer::brewer.pal(8,  'Blues'),#c('#004F7E', '#00806E', '#427355', '#5C4A42', '#958984'),
    na.color = 'yellow',
    levels = as.character(seq(2008, 2017))
  )
  
  # data to plot
  toplo <- cntdat %>% 
    filter(id %in% selcrk) %>% 
    mutate(year = factor(year, levels = seq(yr - 10, yr - 1))) %>% 
    tidyr::complete(id, wbid, JEI, class, year, fill = list(CHLAC = 0, DO = 0, TN = 0, chla_tn_ratio = 0, tsi = 0)) %>% 
    mutate(color = pal_yrs(year))

  if(nrow(toplo) == 0)
    return()

  p1 <- plot_ly(toplo, x = ~year, y = ~CHLAC, type = 'bar', text = ~round(CHLAC, 1), textposition = 'auto',
                marker = list(color = ~color), hoverinfo = 'x'
                ) %>% 
    layout(
      yaxis = list(title = labs['CHLAC']), 
      xaxis = list(title = ''), 
      showlegend = F, 
      shapes = plyline('CHLAC', thrsel = thrsel), 
      annotations = annline('CHLAC', thrsel = thrsel)
    )
  
  p2 <- plot_ly(toplo, x = ~year, y = ~TN, type = 'bar', text = ~round(TN, 1), textposition = 'auto', 
                marker = list(color = ~color), hoverinfo = 'x'
                ) %>%  
    layout(
      yaxis = list(title = labs['TN']), 
      xaxis = list(title = ''), 
      showlegend = F, 
      shapes = plyline('TN', thrsel = thrsel),
      annotations = annline('TN', thrsel = thrsel)
    )
  
  p3 <- plot_ly(toplo, x = ~year, y = ~chla_tn_ratio, type = 'bar', text = ~round(chla_tn_ratio, 1), textposition = 'auto', 
                marker = list(color = ~color), hoverinfo = 'x'
                ) %>% 
    layout(
      yaxis = list(title = labs['chla_tn_ratio']), 
      xaxis = list(title = ''), 
      showlegend = F, 
      shapes = plyline('chla_tn_ratio', thrsel = thrsel),
      annotations = annline('chla_tn_ratio', thrsel = thrsel)
    )
  
  p4 <- plot_ly(toplo, x = ~year, y = ~DO, type = 'bar', text = ~round(DO, 1), textposition = 'auto', 
                marker = list(color = ~color), hoverinfo = 'x'
                ) %>%  
    layout(
      yaxis = list(title = labs['DO']), 
      xaxis = list(title = ''), 
      showlegend = F,
      shapes = plyline('DO', thrsel = thrsel),
      annotations = annline('DO', thrsel = thrsel)
    )
  
  p5 <- plot_ly(toplo, x = ~year, y = ~tsi, type = 'bar', text = ~round(tsi, 0), textposition = 'auto', 
                marker = list(color = ~color), hoverinfo = 'x' 
                ) %>%  
    layout(
      yaxis = list(title = labs['tsi']), 
      xaxis = list(title = ''), 
      showlegend = F, 
      shapes = plyline('tsi', thrsel = thrsel),
      annotations = annline('tsi', thrsel = thrsel)
    )
  
  out <- subplot(p1, p2, p3, p4, p5, shareX = T, titleY = T, nrows = 5)
  
  return(out)
  
  }

# ecdf plots of tidal creek context indicators
show_tdlcrkindiccdf <- function(selcrk, cntdat, yr, thrsel = FALSE){

  # data to plot
  seldat <- cntdat %>% 
    filter(id %in% selcrk) %>% 
    mutate(year = factor(year, levels = seq(yr - 10, yr - 1))) %>% 
    tidyr::complete(id, wbid, JEI, class, year) 

  if(nrow(seldat) == 0)
    return()
  
  toplo <- seldat %>%  
    gather('var', 'val', -id, -wbid, -JEI, -class, -year) %>% 
    group_by(id, wbid, JEI, class, var) %>% 
    nest() %>% 
    mutate(
      cntdat = list(cntdat),
      plo = purrr::pmap(list(data, var, cntdat), function(data, var, cntdat){

        labs <- c('Chla (ug/L)', 'TN (mg/L)', 'Chla:TN', 'DO (mg/L)', 'Florida TSI')
        names(labs) <- c('CHLAC', 'TN', 'chla_tn_ratio', 'DO', 'tsi')
        
        pal_yrs <- leaflet::colorFactor(
          palette = c('#5C4A42', '#427355', '#004F7E'), #RColorBrewer::brewer.pal(8,  'Blues'),#c('#004F7E', '#00806E', '#427355', '#5C4A42', '#958984'),
          na.color = 'yellow',
          levels = as.character(seq(2008, 2017))
        )
        
        ecdfdat <- cntdat[, var]
        
        ecdffun <- ecdf(ecdfdat)
        plodat <- tibble(
          val = seq(min(ecdfdat, na.rm = T), max(ecdfdat, na.rm = T), length.out = 200),
          y = ecdffun(val)
        )
        
        ptdat <- data %>% 
          mutate(
            y = ecdffun(val), 
            color = as.character(factor(year, levels = year, labels = pal_yrs(year))),
            year = as.character(year)
          ) %>% 
          na.omit

        p <- plot_ly(type = 'scatter', colors = c(ptdat$color, 'black')) %>% 
          add_trace(data = plodat, x = ~val,y = ~y, type = 'scatter', mode = 'lines', color = 'test', showlegend = F, hoverinfo = 'y', inherit = F) %>%
          add_trace(data = ptdat, x = ~val, y = ~y, color = ~year, inherit = F, type = 'scatter', mode = 'markers', marker = list(size = 16, opacity = 0.8),
                    hoverinfo = 'text', text = ~year, showlegend = F) %>% 
          layout(
            yaxis = list(title = 'Percentiles', zeroline = T),
            xaxis = list(title = labs[var], zeroline = T),
            shapes = plyline(var, thrsel = thrsel, horiz = F),
            annotations = annline(var, thrsel = thrsel, horiz = F)
          )
        
        return(p)
       
      })
    )

  p1 <- toplo$plo[[1]]
  p2 <- toplo$plo[[2]]
  p3 <- toplo$plo[[3]]
  p4 <- toplo$plo[[4]]
  p5 <- toplo$plo[[5]]
  
  out <- subplot(p1, p2, p3, p4, p5, nrows = 2, shareY = T, titleX = T, margin = c(0.02, 0.02, 0.06, 0.06))
  
  return(out)
  
}

