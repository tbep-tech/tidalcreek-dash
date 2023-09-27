library(tbeptools)
library(tidyverse)
library(here)
library(sf)

# tidal creek results -----------------------------------------------------

tidres <- anlz_tdlcrk(tidalcreeks, iwrraw)

save(tidres, file = here::here('data', 'tidres.RData'), compress = 'xz')

# context indicator data --------------------------------------------------

# context data
cntdat <- anlz_tdlcrkindic(tidalcreeks, iwrraw) %>% 
  select(id, wbid, JEI, class, year, CHLAC, DO, TN, chla_tn_ratio, tsi, no23_source, no23_tidal, no23_ratio)

save(cntdat, file = here::here('data', 'cntdat.RData'), version = 2)

# context indicator data, radar plot --------------------------------------

# context data, radar plot
cntdatrdr <- anlz_tdlcrkindic(tidalcreeks, iwrraw, radar = T)

save(cntdatrdr, file = here::here('data', 'cntdatrdr.RData'), version = 2)

# # wbids ---------------------------------------------------------------------------------------
# 
# library(sf)
# library(mapview)
# 
# # run64 WBIDs
# wbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/WBIDS/MapServer/0/query?outFields=*&where=1%3D1&f=geojson')
# 
# wbid <- filter(wbid, WBID %in% unique(tidalcreeks$wbid))
