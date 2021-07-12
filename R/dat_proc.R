library(tbeptools)
library(tidyverse)
library(here)

# watersheds for spatial filter -------------------------------------------

# anep boundaries
load(file = '../anep-congressional/data/anep.RData')

# tbep and sbep
bnds <- anep %>% 
  filter(nep %in% c('Sarasota Bay Estuary Program', 'Tampa Bay Estuary Program', 'Charlotte Harbor National Estuary Program'))

save(bnds, file = here::here('data', 'bnds.RData'), compress = 'xz')

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

