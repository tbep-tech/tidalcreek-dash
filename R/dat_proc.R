library(tbeptools)
library(tidyverse)
library(here)

# tidal creek results -----------------------------------------------------

tidres <- anlz_tdlcrk(tidalcreeks, iwrraw)

save(tidres, file = here::here('data', 'tidres.RData'), compress = 'xz')

# watersheds for spatial filter -------------------------------------------

# anep boundaries
load(file = '../anep-congressional/data/anep.RData')

# tbep and sbep
bnds <- anep %>% 
  filter(nep %in% c('Sarasota Bay Estuary Program', 'Tampa Bay Estuary Program', 'Charlotte Harbor National Estuary Program'))

save(bnds, file = here::here('data', 'bnds.RData'), compress = 'xz')
