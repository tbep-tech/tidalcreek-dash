library(tbeptools)
library(here)

tidres <- anlz_tdlcrk(tidalcreeks, iwrraw)

save(tidres, file = here('data', 'tidres.RData'), compress = 'xz')
