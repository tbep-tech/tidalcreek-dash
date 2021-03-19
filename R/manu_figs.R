library(tbeptools)
library(tidyverse)
library(extrafont)
library(patchwork)
library(ggmap)
library(sf)

tidcrk <- tidalcreeks
dat <- anlz_tdlcrk(tidcrk, iwrraw, yr = 2018)

class <-  c('3M', '2')
score <-  c('Prioritize', 'Investigate', 'Caution', 'Monitor')
family <- NA
cols <- list(Monitor = 'green', Caution = 'yellow', Investigate = 'orange', Prioritize = 'coral')

# overall score categories
toplo2 <- dat %>%
  dplyr::filter(class %in% !!class) %>%
  dplyr::filter(score %in% !!score) %>%
  dplyr::select(-id, -JEI, -class) %>%
  dplyr::mutate(
    name  = dplyr::case_when(
      name == '' ~ 'no name',
      T ~ name
    )
  ) %>%
  tidyr::unite('id', wbid, name, sep = ', ', remove = F) %>%
  dplyr::mutate(
    score = factor(score, levels =  rev(c('Prioritize', 'Investigate', 'Caution', 'Monitor')))
  ) %>%
  dplyr::filter(!duplicated(id)) %>%
  dplyr::arrange(score, id) %>%
  dplyr::mutate(
    id = factor(id, levels = id)
  ) %>% 
  .[40:nrow(.), ]

# individual year counts
toplo1 <- toplo2 %>%
  tidyr::gather('indyr', 'count', monitor, caution, investigate, prioritize) %>%
  dplyr::mutate(
    count = dplyr::case_when(
      is.na(count) ~ 0L,
      T ~ count
    ),
    indyr = factor(indyr, levels = rev(c('prioritize', 'investigate', 'caution', 'monitor')), labels = rev(c('Prioritize', 'Investigate', 'Caution', 'Monitor')))
  )

# theme
pthm <- ggplot2::theme(
  legend.position = 'top',
  axis.text.y = ggplot2::element_text(size  = 7, family = family),
  panel.background = ggplot2::element_blank(),
  axis.text.x = ggplot2::element_text(size = 9, family = family),
  text = ggplot2::element_text(family = family)
)

# plot for individual year counts
p1 <- ggplot2::ggplot(toplo1, ggplot2::aes(y = id, x = indyr, fill = indyr, alpha = count)) +
  ggplot2::scale_fill_manual(values = cols, guide = 'none') +
  ggplot2::geom_tile(colour = NA) +
  ggplot2::scale_alpha_continuous('Years', range = c(0, 1), limits = c(0, 10), breaks = c(0, 5, 10)) +
  ggplot2::scale_x_discrete(expand = c(0,0)) +
  ggplot2::scale_y_discrete(expand = c(0,0)) +
  ggplot2::labs(
    x = 'Individual year results',
    y = 'Creek Id, name'
  ) +
  pthm

# plot for overall score categories
p2 <- ggplot2::ggplot(toplo2, ggplot2::aes(y = id, x = 'Final category', fill = score)) +
  ggplot2::scale_fill_manual(values = cols, guide = ggplot2::guide_legend(reverse = T)) +
  ggplot2::geom_tile(colour = 'black') +
  ggplot2::scale_x_discrete(expand = c(0,0)) +
  ggplot2::scale_y_discrete(expand = c(0,0)) +
  pthm +
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = 'right',
    axis.title = ggplot2::element_blank()
  )

# combine
out <- p1 + p2 + plot_layout(ncol = 2, widths = c(1, 0.2))

tiff('~/Desktop/fig8.tif', height = 6, width = 6.5, units = 'in', res = 300, compression = 'lzw')
print(out)
dev.off()
