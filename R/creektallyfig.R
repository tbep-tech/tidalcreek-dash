# make a tile plot of results by year for marine wbids --------------------

library(patchwork)
library(tidyverse)

data(tidres)
toplo2 <- tidres %>% 
  filter(class %in% '3M') %>% 
  filter(!score %in% 'No Data') %>% 
  select(-id, -JEI, -class) %>% 
  mutate(
    name  = case_when(
      name == '' ~ 'no name', 
      T ~ name
    )
  ) %>% 
  unite('id', wbid, name, sep = ', ') %>% 
  mutate(
    score = factor(score, 
                   levels = rev(c('Act', 'Investigate', 'Caution', 'Target')), 
                   labels = rev(c('Prioritize', 'Investigate', 'Caution', 'Monitor'))
    )
  ) %>% 
  filter(!duplicated(id)) %>% 
  arrange(score, id) %>% 
  mutate(
    id = factor(id, levels = id)
  )

toplo1 <- toplo2 %>% 
  gather('indyr', 'count', target, caution, investigate, act) %>% 
  mutate(
    count = case_when(
      is.na(count) ~ 0L, 
      T ~ count
    ), 
    indyr = factor(indyr, levels = rev(c('act', 'investigate', 'caution', 'target')), labels = rev(c('Prioritize', 'Investigate', 'Caution', 'Monitor')))
  )

pthm <- theme(
  legend.position = 'top', 
  axis.text.y = element_text(size  = 6), 
  panel.background = element_blank(), 
  axis.text.x = element_text(size = 8)
)

p1 <- ggplot(toplo1, aes(y = id, x = indyr, fill = indyr, alpha = count)) + 
  scale_fill_manual(values = c('green', 'yellow', 'orange', 'coral'), guide = 'none') + 
  geom_tile(colour = NA) +
  scale_alpha_continuous('Years', range = c(0, 1), limits = c(0, 10), breaks = c(0, 5, 10)) +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) + 
  labs(x = 'Individual year results') + 
  pthm + 
  labs(y = 'Creek Id, name')

p2 <- ggplot(toplo2, aes(y = id, x = 'Final category', fill = score)) + 
  scale_fill_manual(values = c('green', 'yellow', 'orange', 'coral'), guide = guide_legend(reverse = T)) + 
  geom_tile(colour = 'black') +
  scale_x_discrete(expand = c(0,0)) + 
  scale_y_discrete(expand = c(0,0)) + 
  pthm + 
  theme(
    axis.text.y = element_blank(), 
    legend.title = element_blank(), 
    legend.position = 'right', 
    axis.title = element_blank()
  )

out <- p1 + p2 + plot_layout(ncol = 2, widths = c(1, 0.2))

jpeg('~/Desktop/tidalcreekreport.jpg', family = fml, height = 10, width = 6, units = 'in', res = 300)
print(out)
dev.off()
