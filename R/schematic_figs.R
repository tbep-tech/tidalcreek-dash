library(tidyverse)
library(tbeptools)
library(extrafont)

loadfonts(device = 'win', quiet = T)
fml <- "Lato Light"
# data(iwrraw)

yr <- 2018
jei <- 'HC17'
ylms1 <- c(0.45, 5.9)
ylms2 <- c(0, 3.25)
fntsz <- 14

rctcol <- c("#2DC938", "#E9C318", "#EE7600", "#FF4040")
alph <- 1

crklen <- tidalcreeks %>%
  filter(JEI %in% !!jei) %>%
  pull(Creek_Length_m) %>%
  unique

prilin <- tidaltargets %>%
  filter(region %in% 'West Central') %>%
  pull(prioritize)
invlin <- tidaltargets %>%
  filter(region %in% 'West Central') %>%
  pull(investigate)
caulin <- tidaltargets %>%
  filter(region %in% 'West Central') %>%
  pull(caution)
tmp <- function(Creek_Length_m) eval(parse(text = caulin))
caulin <- tmp(crklen)

dat <- iwrraw %>%
  dplyr::filter(JEI %in% !!jei) %>%
  dplyr::filter(year > yr - 11) %>%
  dplyr::filter(masterCode %in% 'TN') %>%
  dplyr::filter(!is.na(result) & result > 0) %>%
  tidyr::unite('date', month, day, year, remove = F, sep = '-') %>%
  dplyr::select(wbid, class, JEI, year, date, masterCode, result) %>%
  dplyr::mutate(
    masterCode = dplyr::case_when(
      masterCode %in% 'TN_MG' ~ 'TN',
      masterCode %in% 'TSS_M' ~ 'TSS',
      T ~ masterCode
    ),
    date = as.Date(date, format = '%m-%d-%Y')
  )

datagm <- dat %>%
  mutate(
    result =log(result)
  ) %>%
  group_by(wbid, year) %>%
  summarise(result = mean(result, na.rm = T)) %>%
  ungroup %>%
  mutate(result = exp(result))


# prioritize example
toplo1a <- dat %>%
  filter(wbid %in% '1605D')
toplo1b <- datagm %>%
  filter(wbid %in% '1605D')

p1a <- ggplot(toplo1a, aes(x = date, y = result)) +
  geom_line() +
  geom_point() +
  # scale_y_log10() +
  labs(
    y = 'Total nitrogen (mg/L)'
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    text = element_text(family = fml, size = fntsz),
    panel.grid = element_blank()
    )

p1b <- ggplot(toplo1b, aes(x = factor(year), y = result)) +
  # geom_bar(stat = 'identity') +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf,  ymax = caulin, alpha = alph, fill = rctcol[1]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = caulin, ymax = invlin,  alpha = alph, fill = rctcol[2]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = invlin,  ymax = prilin, alpha = alph, fill = rctcol[3]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = prilin, ymax = Inf,  alpha = alph, fill = rctcol[4]) +
  geom_bar(stat = 'identity', alpha = 0.8) +
  coord_cartesian(ylim = ylms2) +
  labs(
    y = 'Total nitrogen (mg/L)',
    subtitle = 'Annual geometric means'
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = fml, size = fntsz),
    panel.grid = element_blank()
  )



# target example
toplo2a <- dat %>%
  filter(wbid %in% '1605')
toplo2b <- datagm %>%
  filter(wbid %in% '1605')

p2a <- ggplot(toplo2a, aes(x = date, y = result)) +
  geom_line() +
  geom_point() +
  scale_y_log10() +
  coord_cartesian(ylim = ylms1) +
  # labs(
  #   y = 'Total nitrogen (mg/L)',
  #   subtitle = 'Observed'
  # ) +
  theme_void() +
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = fml),
    panel.grid = element_blank()
  )

p2b <- ggplot(toplo2b, aes(x = factor(year), y = result)) +
  # geom_bar(stat = 'identity') +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf,  ymax = caulin, alpha = alph, fill = rctcol[1]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = caulin, ymax = invlin,  alpha = alph, fill = rctcol[2]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = invlin,  ymax = prilin, alpha = alph, fill = rctcol[3]) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = prilin, ymax = Inf,  alpha = alph, fill = rctcol[4]) +
  geom_bar(stat = 'identity', alpha = 0.9) +
  coord_cartesian(ylim = ylms2) +
  labs(
    y = 'Total nitrogen (mg/L)',
    subtitle = 'Annual geometric means'
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    text = element_text(family = fml, size = fntsz),
    panel.grid = element_blank()
  )

png('~/Desktop/TBEP/tidal creeks/tidaldiagram1.png', width = 5.5, height = 2.5, unit = 'in', family = 'Lato Light', res = 500)
p1a
dev.off()
png('~/Desktop/TBEP/tidal creeks/tidaldiagram2.png', width = 6, height = 2.5, unit = 'in', family = 'Lato Light', res = 500)
p1b
dev.off()
png('~/Desktop/TBEP/tidal creeks/tidaldiagram3.png', width = 3.5, height = 2.5, unit = 'in', family = 'Lato Light', res = 500)
p2a
dev.off()
png('~/Desktop/TBEP/tidal creeks/tidaldiagram4.png', width = 6, height = 2.5, unit = 'in', family = 'Lato Light', res = 500)
p2b
dev.off()
