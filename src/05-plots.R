#===============================================================================
# 2021-03-03 -- ex delta
# Plots
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source("src/00-perp-session.R" %>% lp)

# age 0 ------------------------------------------------------------------

load("out/proxy-1990-2019-20y-male.rda" %>% lp)

# plot model fit
df_e0 %>%
    ggplot(aes(year, model_r_squared))+
    geom_hline(yintercept = .9, color = 5, size = .25)+
    geom_line(lineend = "round")+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())


ggsave("fig/r-squared.pdf" %>% lp, width = 10, height = 7, device = cairo_pdf)

# plot predicted values
df_e0 %>%
    ggplot(aes(year, ex))+
    geom_line(lineend = "round")+
    geom_point(aes(y = ex + abs_predicted_ex_change), color = 2, shape = 46)+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/ex-plus-fitted.pdf" %>% lp, width = 10, height = 7, device = cairo_pdf)


# plot real change VS predicted
df_e0 %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(year))+
    geom_hline(yintercept = 0, color = 5, size = .1)+
    geom_line(aes(y = abs_ex_change), size = .1)+
    geom_point(aes(y = abs_ex_change), shape = 45)+
    geom_point(aes(y = abs_predicted_ex_change), color = 2, shape = 46)+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/fitted-vs-real.pdf" %>% lp, width = 10, height = 7, device = cairo_pdf)

# absolute prediction error
df_e0 %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(year))+
    geom_hline(yintercept = 0, color = 5, size = .1)+
    geom_line(aes(y = abs_ex_change), size = .1)+
    geom_point(aes(y = abs_ex_change), shape = 45)+
    geom_segment(aes(xend = year, y = prediction_error, yend = 0),
                 color = 2, size = .1)+
    geom_point(aes(y = prediction_error), color = 2, shape = 46)+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/absolute-errors-vs-real.pdf" %>% lp, width = 10, height = 7, device = cairo_pdf)
