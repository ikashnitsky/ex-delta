#===============================================================================
# 2021-03-01 -- ex delta
# Test on HMD data the validity of life expectancy prediction from the change
# in raw death counts for the total population
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr)
library(fs); library(here); library(glue)
library(hrbrthemes)

# functions to read local HMD directories
devtools::source_gist("0f93062f2b67eeac69949554027fa84f")

# the local HMD path (Ilya)
hmdpath <- fs::as_fs_path("~/data/hmd/")

# # deaths
# d1x1 <- path(hmdpath, "deaths", "Deaths_1x1") %>% fread_hmd_dir() %>%
#     rename(b = total, f = female, m = male)
#
# save(d1x1, file = "dat/d1x1.rda", compress = "xz")
#
# # life expectancy
# lt1x1 <- bind_rows(
#     b = path(hmdpath, "lt_both", "bltper_1x1") %>% fread_hmd_dir(),
#     f = path(hmdpath, "lt_female", "fltper_1x1") %>% fread_hmd_dir(),
#     m = path(hmdpath, "lt_male", "mltper_1x1") %>% fread_hmd_dir(),
#     .id = "sex"
# )
#
# save(lt1x1, file = "dat/lt1x1.rda", compress = "xz")

# load back
load("dat/d1x1.rda")
load("dat/lt1x1.rda")

d_all <- d1x1 %>%
    pivot_longer(f:b, names_to = "sex") %>%
    group_by(country, year, sex) %>%
    summarise(death = value %>% sum)

d_30 <- d1x1 %>%
    filter(age %>% is_weakly_greater_than(30)) %>%
    pivot_longer(f:b, names_to = "sex") %>%
    group_by(country, year, sex) %>%
    summarise(death = value %>% sum)



e0 <- lt1x1 %>%
    filter(age == 0) %>%
    select(country, year, sex, ex)

e30 <- lt1x1 %>%
    filter(age == 30) %>%
    select(country, year, sex, ex)


age0 <- left_join(d_all, e0) %>%
    group_by(country, sex) %>%
    mutate(
        death_change = death %>% subtract(lag(death)),
        rel_death_change = death_change %>% divide_by(lag(death)),
        ex_change = ex %>% subtract(lag(ex)),
        death_ratio = death %>% divide_by(lag(death)),
        ex_ratio = ex %>% divide_by(lag(ex))
    ) %>%
    ungroup()

age30 <- left_join(d_30, e30) %>%
    group_by(country, sex) %>%
    mutate(
        death_change = death %>% subtract(lag(death)),
        rel_death_change = death_change %>% divide_by(lag(death)),
        ex_change = ex %>% subtract(lag(ex)),
        death_ratio = death %>% divide_by(lag(death)),
        ex_ratio = ex %>% divide_by(lag(ex))
    ) %>%
    ungroup()


# func to predict ex ------------------------------------------------------

# xcountry = "ITA"; xsex = "f";  xyear = 2014; n_years = 20

ex_proxy <- function(
    xcountry, xsex, xyear, n_years = 20,
    log_trans = TRUE, rel_diff = FALSE
) {

    di <- age0 %>% filter(country == xcountry, sex == xsex)
    out_na <- di %>% filter(year == xyear) %>%
        mutate(
            predicted_ex_change = NA,
            model_r_squared = NA,
            model_intercept = NA,
            model_intercept_se = NA,
            model_beta = NA,
            model_beta_se = NA,
            abs_ex_change = NA,
            abs_predicted_ex_change = NA,
            prediction_error = NA,
            prediction_error_pct = NA
        )

    if (min(di$year) > (xyear-n_years-1) |
        min(di$year) == xyear) {

        return(out_na)

    } else {

        di <- di %>% filter(year %in% {xyear-n_years}:{xyear-1})

        # specify variants of the model
        if (isTRUE(log_trans) & !isTRUE(rel_diff)) {
            delta_fit <- lm(log(ex_ratio)~log(death_ratio), di)
        } else {
            delta_fit <- lm(ex_ratio~death_ratio, di)
        }

        if (isTRUE(rel_diff)){
            delta_fit <- lm(ex_change~rel_death_change, di)
        }


        out <- out_na %>%
            mutate(
                predicted_ex_change = delta_fit %>% predict(out_na) %>% unname,
                model_r_squared = delta_fit %>% summary %>%
                    extract2("r.squared"),
                model_intercept = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(1, 1),
                model_intercept_se = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(1, 2),
                model_beta = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(2, 1),
                model_beta_se = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(2, 2)
            ) %>%
            mutate(
                abs_ex_change = (ex * ex_change) %>% subtract(ex),
                abs_predicted_ex_change = (ex * predicted_ex_change) %>%
                    subtract(ex),
                prediction_error = abs_predicted_ex_change - abs_ex_change,
                prediction_error_pct = prediction_error/abs_ex_change * 100
            )

    return(out)

    }

}

# age 0 -------------------------------------------------------------------

coo <- crossing(xcountry = age0$country %>% unique(), xyear = 1990:2019)



boo <- pmap_df(
        .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
        .f = ex_proxy,
        xsex = "b", n_years = 20
    )

write_rds(boo, path = "out/predicted-1990-2019-20y-both.rds")

# foo <- 1960:2014 %>%
#     map_df(.f = ex_proxy, xcountry = "JPN", xsex = "f", n_years = 20)
#
# foo %>%
#     # filter(year %>% is_weakly_greater_than(1970)) %>%
    # ggplot(aes(year, ex))+
    # geom_line()+
    # geom_point(aes(y = ex + abs_predicted_ex_change), color = 2, shape = 1)


# plot model fit
boo %>%
    ggplot(aes(year, model_r_squared))+
    geom_hline(yintercept = .9, color = 5, size = .1)+
    geom_line(lineend = "round")+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())


ggsave("fig/r-squared.pdf", width = 10, height = 7, device = cairo_pdf)

# plot predicted values
boo %>%
    ggplot(aes(year, ex))+
    geom_line(lineend = "round")+
    geom_point(aes(y = ex + abs_predicted_ex_change), color = 2, shape = 46)+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/ex-plus-fitted.pdf", width = 10, height = 7, device = cairo_pdf)


# plot real change VS predicted
boo %>%
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

ggsave("fig/fitted-vs-real.pdf", width = 10, height = 7, device = cairo_pdf)


# # absolute prediction error
# boo %>%
#     filter(!country %in% c("NZL_MA")) %>%
#     ggplot(aes(year))+
#     geom_hline(yintercept = 0, color = 5, size = .1)+
#     geom_segment(aes(xend = year, y = prediction_error, yend = 0),
#                  color = 2, size = .1)+
#     geom_point(aes(y = prediction_error), color = 2, shape = 46)+
#     facet_wrap(~country, ncol = 10)+
#     scale_x_continuous(breaks = c(1990, 2000, 2010),
#                        labels = c("'90", "2000", "'10"))+
#     theme_minimal(base_family = font_rc)+
#     theme(panel.grid.minor = element_blank())
#
# ggsave("~/Downloads/absolute-errors.png")

# absolute prediction error
boo %>%
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

ggsave("fig/absolute-errors-vs-real.pdf", width = 10, height = 7, device = cairo_pdf)

# UPD  2021-02-04 ------------------------------
# additiional plots upon Jim's request


# prediction error vs abs_ex_change
boo %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(y = abs_ex_change, x = prediction_error))+
    geom_hline(yintercept = 0, color = 5, size = .1)+
    geom_vline(xintercept = 0, color = 5, size = .1)+
    stat_smooth(method = "lm", se = F, size = .5, color = 2)+
    geom_point(shape = 45)+
    facet_wrap(~country, ncol = 10)+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/errors-vs-ex-change.pdf", width = 10, height = 7, device = cairo_pdf)

# prediction error vs death_change
boo %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(y = death_change, x = prediction_error))+
    geom_hline(yintercept = 1, color = 5, size = .1)+
    geom_vline(xintercept = 0, color = 5, size = .1)+
    stat_smooth(method = "lm", se = F, size = .5, color = 2)+
    geom_point(shape = 45)+
    facet_wrap(~country, ncol = 10)+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/errors-vs-death-change.pdf", width = 10, height = 7, device = cairo_pdf)

# death_change vs abs_ex_change
boo %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(y = abs_ex_change, x = death_change))+
    geom_hline(yintercept = 0, color = 5, size = .1)+
    geom_vline(xintercept = 1, color = 5, size = .1)+
    stat_smooth(method = "lm", se = F, size = .5, color = 2)+
    geom_point(shape = 1, size = .5)+
    facet_wrap(~country, ncol = 10)+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/death-change-vs-ex-change.pdf", width = 10, height = 7, device = cairo_pdf)


# UPD  2021-02-05 ------------------------------
# all the same plots for ages 30+



e30_proxy <- function(xcountry, xsex, xyear, n_years = 20) {

    di <- age30 %>% filter(country == xcountry, sex == xsex)
    out_na <- di %>% filter(year == xyear) %>%
        mutate(
            predicted_ex_change = NA,
            model_r_squared = NA,
            model_intercept = NA,
            model_intercept_se = NA,
            model_beta = NA,
            model_beta_se = NA,
            abs_ex_change = NA,
            abs_predicted_ex_change = NA,
            prediction_error = NA,
            prediction_error_pct = NA
        )

    if (min(di$year) > (xyear-n_years-1) |
        min(di$year) == xyear) {

        return(out_na)

    } else {

        di <- di %>% filter(year %in% {xyear-n_years}:{xyear-1})

        delta_fit <- lm(ex_change~death_change, di)

        out <- out_na %>%
            mutate(
                predicted_ex_change = delta_fit %>% predict(out_na) %>% unname,
                model_r_squared = delta_fit %>% summary %>%
                    extract2("r.squared"),
                model_intercept = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(1, 1),
                model_intercept_se = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(1, 2),
                model_beta = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(2, 1),
                model_beta_se = delta_fit %>% summary %>%
                    extract2("coefficients") %>% extract(2, 2)
            ) %>%
            mutate(
                abs_ex_change = (ex * ex_change) %>% subtract(ex),
                abs_predicted_ex_change = (ex * predicted_ex_change) %>%
                    subtract(ex),
                prediction_error = abs_predicted_ex_change - abs_ex_change,
                prediction_error_pct = prediction_error/abs_ex_change * 100
            )

        return(out)

    }

}

# age 30 -------------------------------------------------------------------

coo30 <- crossing(xcountry = age30$country %>% unique(), xyear = 1990:2019)



boo30 <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = e30_proxy,
    xsex = "b", n_years = 20
)

write_rds(boo30, path = "out/predicted-1990-2019-20y-both-e30.rds")

# foo <- 1960:2014 %>%
#     map_df(.f = ex_proxy, xcountry = "JPN", xsex = "f", n_years = 20)
#
# foo %>%
#     # filter(year %>% is_weakly_greater_than(1970)) %>%
# ggplot(aes(year, ex))+
# geom_line()+
# geom_point(aes(y = ex + abs_predicted_ex_change), color = 2, shape = 1)


# plot model fit
boo30 %>%
    ggplot(aes(year, model_r_squared))+
    geom_hline(yintercept = .9, color = 5, size = .1)+
    geom_line(lineend = "round")+
    geom_line(data = boo, color = 2, lineend = "round")+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())


ggsave("fig/r-squared-0-vs-30.pdf", width = 10, height = 7, device = cairo_pdf)

# plot predicted values
boo30 %>%
    ggplot(aes(year, ex))+
    geom_line(lineend = "round")+
    geom_point(aes(y = ex + abs_predicted_ex_change), color = 2, shape = 46)+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/30-ex-plus-fitted.pdf", width = 10, height = 7, device = cairo_pdf)


# plot real change VS predicted
boo30 %>%
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

ggsave("fig/30-fitted-vs-real.pdf", width = 10, height = 7, device = cairo_pdf)


# # absolute prediction error
# boo %>%
#     filter(!country %in% c("NZL_MA")) %>%
#     ggplot(aes(year))+
#     geom_hline(yintercept = 0, color = 5, size = .1)+
#     geom_segment(aes(xend = year, y = prediction_error, yend = 0),
#                  color = 2, size = .1)+
#     geom_point(aes(y = prediction_error), color = 2, shape = 46)+
#     facet_wrap(~country, ncol = 10)+
#     scale_x_continuous(breaks = c(1990, 2000, 2010),
#                        labels = c("'90", "2000", "'10"))+
#     theme_minimal(base_family = font_rc)+
#     theme(panel.grid.minor = element_blank())
#
# ggsave("~/Downloads/absolute-errors.png")

# absolute prediction error
boo30 %>%
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

ggsave("fig/30-absolute-errors-vs-real.pdf", width = 10, height = 7, device = cairo_pdf)

# prediction error vs abs_ex_change
boo30 %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(y = abs_ex_change, x = prediction_error))+
    geom_hline(yintercept = 0, color = 5, size = .1)+
    geom_vline(xintercept = 0, color = 5, size = .1)+
    stat_smooth(method = "lm", se = F, size = .5, color = 2)+
    geom_point(shape = 45)+
    facet_wrap(~country, ncol = 10)+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/30-errors-vs-ex-change.pdf", width = 10, height = 7, device = cairo_pdf)

# prediction error vs death_change
boo30 %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(y = death_change, x = prediction_error))+
    geom_hline(yintercept = 1, color = 5, size = .1)+
    geom_vline(xintercept = 0, color = 5, size = .1)+
    stat_smooth(method = "lm", se = F, size = .5, color = 2)+
    geom_point(shape = 45)+
    facet_wrap(~country, ncol = 10)+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/30-errors-vs-death-change.pdf", width = 10, height = 7, device = cairo_pdf)

# death_change vs abs_ex_change
boo30 %>%
    filter(!country %in% c("NZL_MA")) %>%
    ggplot(aes(y = abs_ex_change, x = death_change))+
    geom_hline(yintercept = 0, color = 5, size = .1)+
    geom_vline(xintercept = 1, color = 5, size = .1)+
    stat_smooth(method = "lm", se = F, size = .5, color = 2)+
    geom_point(shape = 46, size = 2)+
    facet_wrap(~country, ncol = 10)+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/30-death-change-vs-ex-change.pdf", width = 10, height = 7, device = cairo_pdf)

# UPD  2021-02-17 ------------------------------


# test the relative death difference model suggested by Jim ---------------

roo <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = ex_proxy,
    xsex = "b", n_years = 20, rel_diff = TRUE
)


# compare R squared
boo %>%
    filter(!country %in% c("HRV", "KOR")) %>%
    ggplot(aes(year, model_r_squared))+
    geom_hline(yintercept = .9, color = 5, size = .1)+
    geom_line(lineend = "round", size = 2, alpha = .2)+
    geom_line(lineend = "round", size = .2)+
    geom_line(data = roo %>%
                  filter(!country %in% c("HRV", "KOR")),
              color = 2, size = .5, lineend = "round")+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("fig/r-squared-log-ratio-vs-rel_diff.pdf", width = 10, height = 7, device = cairo_pdf)
