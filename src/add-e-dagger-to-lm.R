#===============================================================================
# 2021-09-21 -- ex delta
# Test on HMD data the validity of life expectancy prediction from the change
# in raw death counts for the total population and e-dagger (add to the model)
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

library(tidyverse)
library(magrittr)
library(fs)
library(hrbrthemes)

load("dat/d1x1.rda")
load("dat/lt1x1.rda")


d_all <- d1x1 %>%
    pivot_longer(f:b, names_to = "sex") %>%
    group_by(country, year, sex) %>%
    summarise(death = value %>% sum)

e0 <- lt1x1 %>%
    filter(age == 0) %>%
    transmute(country, year, sex, ex)

edag <- lt1x1 %>%
    mutate(inter = ex * dx) %>%
    group_by(country, year, sex) %>%
    summarise(edag = inter %>% sum %>% divide_by(1e5)) %>%
    ungroup()

e0_edag <- left_join(d_all, e0) %>%
    left_join(edag) %>%
    group_by(country, sex) %>%
    mutate(
        death_ratio = death %>% divide_by(lag(death)),
        ex_ratio = ex %>% divide_by(lag(ex))
    ) %>%
    ungroup()

e0_edag %>%
    filter(sex == "b", country == "FRATNP", year %in% 1990:2019) %>%
    mutate_if(.predicate = is.numeric, .funs = round, 2) %>%
    view


# crossing of the countries and years
coo <- crossing(xcountry = df_e0_edag$country %>% unique(), xyear = 1990:2019)

# estimate e0 with edagger
est_e0_edag <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = e0_proxy_edag,
    df = e0_edag, xsex = "b", n_years = 20
)

save(est_e0_edag, file = "tmp/out/est_e0_edag-1990-2019-20y-both.rda" %>% lp)


# compare formally to the regression approach -----------------------------

# estimate e0 only from ratios
est_e0 <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = e0_proxy,
    df = e0_edag, xsex = "b", n_years = 20
)

save(est_e0, file = "tmp/out/est_e0-1990-2019-20y-both.rda" %>% lp)


df_comp <- left_join(
    est_e0 %>%
        transmute(
            country, year, abs_ex_change, abs_predicted_ex_change
        ),
    est_e0_edag %>%
        transmute(
            country, year,
            edag_abs_predicted_ex_change = abs_predicted_ex_change
        )
    )

df_comp %>%
    group_by(country) %>%
    summarise(
        avg_e0_change = abs_ex_change %>% mean,
        mae_e0 = (abs_predicted_ex_change - abs_ex_change) %>%
            abs %>% mean(na.rm = T),
        mae_edag = (edag_abs_predicted_ex_change - abs_ex_change) %>%
            abs %>% mean(na.rm = T),
        rmse_e0 = (abs_predicted_ex_change - abs_ex_change) %>%
            raise_to_power(2) %>% mean(na.rm = T) %>% sqrt,
        rmse_edag = (edag_abs_predicted_ex_change - abs_ex_change) %>%
            raise_to_power(2) %>% mean(na.rm = T) %>% sqrt
    ) %>%
    ungroup() %>%
    # select(-1) %>%
    # summarise_at(vars(2:5), mean, na.rm = T) %>%
    mutate_if(.predicate = is.numeric, .funs = round, 3) %>%
    write_csv(path = "~/Downloads/out-errors.csv")


# plot real change VS predicted
est_e0 %>%
    filter(!country %in% c("NZL_MA", "HRV", "KOR")) %>%
    ggplot(aes(year))+
    geom_hline(yintercept = 1, color = 6, size = .1)+
    geom_line(aes(y = ex_ratio), size = .1)+
    geom_point(aes(y = ex_ratio), shape = 45)+
    geom_point(aes(y = predicted_ex_ratio), color = 2, shape = 46)+
    geom_point(
        data = est_e0_edag %>% filter(!country %in% c("NZL_MA", "HRV", "KOR")),
        aes(y = predicted_ex_ratio), color = 4, shape = 46, nudge_x = .2
    )+
    facet_wrap(~country, ncol = 10)+
    scale_x_continuous(breaks = c(1990, 2000, 2010),
                       labels = c("'90", "2000", "'10"))+
    theme_minimal(base_family = font_rc)+
    theme(panel.grid.minor = element_blank())

ggsave("tmp/fig/compare-e0-edag.pdf", width = 10, height = 7, device = cairo_pdf)
