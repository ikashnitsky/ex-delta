#===============================================================================
# 2021-09-21 -- ex delta
# Export plots for EAPS Mort presentation
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source("src/00-perp-session.R" %>% lp)



# set ggplot theme preferences
theme_set(
    dark_theme_bw(base_family = font_rc, base_size = 20)+
        theme(
            plot.title = element_text(family = "Roboto Slab", size = 24),
            axis.ticks = element_line(color = "grey", size = .5),
            axis.ticks.length = unit(.5, "line"),
            # panel.grid = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.y = element_text(vjust = 1),
            panel.border = element_rect(color = "grey90", fill = NA, size = .5),
            line = element_line(lineend = "round")
        )
)

# load("tmp/out/est_e0-1990-2019-20y-both.rda" %>% lp)

load("out/hmd_0.rda" %>% lp)

# crossing of the countries and years
coo <- crossing(xcountry = hmd_0$country %>% unique(), xyear = 1990:2019)

# load the function to proxy ex
source("src/03-fun-proxy-e0.R" %>% lp)

# estimate e0
est_e0_m <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = e0_proxy,
    df = hmd_0, xsex = "m", n_years = 20
)



# Spain life expectancy
est_e0_m %>%
    filter(country == "ESP") %>%
    ggplot(aes(year, ex))+
    geom_point(color = 7)+
    stat_smooth(se = F, size = 1, alpha = .5, color = 7)+
    labs(
        x = NULL, y = "Life expectancy at birth",
        title = "Life expectancy"
    )

ggsave("tmp/fig-eaps/spain-e0.pdf", width = 5, height = 4, device = cairo_pdf)

# deaths in Spain
d1x1 %>%
    filter(country == "ESP", year > 1989) %>%
    group_by(year) %>%
    summarise(dcount = m %>% sum) %>%
    ggplot(aes(year, dcount))+
    geom_point(color = 5)+
    stat_smooth(se = F, size = 1, alpha = .5, color = 5)+
    scale_y_comma(position = "right")+
    labs(
        x = NULL, y = "Total deaths",
        title = "Total death counts"
    )

ggsave("tmp/fig-eaps/spain-deaths.pdf", width = 5, height = 4, device = cairo_pdf)



# Spain life expectancy
est_e0_m %>%
    filter(country == "ESP") %>%
    ggplot(aes(death_ratio, ex_ratio))+
    geom_hline(yintercept = 1, size = 1, alpha = .5)+
    geom_vline(xintercept = 1, size = 1, alpha = .5)+
    coord_cartesian(ylim = c(.995, 1.01))+
    geom_point(color = 2)+
    stat_smooth(se = F, method = "lm", size = 1, alpha = .5, color = 7)+
    labs(
        x = "Change in Deaths count", y = "Change in Life expectancy",
        title = "Linear fit"
    )

ggsave("tmp/fig-eaps/spain-proxy.pdf", width = 5, height = 4, device = cairo_pdf)


# ex2020
# 29 countries before 2020
est_e0_m %>%
    filter(country %in% cntr) %>%
    mutate(country = country %>% as_factor()) %>%
    droplevels() %>%
    ggplot(aes(death_ratio, ex_ratio, color = year))+
    geom_hline(yintercept = 1, size = .4, color = "#bababa")+
    geom_vline(xintercept = 1, size = .4, color = "#bababa")+
    coord_cartesian(xlim = c(.9, 1.2), ylim = c(.95, 1.05))+
    geom_point(color = 2)+
    stat_smooth(se = F, method = "lm", size = 3/4, alpha = .5, color = 7)+
    scale_colour_viridis_b(
        option = "mako", direction = -1, end = .9,
        breaks = c(2005, 2010, 2015, 2019)
    )+
    facet_wrap(~country, ncol = 5)+
    labs(
        x = "Change in Deaths count", y = "Change in Life expectancy"
    )+
    dark_theme_minimal(base_family = font_rc)

ggsave("tmp/fig-eaps/proxy-29.pdf", width = 8, height = 6, device = cairo_pdf)

# 29 countries before 2020
est_e0_m %>%
    filter(country %in% cntr) %>%
    mutate(country = country %>% as_factor()) %>%
    droplevels() %>%
    ggplot(aes(death_ratio, ex_ratio, color = year))+
    geom_hline(yintercept = 1, size = 1, color = "#bababa")+
    geom_vline(xintercept = 1, size = 1, color = "#bababa")+
    geom_point(color = 2)+
    stat_smooth(se = F, method = "lm", size = 3/4, alpha = .5, color = 7, fullrange = T)+
    geom_point(
        data = dfp20hmd %>%
            filter(sex == "Male") %>%
            transmute(
                country = code_hmd,
                year,
                death_ratio = death_change,
                ex_ratio = e0_change
            ),
        color = 5, size = 2
    )+
    coord_cartesian(xlim = c(.9, 1.2), ylim = c(.95, 1.05))+
    scale_colour_viridis_b(
        option = "mako", direction = -1, end = .9,
        breaks = c(2005, 2010, 2015, 2019)
    )+
    facet_wrap(~country, ncol = 5)+
    labs(
        x = "Change in Deaths count", y = "Change in Life expectancy"
    )+
    dark_theme_minimal(base_family = font_rc)

ggsave("tmp/fig-eaps/proxy-29-2020.pdf", width = 8, height = 6, device = cairo_pdf)



# deaths age profile in Spain
d1x1 %>%
    filter(country == "ESP", year > 1989) %>%
    ungroup() %>%
    ggplot(aes(age, m, color = year, group = year))+
    # geom_point(alpha = .2)+
    stat_smooth(se = F, span = .1)+
    scale_colour_viridis_c(
        option = "mako", direction = -1, begin = .2,
        guide = guide_colorbar(barwidth = 15)
    )+
    # scale_y_continuous(trans = "log", breaks = c(.8, 1, 1.25))+
    # coord_cartesian(ylim = c(.5, 2))+
    labs(
        x = "Age", y = "Death counts",
        title = "Spain", color = NULL
    )+
    theme(legend.position = "top")

ggsave("tmp/fig-eaps/spain-age-prof-deaths.pdf", width = 7, height = 5, device = cairo_pdf)


# deaths age profile in Spain -- ratios
d1x1 %>%
    filter(country == "ESP", year > 1989) %>%
    group_by(age) %>%
    mutate(
        d_ratio = m %>% divide_by(lag(m))
    ) %>%
    ungroup() %>%
    ggplot(aes(age, d_ratio, color = year, group = year))+
    geom_point(alpha = .2)+
    stat_smooth(se = F)+
    scale_colour_viridis_c(
        option = "mako", direction = -1, begin = .2,
        guide = guide_colorbar(barwidth = 15)
    )+
    scale_y_continuous(trans = "log", breaks = c(.8, 1, 1.25))+
    coord_cartesian(ylim = c(.5, 2))+
    labs(
        x = "Age", y = "Death counts ratio",
        title = "Spain", color = NULL
    )+
    theme(legend.position = "top")

ggsave("tmp/fig-eaps/spain-age-prof-deaths-ratios.pdf", width = 7, height = 5, device = cairo_pdf)
