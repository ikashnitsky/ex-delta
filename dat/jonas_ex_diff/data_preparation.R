# Changes in total life-expectancy 1950-2020

# Init ------------------------------------------------------------

library(here); setwd(here())
library(tidyverse); library(yaml)

source('dat/jonas_ex_diff/figure_specification.R')
config <- yaml.load_file('dat/jonas_ex_diff/config.yaml')

fig <- list()

# Input -----------------------------------------------------------

region_meta <- read_csv('src/region_metadata.csv')
pclm_deaths <- read_rds('dat/jonas_ex_diff/lt_input_85.rds')
hmd_lifetab <- read_rds('dat/jonas_ex_diff/lt-1x1.rds')
e0_diff_comparison <- read_csv('dat/jonas_ex_diff/comparison_aburto_islam_raksha.csv')

# hmd deaths

# list all files in archive
hmd_filenames <- unzip(
    'dat/jonas_ex_diff/death1x1.zip',
    list = TRUE
)[['Name']]

# bind all .csv files in stmf zip archive into single file
hmd_deaths <-
    hmd_filenames %>%
    map(~{
        unz('dat/jonas_ex_diff/death1x1.zip', filename = .x) %>%
            read_table(
                col_names = c('Year', 'Age', 'Female', 'Male', 'Total'),
                col_types = 'iiddd',
                skip = 3,
                na = '.'
            ) %>%
            mutate(Country = gsub('(^.+)(\\.).+\\..+$', '\\1', .x, perl = TRUE))
    }) %>%
    bind_rows()

# Create skeleton -------------------------------------------------

skeleton <- expand_grid(
    region_meta %>%
        select(region_code = region_code_iso3166_2, region_name),
    sex = c('Female', 'Male', 'Total'),
    year = 1945:2020
)

# Calculate e0 from PCLM ungrouped data ---------------------------

# simple piecewise-exponential life-table
CalculateLifeTable <-
    function (df, x, nx, Dx, Ex) {

        require(dplyr)

        df %>%
            transmute(
                x = {{x}},
                nx = {{nx}},
                mx = {{Dx}}/{{Ex}},
                px = exp(-mx*{{nx}}),
                qx = 1-px,
                lx = head(cumprod(c(1, px)), -1),
                dx = c(-diff(lx), tail(lx, 1)),
                Lx = ifelse(mx==0, lx*nx, dx/mx),
                Tx = rev(cumsum(rev(Lx))),
                ex = Tx/lx
            )

    }

# sum female and male deaths and population exposures to a total column
pclm_with_total <-
    pclm_deaths %>%
    select(
        region_code = region_iso, sex, year, age_start, age_width,
        death_total, population_py
    ) %>%
    pivot_wider(
        id_cols = c(region_code, year, age_start, age_width),
        names_from = c(sex),
        values_from = c(death_total, population_py),
        names_sep = '.'
    ) %>%
    mutate(
        death_total.Total = death_total.Female + death_total.Male,
        population_py.Total = population_py.Female + population_py.Male,
    ) %>%
    pivot_longer(
        cols = starts_with(c('death_total', 'population_py')),
        names_to = c('.value', 'sex'), names_sep = '\\.'
    )

pclm_lifetab <-
    pclm_with_total %>%
    arrange(region_code, sex, year, age_start) %>%
    group_by(region_code, sex, year) %>%
    group_modify(~{
        CalculateLifeTable(.x, age_start, age_width, death_total, population_py)
    }) %>%
    ungroup()

pclm_lifetab_harmonized <-
    pclm_lifetab %>%
    filter(x == 0) %>%
    group_by(region_code, sex) %>%
    ungroup() %>%
    select(region_code, sex, year, e0_pclm = ex)

# Harmonize PCLM deaths -------------------------------------------

pclm_deaths_harmonized <-
    pclm_with_total %>%
    group_by(year, region_code, sex) %>%
    summarise(deaths = sum(death_total)) %>%
    ungroup() %>%
    select(region_code, sex, year, deaths_pclm = deaths) %>%
    arrange(region_code, sex, year)

# Harmonize HMD life table data -----------------------------------

hmd_lifetab_harmonized <-
    hmd_lifetab %>%
    as_tibble() %>%
    filter(age == 0) %>%
    mutate(
        sex = case_when(
            sex == 'b' ~ 'Total', sex == 'f' ~ 'Female', sex == 'm' ~ 'Male')
    ) %>%
    right_join(region_meta, by = c('country' = 'region_code_hmd')) %>%
    select(region_code = region_code_iso3166_2, sex, year, e0_hmd = ex) %>%
    arrange(region_code, sex, year)

# Harmonize HMD death counts --------------------------------------

hmd_deaths_harmonized <-
    hmd_deaths %>%
    pivot_longer(
        cols = c(Female, Male, Total),
        values_to = 'deaths', names_to = 'sex'
    ) %>%
    group_by(Year, Country, sex) %>%
    summarise(deaths = sum(deaths)) %>%
    ungroup() %>%
    right_join(region_meta, by = c('Country' = 'region_code_hmd')) %>%
    select(region_code = region_code_iso3166_2, sex, year = Year, deaths_hmd = deaths) %>%
    arrange(region_code, sex, year)

# Merge -----------------------------------------------------------

de0 <-
    skeleton %>%
    left_join(pclm_lifetab_harmonized) %>%
    left_join(pclm_deaths_harmonized) %>%
    left_join(hmd_lifetab_harmonized) %>%
    left_join(hmd_deaths_harmonized) %>%
    mutate(
        e0 = ifelse(is.na(e0_hmd), e0_pclm, e0_hmd),
        deaths = ifelse(is.na(deaths_hmd), deaths_pclm, deaths_hmd)
    ) %>%
    group_by(region_code, sex) %>%
    mutate(
        le0 = log(e0),
        de0 = e0 - lag(e0),
        dle0 = le0 - lag(le0),
        ddeath = deaths - lag(deaths),
        dldeath = log(deaths) - lag(log(deaths))
    ) %>%
    ungroup() %>%
    filter(region_code %in% config$regions_for_all_cause_analysis)

# Preliminary analysis --------------------------------------------

# predict dle0
de0_prd <-
    de0 %>%
    filter(year >= 1970) %>%
    group_by(region_code, sex) %>%
    group_modify(~{
        dat <- .x %>% arrange(year)
        wgt <- 1 - (dat$year == 2020)
        fit <- lm(dle0 ~ dldeath, data = dat, weights = wgt)
        prd <- predict(fit, dat)
        bind_cols(dat, dle0_pred = prd) %>%
            mutate(
                e0_pred = exp(dle0_pred + lag(le0)),
                de0_pred = e0_pred - lag(e0),
                model = list(fit)
            )
    }) %>%
    ungroup()

de0_fit <-
    de0_prd %>%
    group_by(region_code, sex) %>%
    group_modify(~{
        tibble(
            a = coef(.x$model[[1]])[2],
            r2 = summary(.x$model[[1]])[['r.squared']]
        )
    }) %>%
    ungroup()

de0_prd_sub <-
    de0_prd %>%
    filter(sex == 'Male')
de0_fit_sub <-
    de0_fit %>%
    filter(sex == 'Total')

fig$de0 <-
    de0_prd_sub %>%
    ggplot(aes(x = dldeath, y = dle0)) +
    geom_hline(yintercept = 0, size = 1, color = 'grey80') +
    geom_vline(xintercept = 0, size = 1, color = 'grey80') +
    geom_line(aes(x = dldeath, y = dle0_pred), color = 'grey50') +
    geom_point(aes(color = ifelse(year == 2020, T, F))) +
    scale_x_continuous(breaks = seq(-0.2, 0.2, 0.1)) +
    scale_y_continuous(breaks = seq(-0.2, 0.2, 0.02)) +
    facet_wrap(~region_name) +
    labs(x = expression(Delta~log~D), y = expression(Delta~log~e[0])) +
    #coord_equal() +
    fig_spec$MyGGplotTheme(
        family = 'Roboto',
        axis = '', grid = '', show_legend = FALSE
    ) +
    theme(panel.background = element_rect(fill = 'grey95', color = NA))
fig$de0

fig_spec$ExportFigure(fig$de0, path = 'dat/jonas_ex_diff/', device = 'svg')

e0_diff_comparison <-
    e0_diff_comparison %>%
    mutate(country = fct_reorder(Country, ex_diff_aburto),
           country_pos = as.integer(country))

fig$e0_diff_comparison <-
    e0_diff_comparison %>%
    ggplot(aes(y = country_pos)) +
    annotate('segment', y = seq(1,30,2), yend = seq(1,30,2),
             x = -Inf, xend = Inf,
             color = 'grey90', size = 4) +
    geom_vline(xintercept = 0, color = 'grey80', size = 1) +
    geom_vline(xintercept = seq(-1,-2,-1), size = 0.2, color = 'grey80') +
    geom_point(aes(x = ex_diff_islam), color = 'black') +
    geom_point(aes(x = ex_diff_aburto), color = 'blue') +
    geom_point(aes(x = ex_diff_raksha), color = 'red') +
    fig_spec$MyGGplotTheme(family = 'Roboto', grid = '') +
    scale_y_continuous(
        breaks = e0_diff_comparison$country_pos,
        labels = e0_diff_comparison$country
    ) +
    labs(x = 'Years of life expectancy change', y = NULL)
fig$e0_diff_comparison

fig_spec$ExportFigure(fig$e0_diff_comparison, path = 'dat/jonas_ex_diff/', device = 'svg')

# Export ----------------------------------------------------------

de0 %>%
    write_csv('dat/jonas_ex_diff/de0.csv')
