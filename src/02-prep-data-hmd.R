#===============================================================================
# 2021-03-03 -- ex delta
# Prepare data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source("src/00-perp-session.R" %>% lp)


# load back the data
load("dat/d1x1.rda" %>% lp)
load("dat/lt1x1.rda" %>% lp)



# age 0 -------------------------------------------------------------------

# sum total deaths
d_all <- d1x1 %>%
    pivot_longer(f:b, names_to = "sex") %>%
    group_by(country, year, sex) %>%
    summarise(death = value %>% sum)

# extract life expectancy at age 0
e0 <- lt1x1 %>%
    filter(age == 0) %>%
    select(country, year, sex, ex)

# join the two and calculate changes and ratios
hmd_0 <- left_join(d_all, e0) %>%
    group_by(country, sex) %>%
    mutate(
        death_change = death %>% subtract(lag(death)),
        rel_death_change = death_change %>% divide_by(lag(death)),
        ex_change = ex %>% subtract(lag(ex)),
        death_ratio = death %>% divide_by(lag(death)),
        ex_ratio = ex %>% divide_by(lag(ex))
    ) %>%
    ungroup()

# save
save(hmd_0, file = "out/hmd_0.rda" %>% lp, compress = "xz")


# age 30 ------------------------------------------------------------------
# the same for ages 30+

d_30 <- d1x1 %>%
    filter(age %>% is_weakly_greater_than(30)) %>%
    pivot_longer(f:b, names_to = "sex") %>%
    group_by(country, year, sex) %>%
    summarise(death = value %>% sum)

e30 <- lt1x1 %>%
    filter(age == 30) %>%
    select(country, year, sex, ex)


hmd_30 <- left_join(d_30, e30) %>%
    group_by(country, sex) %>%
    mutate(
        death_change = death %>% subtract(lag(death)),
        rel_death_change = death_change %>% divide_by(lag(death)),
        ex_change = ex %>% subtract(lag(ex)),
        death_ratio = death %>% divide_by(lag(death)),
        ex_ratio = ex %>% divide_by(lag(ex))
    ) %>%
    ungroup()

# save
save(hmd_30, file = "out/hmd_30.rda" %>% lp, compress = "xz")
