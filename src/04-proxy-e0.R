#===============================================================================
# 2021-03-03 -- ex delta
# Estimate e0
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source("src/00-perp-session.R" %>% lp)

# load the function to proxy ex
source("src/03-fun-proxy-ex.R" %>% lp)


# crossing of the countries and years
coo <- crossing(xcountry = hmd_0$country %>% unique(), xyear = 1990:2019)


# estimate e0
df_e0 <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = ex_proxy,
    df = hmd_0, xsex = "b", n_years = 20
)

save(df_e0, file = "out/proxy-1990-2019-20y-both.rda" %>% lp)



# same estimates for age 30 -----------------------------------------------

# estimate e30
df_e30 <- pmap_df(
    .l = list(xcountry = coo$xcountry, xyear = coo$xyear),
    .f = ex_proxy,
    df = hmd_30, xsex = "b", n_years = 20
)

save(df_e30, file = "out/proxy-1990-2019-20y-both-e30.rda" %>% lp)
