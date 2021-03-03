#===============================================================================
# 2021-03-03 -- ex delta
# Prepare data
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

# function to localize paths
devtools::source_gist("32e9aa2a971c6d2682ea8d6af5eb5cde")
# prepare session
source("src/00-perp-session.R" %>% lp)


# hmd life tables and deaths  ----------------------------------------------

# !!! The code below assumes that one has full HMD downloaded and unpacked locally. To get a copy of it go to
# https://www.mortality.org/cgi-bin/hmd/hmd_download.php
# and choose "All statistics for HMD", currently 160326 Kb
# Ilya: my local copy was downloaded on 2021-02-02

# functions to read local HMD directories
devtools::source_gist("0f93062f2b67eeac69949554027fa84f")

# the local HMD path (Ilya)
hmdpath <- fs::as_fs_path("~/data/hmd/")

# deaths
d1x1 <- path(hmdpath, "deaths", "Deaths_1x1") %>% fread_hmd_dir() %>%
    rename(b = total, f = female, m = male)

save(d1x1, file = "dat/d1x1.rda" %>% lp, compress = "xz")

# life expectancy
lt1x1 <- bind_rows(
    b = path(hmdpath, "lt_both", "bltper_1x1") %>% fread_hmd_dir(),
    f = path(hmdpath, "lt_female", "fltper_1x1") %>% fread_hmd_dir(),
    m = path(hmdpath, "lt_male", "mltper_1x1") %>% fread_hmd_dir(),
    .id = "sex"
)

save(lt1x1, file = "dat/lt1x1.rda" %>% lp, compress = "xz")
