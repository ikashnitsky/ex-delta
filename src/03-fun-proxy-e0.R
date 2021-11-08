#===============================================================================
# 2021-03-03 -- ex delta
# Function to proxy ex ratio based on deaths ratio
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================


# func to predict ex ------------------------------------------------------

# the default -- log trans ratios
e0_proxy <- function(
    df = hmd_0,
    xcountry, xsex, xyear, n_years = 20,
    log_trans = TRUE, rel_diff = FALSE
) {

    di <- df %>% filter(country == xcountry, sex == xsex)
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

        # subset years
        di <- di %>% filter(year %in% {xyear-n_years}:{xyear-1})

        # the model
        delta_fit <- lm(log(ex_ratio)~log(death_ratio), di)

        out <- out_na %>%
            mutate(
                predicted_ex_ratio = delta_fit %>% predict(out_na) %>%
                    unname %>% exp,
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
                abs_ex_change = (ex * ex_ratio) %>% subtract(ex),
                abs_predicted_ex_change = (ex * predicted_ex_ratio) %>%
                    subtract(ex),
                prediction_error = abs_predicted_ex_change - abs_ex_change,
                prediction_error_pct = prediction_error/abs_ex_change * 100
            )

        return(out)

    }

}

# func to predict ex ------------------------------------------------------

# add edagger to the model
e0_proxy_edag <- function(
    df = hmd_0,
    xcountry, xsex, xyear, n_years = 20,
    log_trans = TRUE, rel_diff = FALSE
) {

    di <- df %>% filter(country == xcountry, sex == xsex)
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

        # subset years
        di <- di %>% filter(year %in% {xyear-n_years}:{xyear-1})

        # the model
        delta_fit <- lm(log(ex_ratio)~log(death_ratio)+edag, di)

        out <- out_na %>%
            mutate(
                predicted_ex_ratio = delta_fit %>% predict(out_na) %>%
                    unname %>% exp,
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
                abs_ex_change = (ex * ex_ratio) %>% subtract(ex),
                abs_predicted_ex_change = (ex * predicted_ex_ratio) %>%
                    subtract(ex),
                prediction_error = abs_predicted_ex_change - abs_ex_change,
                prediction_error_pct = prediction_error/abs_ex_change * 100
            )

        return(out)

    }

}

#
# ex_proxy_ratio <- function(
#     df = hmd_0,
#     xcountry, xsex, xyear, n_years = 20,
#     log_trans = TRUE, rel_diff = FALSE
# ) {
#
#     di <- df %>% filter(country == xcountry, sex == xsex)
#     out_na <- di %>% filter(year == xyear) %>%
#         mutate(
#             predicted_ex_change = NA,
#             model_r_squared = NA,
#             model_intercept = NA,
#             model_intercept_se = NA,
#             model_beta = NA,
#             model_beta_se = NA,
#             abs_ex_change = NA,
#             abs_predicted_ex_change = NA,
#             prediction_error = NA,
#             prediction_error_pct = NA
#         )
#
#     if (min(di$year) > (xyear-n_years-1) |
#         min(di$year) == xyear) {
#
#         return(out_na)
#
#     } else {
#
#         di <- di %>% filter(year %in% {xyear-n_years}:{xyear-1})
#
#         # specify variants of the model
#         if (isTRUE(log_trans) & !isTRUE(rel_diff)) {
#             delta_fit <- lm(log(ex_ratio)~log(death_ratio), di)
#         } else {
#             delta_fit <- lm(ex_ratio~death_ratio, di)
#         }
#
#         if (isTRUE(rel_diff)){
#             delta_fit <- lm(ex_change~rel_death_change, di)
#         }
#
#
#         out <- out_na %>%
#             mutate(
#                 predicted_ex_change = delta_fit %>% predict(out_na) %>% unname,
#                 model_r_squared = delta_fit %>% summary %>%
#                     extract2("r.squared"),
#                 model_intercept = delta_fit %>% summary %>%
#                     extract2("coefficients") %>% extract(1, 1),
#                 model_intercept_se = delta_fit %>% summary %>%
#                     extract2("coefficients") %>% extract(1, 2),
#                 model_beta = delta_fit %>% summary %>%
#                     extract2("coefficients") %>% extract(2, 1),
#                 model_beta_se = delta_fit %>% summary %>%
#                     extract2("coefficients") %>% extract(2, 2)
#             ) %>%
#             mutate(
#                 abs_ex_change = (ex * ex_change) %>% subtract(ex),
#                 abs_predicted_ex_change = (ex * predicted_ex_change) %>%
#                     subtract(ex),
#                 prediction_error = abs_predicted_ex_change - abs_ex_change,
#                 prediction_error_pct = prediction_error/abs_ex_change * 100
#             )
#
#         return(out)
#
#     }
#
# }
