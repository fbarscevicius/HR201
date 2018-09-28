# Challenge # 6 Solution Code ----
# Note that this is an extension of the Chapter 8 Code
# and all Chapter 8 code + Challenge #5 Solution must be run first!

# Part 2: Perform sensitivity analysis at optimal threshold ----

net_revenue_per_employee <- 250000
avg_overtime_pct   <- seq(0.05, 0.30, by = 0.05)
stock_option_cost <- seq(5000, 25000, by = 5000)


# Part 2: Solution -----

max_savings_rates_tbl_3 <- rates_by_threshold_optimized_tbl_3 %>%
    filter(savings == max(savings))

calculate_savings_by_threshold_3_preloaded <- partial(
    calculate_savings_by_threshold_3,
    # Function Arguments
    data = test_tbl,
    h2o_model = automl_leader,
    threshold = max_savings_rates_tbl_3$threshold,
    tnr = max_savings_rates_tbl_3$tnr,
    fnr = max_savings_rates_tbl_3$fnr,
    fpr = max_savings_rates_tbl_3$fpr,
    tpr = max_savings_rates_tbl_3$tpr
)

calculate_savings_by_threshold_3_preloaded(
    avg_overtime_pct = 0.10, 
    net_revenue_per_employee = 250000,
    stock_option_cost = 5000
)

sensitivity_tbl_3 <- list(
    avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
    net_revenue_per_employee = 250000,
    stock_option_cost = seq(5000, 25000, by = 5000)
) %>%
    cross_df() %>%
    mutate(
        savings = pmap_dbl(
            .l = list(
                avg_overtime_pct = avg_overtime_pct,
                net_revenue_per_employee = net_revenue_per_employee,
                stock_option_cost = stock_option_cost
            ),
            .f = calculate_savings_by_threshold_3_preloaded
        )
    )

sensitivity_tbl_3

sensitivity_tbl_3 %>%
    ggplot(aes(avg_overtime_pct, stock_option_cost)) +
    geom_tile(aes(fill = savings)) +
    geom_label(aes(label = savings %>% round(0) %>% scales::dollar())) +
    theme_tq() +
    theme(legend.position = "none") +
    scale_fill_gradient2(
        low = palette_light()[[2]],
        mid = "white",
        high = palette_light()[[1]],
        midpoint = 0
    ) +
    scale_x_continuous(
        labels = scales::percent,
        breaks = seq(0.05, 0.30, by = 0.05)
    ) +
    scale_y_continuous(
        labels = scales::dollar,
        breaks = seq(5000, 25000, by = 5000)
    ) +
    labs(
        title = "Profitability Heatmap: Expected Savings Sensitivity Analysis",
        subtitle = "How sensitive is savings to stock option cost and average overtime percentage?",
        x = "Average Overtime Percentage",
        y = "Average Stock Option Cost"
    )




