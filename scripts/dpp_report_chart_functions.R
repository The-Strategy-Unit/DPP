#' ----------------------------------------------------
#' REPORT CHARTS
#' 
#' Functions to produce charts in reports
#' ------------------------------------------------------------

# define a function to plot time series data facetted by value, numerator and denominator

#' Return a plot showing a time series view for a single measurement
#' 
#' Takes a `df` which is pre-filtered for a single measurement (one of `value`, 
#' `numerator` or `denominator`) and produces a time-series chart illustrating
#' the average (mean) point-estimates and a range of 1 standard deviation above
#' and below for each series e.g. (`Intervention` and `Control`).
#'
#' @param df Tibble of `df_did_data_transformed` which is filtered for either c(`value`, `numerator`, or `denominator`)
#' @param y_axis_title String - the text to label the y-axis
#'
#' @returns {ggplot2} plot showing a single timeseries
plot_cvd_metric_timeseries_single <- function(df, y_axis_title) {

  p <-
    df |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = year_month,
        y = mean,
        colour = flag_intervention_f,
        group = flag_intervention_f
      )
    ) +
    # add start of intervention
    ggplot2::geom_vline(
      xintercept = zoo::as.yearmon(project_vars$project_start_ym),
      linetype = 'dotted'
    ) +
    ggplot2::geom_text(
      data = df |>
        dplyr::summarise(
          # handle single practice where no lower value available
          y_pos_l = min(lower, na.rm = TRUE),
          y_pos_m = min(mean, na.rm = TRUE),
          flag_intervention_f = 'Intervention' |> forcats::fct(),
          .by = c(measurement)
        ) |> 
        dplyr::mutate(
          y_pos = pmin(y_pos_l, y_pos_m)
        ),
      ggplot2::aes(
        label = "  DPP activity →",
        x = zoo::as.yearmon(project_vars$project_start_ym),
        y = y_pos * 0.99,
        group = measurement
      ),
      hjust = 0,
      colour = 'black'
    ) +
    # add time series
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = lower,
        ymax = upper
      ),
      position = ggplot2::position_dodge(width = 0.05),
      alpha = 0.4
    ) +
    ggplot2::geom_point(
      size = 3,
      position = ggplot2::position_dodge(width = 0.05)
    ) +
    # note time period ids
    ggplot2::geom_text(
      data = df |>
        # handle single practice where no upper value available
        dplyr::summarise(
          y_pos_u = max(upper, na.rm = TRUE),
          y_pos_m = max(mean, na.rm = TRUE),
          flag_intervention_f = 'Intervention' |> forcats::fct(),
          .by = c(year_month, time_period_id, measurement)
        ) |> 
        dplyr::mutate(
          y_pos = pmax(y_pos_u, y_pos_m)
        ),
      ggplot2::aes(
        label = time_period_id,
        x = year_month,
        y = y_pos * 1.1
      ),
      colour = 'black'
    ) +
    # facetting
    # ggplot2::facet_wrap(
    #   facets = ggplot2::vars(measurement), 
    #   ncol = 1,
    #   scales = 'free'
    # ) +
    ggplot2::scale_colour_manual(
      values = c('Intervention' = '#0072CE', 'Control' = '#9E9E9E')
    ) +
    zoo::scale_x_yearmon(
      breaks = unique(df$year_month)
    ) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::labs(
      title = unique(df$measurement),
      x = 'CVDPREVENT reporting period',
      y = stringr::str_wrap(y_axis_title, width = 25),
      colour = 'Group'
    ) +
    ggplot2::theme(
      #axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = '#768692', fill = NA),
      strip.background = ggplot2::element_rect(fill = '#768692'),
      strip.text = ggplot2::element_text(colour = 'white')
    )
  
  return(p)
}

#' Return a plot showing a time series view for a single measurement
#' 
#' Takes a `df` which is pre-filtered for a single measurement (one of `value`, 
#' `numerator` or `denominator`) and produces a time-series chart illustrating
#' the average (mean) point-estimates and a range of 1 standard deviation above
#' and below for each series e.g. (`Intervention` and `Control`).
#'
#' @param df Tibble of `df_did_data_transformed` which is filtered for either c(`value`, `numerator`, or `denominator`)
#' @param y_axis_title String - the text to label the y-axis
#'
#' @returns {ggplot2} plot showing a single timeseries
plot_cvd_metric_timeseries_single_v2 <- function(df, y_axis_title) {
  
  p <-
    df |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = year_month,
        y = mean,
        colour = flag_intervention_f,
        group = flag_intervention_f
      )
    ) +
    # colour intervention period
    ggplot2::annotate(
      geom = 'rect',
      xmin = zoo::as.yearmon(project_vars$project_start_ym),
      xmax = zoo::as.yearmon(project_vars$project_end_ym),
      ymin = -Inf,
      ymax = Inf,
      fill = '#0640f8', alpha = 0.03
    ) +
    # add start of intervention
    ggplot2::geom_vline(
      xintercept = zoo::as.yearmon(project_vars$project_start_ym),
      linetype = 'dotted'
    ) +
    # add end of intervention
    ggplot2::geom_vline(
      xintercept = zoo::as.yearmon(project_vars$project_end_ym),
      linetype = 'dotted'
    ) +
    # add add label for the start of the intervention
    ggplot2::geom_text(
      data = df |>
        dplyr::summarise(
          # handle single practice where no lower value available
          y_pos_l = min(lower, na.rm = TRUE),
          y_pos_m = min(mean, na.rm = TRUE),
          flag_intervention_f = 'Intervention' |> forcats::fct(),
          .by = c(measurement)
        ) |> 
        dplyr::mutate(
          y_pos = pmin(y_pos_l, y_pos_m)
        ),
      ggplot2::aes(
        label = "  DPP activity →",
        x = zoo::as.yearmon(project_vars$project_start_ym),
        y = y_pos * 0.99,
        group = measurement
      ),
      hjust = 0,
      colour = 'black'
    ) +
    # add time series
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = lower,
        ymax = upper
      ),
      position = ggplot2::position_dodge(width = 0.05),
      alpha = 0.4
    ) +
    ggplot2::geom_point(
      size = 3,
      position = ggplot2::position_dodge(width = 0.05)
    ) +
    ggplot2::scale_colour_manual(
      values = c('Intervention' = '#f9bf07', 'Control' = '#9E9E9E')
    ) +
    zoo::scale_x_yearmon(breaks = unique(df$year_month)) +
    # cut off the chart at Oct 2024
    ggplot2::coord_cartesian(xlim = c(
      zoo::as.yearmon("Feb 2022"), 
      zoo::as.yearmon("Oct 2024"))
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      x = 'CVDPREVENT reporting period',
      title = glue::glue("{y_axis_title} ({df$measurement})"),
      y = dplyr::case_match(
        .x = df$measurement,
        'value' ~ 'Percent',
        'numerator' ~ 'Count of patients',
        'denominator' ~ 'Count of patients'
      ),
      colour = 'Group'
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9),
      axis.title = ggplot2::element_text(colour = 'grey30'),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = '#768692', fill = NA),
      strip.background = ggplot2::element_rect(fill = '#768692'),
      strip.text = ggplot2::element_text(colour = 'white')
    )
  
  return(p)
}

#' Return a plot showing a combined time series view for all measurements
#' 
#' Takes a `df` which has values calculated for the means and upper / lower 
#' values for one standard deviation above and below for each time point and
#' for each series c(`Intervention` and `Control`).
#'
#' @param df Tibble of `df_did_data_transformed`
#' @param str_metric String - the metric being calculated, e.g. 'CVDP001AF'
#'
#' @returns {ggplot2} {patchwork} plot showing a combined view of each timeseries
plot_cvd_metric_timeseries <- function(df, str_metric) {
  
  # define the y-axes
  if (str_metric == 'CVDP001AF') {
    y_axis_value = 'Prevalence of high-risk AF'
    y_axis_numerator = 'Adults with high-risk AF'
    y_axis_denominator = 'Adults registered'
  } else {
    y_axis_value = 'Proportion of people with high-risk AF on anticoagulants'
    y_axis_numerator = 'Adults with high-risk AF on anticoagulants'
    y_axis_denominator = 'Adults with high-risk AF'
  }
  
  # get a plot for the value
  p_value <- 
    df |> 
    dplyr::filter(measurement == 'value') |> 
    plot_cvd_metric_timeseries_single_v2(y_axis_title = y_axis_value)
  
  # get a plot for the numerator
  p_numerator <-
    df |> 
    dplyr::filter(measurement == 'numerator') |> 
    plot_cvd_metric_timeseries_single_v2(y_axis_title = y_axis_numerator)
  
  # get a plot for the denominator
  p_denominator <-
    df |> 
    dplyr::filter(measurement == 'denominator') |> 
    plot_cvd_metric_timeseries_single_v2(y_axis_title = y_axis_denominator)
  
  # combine as patchwork
  library(patchwork)
  
  p_return <-
    p_value + p_numerator + p_denominator + patchwork::plot_layout(
      ncol = 1,
      guides = 'collect',
      axes = 'keep',
      axis_titles = 'keep'
    ) #+
    # patchwork::plot_annotation(
    #   title = unique(df$indicator_short_name)
    # )
}


# 
# this is the original function, now defunct but keep for reference.
# plot_cvd_metric_timeseries <- function(df) {
#   p <-
#     df |> 
#     ggplot2::ggplot(
#       ggplot2::aes(
#         x = year_month,
#         y = mean,
#         colour = flag_intervention_f,
#         group = flag_intervention_f
#       )
#     ) +
#     # add start of intervention
#     ggplot2::geom_vline(
#       xintercept = zoo::as.yearmon(project_vars$project_start_ym),
#       linetype = 'dotted'
#     ) +
#     ggplot2::geom_text(
#       data = df |>
#         dplyr::summarise(
#           # handle single practice where no lower value available
#           y_pos_l = min(lower, na.rm = TRUE),
#           y_pos_m = min(mean, na.rm = TRUE),
#           flag_intervention_f = 'Intervention' |> forcats::fct(),
#           .by = c(measurement)
#         ) |> 
#         dplyr::mutate(
#           y_pos = pmin(y_pos_l, y_pos_m)
#         ),
#       ggplot2::aes(
#         label = "  DPP activity →",
#         x = zoo::as.yearmon(project_vars$project_start_ym),
#         y = y_pos * 0.99,
#         group = measurement
#       ),
#       hjust = 0,
#       colour = 'black'
#     ) +
#     # add time series
#     ggplot2::geom_line(alpha = 0.4) +
#     ggplot2::geom_pointrange(
#       ggplot2::aes(
#         ymin = lower,
#         ymax = upper
#       ),
#       position = ggplot2::position_dodge(width = 0.05),
#       alpha = 0.4
#     ) +
#     ggplot2::geom_point(
#       size = 3,
#       position = ggplot2::position_dodge(width = 0.05)
#     ) +
#     # note time period ids
#     ggplot2::geom_text(
#       data = df |>
#         # handle single practice where no upper value available
#         dplyr::summarise(
#           y_pos_u = max(upper, na.rm = TRUE),
#           y_pos_m = max(mean, na.rm = TRUE),
#           flag_intervention_f = 'Intervention' |> forcats::fct(),
#           .by = c(year_month, time_period_id, measurement)
#         ) |> 
#         dplyr::mutate(
#           y_pos = pmax(y_pos_u, y_pos_m)
#         ),
#       ggplot2::aes(
#         label = time_period_id,
#         x = year_month,
#         y = y_pos * 1.1
#       ),
#       colour = 'black'
#     ) +
#     # facetting
#     ggplot2::facet_wrap(
#       facets = ggplot2::vars(measurement), 
#       ncol = 1,
#       scales = 'free'
#     ) +
#     ggplot2::scale_colour_manual(
#       values = c('Intervention' = '#0072CE', 'Control' = '#9E9E9E')
#     ) +
#     zoo::scale_x_yearmon(
#       breaks = unique(df$year_month)
#     ) +
#     ggplot2::theme_minimal(base_size = 16) +
#     ggplot2::labs(
#       title = unique(df$indicator_short_name),
#       x = 'CVDPREVENT reporting period',
#       colour = 'Group'
#     ) +
#     ggplot2::theme(
#       axis.title.y = ggplot2::element_blank(),
#       axis.text.x = ggplot2::element_text(size = 10),
#       panel.grid.major = ggplot2::element_blank(),
#       panel.grid.minor = ggplot2::element_blank(),
#       panel.border = ggplot2::element_rect(color = '#768692', fill = NA),
#       strip.background = ggplot2::element_rect(fill = '#768692'),
#       strip.text = ggplot2::element_text(colour = 'white')
#     )
#   
#   return(p)
# }


make_stars <- function(pval) {
  
  stars <- dplyr::case_when(
    pval <= 0.001 ~ "***",
    pval <= 0.01 ~ "**",
    pval <= 0.05 ~ "*",
    pval <= 0.1 ~ ".",
    .default = ""
  )
  
  return(stars)
  
}




plot_cvd_did_manual <- function(
    df, str_title, str_y
) {
  p <-
    df |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = flag_time_f, 
        y = mean,
        ymin = sd_lo,
        ymax = sd_hi,
        group = flag_intervention_f,
        colour = flag_intervention_f
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_pointrange(
        position = ggplot2::position_dodge(width = 0.05),
        alpha = 0.4
      ) +
      ggplot2::geom_point(
        size = 3,
        position = ggplot2::position_dodge(width = 0.05)
      ) +
      ggplot2::scale_colour_manual(
        values = c('Intervention' = '#f9bf07', 'Control' = '#9E9E9E')
      ) +
      ggplot2::theme_minimal(base_size = 16) +
      ggplot2::labs(
        #title = 'AF: Prevalence (CVDP001AF) (custom)',
        title = str_title,
        #y = 'Prevalence of high-risk AF',
        y = str_y,
        colour = 'Group'
      ) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(size = 10),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(color = '#768692', fill = NA),
        strip.background = ggplot2::element_rect(fill = '#768692'),
        strip.text = ggplot2::element_text(colour = 'white')
      )
  
  return(p)
}

gttable_cvd_did_manual <- function(df) {
  t <- 
    df |> 
    gt::gt() |> 
    gt::cols_hide(columns = c(statistic, p.value)) |> 
    gt::cols_label(
      term = 'Term',
      estimate = 'Estimate',
      std.error = 'SE',
      conf.lo_95 = 'Lower',
      conf.hi_95 = 'Upper',
      signif = 'Significance'
    ) |> 
    gt::tab_spanner(
      label = 'ATT',
      columns = c(estimate, std.error)
    ) |> 
    gt::tab_spanner(
      label = '95% Confidence Interval',
      columns = c(conf.lo_95, conf.hi_95, signif)
    ) |> 
    gt::fmt_number(
      columns = c(estimate, std.error, conf.lo_95, conf.hi_95),
      decimals = 3
    ) |> 
    gt::tab_options(
      quarto.disable_processing = TRUE,
      table.align = 'left'
    ) |> 
    gt::tab_footnote(
      locations = gt::cells_column_spanners(spanners = 'ATT'),
      footnote = gt::md('Average Treatment Effect in the Treated (ATT). This is the average of the individual treatment effects for the GP practice in this project. Figures are for the point estimate (*Estimate*) and the standard error (*SE*).')
    ) |> 
    gt::tab_footnote(
      locations = gt::cells_column_spanners(spanners = '95% Confidence Interval'),
      footnote = gt::md('A range of values with 95% probability that contains the *true* value of the ATT estimates. Figures are for the *Lower* and *Upper* end of this range and a *Signficance* value that is TRUE when the confidence interval does not cover zero and indicates a statistically significant finding.')
    ) |> 
    # highlight rows where finding is significant
    gtExtras::gt_highlight_rows(
      columns = gt::everything(),
      rows = signif == TRUE,
      fill = '#0072CE',
      alpha = 0.1,
      font_weight = 'normal'
    )
  
  return(t)
}




# gathering some data to use
# NB, in `dpp_gp_report_template.qmd` run everything to chunk 16 to get the data
# assuming the two dates are Mar 2023 (8) and Sep 2024 (20)

# Possible references:
# https://medium.com/@nico_kinel/how-to-create-a-dumbbell-plot-in-r-fd23aa0104ea
# 

# df_did_data |> 
#   dplyr::filter(
#     time_period_id %in% c(8, 20), # mirrored time points
#     indicator_id == 1 # outcome 1 - AF prevalance
#   ) |> 
#   dplyr::mutate(
#     time_period_id_f = time_period_id |> as.character() |> forcats::fct(),
#     prac_code_f = prac_code |> forcats::fct_reorder((numerator))
#   ) |> 
#   # summarise per GP practice
#   ggplot2::ggplot(
#     ggplot2::aes(
#       x = numerator,
#       y = prac_code_f,
#       colour = time_period_id_f,
#     )
#   ) +
#   ggplot2::geom_line(
#     ggplot2::aes(group = prac_code),
#     colour = 'black'
#   ) +
#   ggplot2::geom_point(
#     ggplot2::aes(
#       colour = time_period_id_f
#     ),
#     size = 3
#   ) +
#   ggplot2::theme_minimal() +
#   ggplot2::theme(
#     legend.position = 'top'
#   ) +
#   ggplot2::scale_color_manual(values = c('8' = '#a5b1c2', '20' = '#4b7bec'))


#' Return a plot showing a combined time series view for all measurements
#' 
#' Takes a `df` which has values calculated for the means and upper / lower 
#' values for one standard deviation above and below for each time point and
#' for each series c(`Intervention` and `Control`).
#'
#' @param df Tibble of `df_did_data_transformed`
#' @param df_project Tibble of summary
#' @param str_metric String - the metric being calculated, e.g. 'CVDP001AF'
#'
#' @returns {ggplot2} {patchwork} plot showing a combined view of each timeseries
plot_cvd_metric_timeseries_overall <- function(df, df_project, str_metric) {
  
  # define the y-axes
  if (str_metric == 'CVDP001AF') {
    y_axis_value = 'Prevalence of high-risk AF'
    y_axis_numerator = 'Adults with high-risk AF'
    y_axis_denominator = 'Adults registered'
  } else {
    y_axis_value = 'Proportion of people with high-risk AF on anticoagulants'
    y_axis_numerator = 'Adults with high-risk AF on anticoagulants'
    y_axis_denominator = 'Adults with high-risk AF'
  }
  
  # get a plot for the value
  p_value <- 
    df |> 
    dplyr::filter(measurement == 'value') |> 
    plot_cvd_metric_timeseries_overall_single(
      y_axis_title = y_axis_value, 
      df_project = df_project
    )
  
  # get a plot for the numerator
  p_numerator <-
    df |> 
    dplyr::filter(measurement == 'numerator') |> 
    plot_cvd_metric_timeseries_overall_single(
      y_axis_title = y_axis_numerator,
      df_project = df_project
    )
  
  # get a plot for the denominator
  p_denominator <-
    df |> 
    dplyr::filter(measurement == 'denominator') |> 
    plot_cvd_metric_timeseries_overall_single(
      y_axis_title = y_axis_denominator,
      df_project = df_project
    )
  
  # combine as patchwork
  #library(patchwork)
  
  p_return <-
    p_value + p_numerator + p_denominator + patchwork::plot_layout(
      ncol = 1,
      guides = 'collect',
      axes = 'keep',
      axis_titles = 'keep'
    )
  
  return(p_return)
}

plot_cvd_metric_timeseries_overall_single <- function(
    df, 
    df_project = df_project,
    y_axis_title
  ) {
  
  # get the intervention dates
  df_int_dates <-
    df_project |> 
    dplyr::select(project_code, started_month, ended_month) |> 
    dplyr::summarise(
      project_code = paste0(project_code, collapse = ", "),
      flag_intervention_f = 'Intervention' |> forcats::fct(),
      .by = c(started_month, ended_month)
    )
  
  p <-
    df |> 
    #dplyr::filter(measurement == 'value') |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = year_month,
        y = mean,
        colour = flag_intervention_f,
        group = flag_intervention_f
      )
    ) +
    # colour intervention period
    ggplot2::annotate(
      geom = 'rect',
      xmin = df_int_dates$started_month |> min(na.rm = TRUE),
      xmax = df_int_dates$ended_month |> max(na.rm = TRUE),
      ymin = -Inf,
      ymax = Inf,
      fill = '#0640f8', alpha = 0.03
    ) +
    # add the start of the intervention
    ggplot2::geom_vline(
      xintercept = df_int_dates$started_month,
      linetype = 'dotted',
      alpha = 0.5
    ) + 
    # label the grants starting this month
    ggplot2::geom_text(
      data = df_int_dates,
      ggplot2::aes(
        label = project_code,
        x = started_month,
        y = 0
      ),
      angle = 90,
      color = 'black',
      size = 2,
      hjust = 0,
      nudge_x = -0.03
    ) + 
    # add time series
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = lower,
        ymax = upper
      ),
      position = ggplot2::position_dodge(width = 0.05),
      alpha = 0.4
    ) +
    ggplot2::geom_point(
      size = 3,
      position = ggplot2::position_dodge(width = 0.05)
    ) +
    # note time period ids
    # ggplot2::geom_text(
    #   data = df |>
    #     dplyr::filter(measurement == 'value') |> 
    #     # handle single practice where no upper value available
    #     dplyr::summarise(
    #       y_pos_u = max(upper, na.rm = TRUE),
    #       y_pos_m = max(mean, na.rm = TRUE),
    #       flag_intervention_f = 'Intervention' |> forcats::fct(),
    #       .by = c(year_month, time_period_id, measurement)
    #     ) |>
    #     dplyr::mutate(
    #       y_pos = pmax(y_pos_u, y_pos_m)
    #     ),
    #   ggplot2::aes(
    #     label = time_period_id,
    #     x = year_month,
    #     y = y_pos * 1.1
    #   ),
    #   colour = 'black'
    # ) |> 
    ggplot2::scale_colour_manual(
      values = c('Intervention' = '#f9bf07', 'Control' = '#9E9E9E')
    ) +
    zoo::scale_x_yearmon(breaks = unique(df$year_month)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    # cut off the cart at Oct 2024
    ggplot2::coord_cartesian(xlim = c(
      zoo::as.yearmon('Feb 2022'),
      zoo::as.yearmon('Oct 2024')
    )) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      x = 'CVDPREVENT reporting period',
      #title = unique(df$indicator_short_name),
      title = glue::glue("{y_axis_title} ({df$measurement})"),
      y = dplyr::case_match(
        .x = df$measurement,
        'value' ~ 'Percent',
        'numerator' ~ 'Count of patients',
        'denominator' ~ 'Count of patients'
      ),
      colour = 'Group'
    ) +
    ggplot2::theme(
      #axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = '#768692', fill = NA),
      strip.background = ggplot2::element_rect(fill = '#768692'),
      strip.text = ggplot2::element_text(colour = 'white')
    )
  
  return(p)

}


#' Return a plot showing a combined time series view for all measurements
#' 
#' THIS VERSION IS USED IN THE PROGRAMME-WIDE REPORT
#' 
#' Takes a `df` which has values calculated for the means and upper / lower 
#' values for one standard deviation above and below for each time point and
#' for each series c(`Intervention` and `Control`).
#'
#' @param df Tibble of `df_did_data_transformed`
#' @param df_project Tibble of summary
#' @param str_metric String - the metric being calculated, e.g. 'CVDP001AF'
#'
#' @returns {ggplot2} {patchwork} plot showing a combined view of each timeseries
plot_cvd_metric_timeseries_overall_v2 <- function(df, df_project, str_metric) {
  
  # define the y-axes
  if (str_metric == 'CVDP001AF') {
    y_axis_value = 'Prevalence of high-risk AF'
    y_axis_numerator = 'Adults with high-risk AF'
    y_axis_denominator = 'Adults registered'
  } else {
    y_axis_value = 'Proportion of people with high-risk AF on anticoagulants'
    y_axis_numerator = 'Adults with high-risk AF on anticoagulants'
    y_axis_denominator = 'Adults with high-risk AF'
  }
  
  # get a plot for the value
  p_value <- 
    df |> 
    dplyr::filter(measurement == 'value') |> 
    plot_cvd_metric_timeseries_overall_single_v2(
      y_axis_title = y_axis_value, 
      df_project = df_project
    )
  
  # get a plot for the numerator
  p_numerator <-
    df |> 
    dplyr::filter(measurement == 'numerator') |> 
    plot_cvd_metric_timeseries_overall_single_v2(
      y_axis_title = y_axis_numerator,
      df_project = df_project
    )
  
  # get a plot for the denominator
  p_denominator <-
    df |> 
    dplyr::filter(measurement == 'denominator') |> 
    plot_cvd_metric_timeseries_overall_single_v2(
      y_axis_title = y_axis_denominator,
      df_project = df_project
    )
  
  # combine as patchwork
  #library(patchwork)
  
  p_return <-
    p_value + p_numerator + p_denominator + patchwork::plot_layout(
      ncol = 1,
      guides = 'collect',
      axes = 'keep',
      axis_titles = 'keep'
    )
  
  return(p_return)
}

#' Return a plot showing a single time series view for the specified measurement
#' 
#' THIS VERSION IS USED IN THE PROGRAMME-WIDE REPORT.
#' 
#' Takes a `df` which has values calculated for the means and upper / lower 
#' values for one standard deviation above and below for each time point and
#' for each series c(`Intervention` and `Control`).
#'
#' @param df Tibble of `df_did_data_transformed`
#' @param df_project Tibble of summary
#' @param y_axis_title String description of the y-axis
#'
#' @returns {ggplot2} of the specified timeseries
plot_cvd_metric_timeseries_overall_single_v2 <- function(
    df, 
    df_project = df_project,
    y_axis_title
) {
  
  # get the intervention dates
  df_int_dates <-
    df_project |> 
    dplyr::select(grant_id, started_month) |> 
    dplyr::mutate(
      # set the end month as today - i.e. colour the rest of the chart blue
      ended_month = zoo::as.yearmon(Sys.Date())
    ) |> 
    dplyr::summarise(
      project_code = paste0(grant_id, collapse = ", "),
      flag_intervention_f = 'Intervention' |> forcats::fct(),
      # produce a label for the months which include how many projects started
      project_count = dplyr::n_distinct(grant_id, na.rm = TRUE),
      .by = c(started_month, ended_month)
    ) |> 
    dplyr::mutate(
      started_month_label = glue::glue(
        "{started_month} (n = {project_count})"
      )
    )
  
  p <-
    df |> 
    #dplyr::filter(measurement == 'value') |> 
    ggplot2::ggplot(
      ggplot2::aes(
        x = year_month,
        y = mean,
        colour = flag_intervention_f,
        group = flag_intervention_f
      )
    ) +
    # colour intervention period
    ggplot2::annotate(
      geom = 'rect',
      xmin = df_int_dates$started_month |> min(na.rm = TRUE),
      xmax = df_int_dates$ended_month |> max(na.rm = TRUE),
      ymin = -Inf,
      ymax = Inf,
      fill = '#0640f8', alpha = 0.03
    ) +
    # add the start of the intervention
    ggplot2::geom_vline(
      xintercept = df_int_dates$started_month,
      linetype = 'dotted',
      alpha = 0.5
    ) + 
    # label the months which mark the start of an intervention
    ggplot2::geom_text(
      data = df_int_dates,
      ggplot2::aes(
#        label = project_code,
        label = started_month_label,
        x = started_month,
        y = 0
      ),
      angle = 90,
      color = 'black',
      size = 2,
      hjust = 0,
      nudge_x = -0.03
    ) + 
    # add time series
    ggplot2::geom_line(alpha = 0.4) +
    ggplot2::geom_pointrange(
      ggplot2::aes(
        ymin = lower,
        ymax = upper
      ),
      position = ggplot2::position_dodge(width = 0.05),
      alpha = 0.4
    ) +
    ggplot2::geom_point(
      size = 3,
      position = ggplot2::position_dodge(width = 0.05)
    ) +
    ggplot2::scale_colour_manual(
      values = c('Intervention' = '#f9bf07', 'Control' = '#9E9E9E')
    ) +
    zoo::scale_x_yearmon(breaks = unique(df$year_month)) +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    # cut off the cart at Oct 2024
    ggplot2::coord_cartesian(xlim = c(
      zoo::as.yearmon('Feb 2022'),
      zoo::as.yearmon('Oct 2024')
    )) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      x = 'CVDPREVENT reporting period',
      #title = unique(df$indicator_short_name),
      title = glue::glue("{y_axis_title} ({df$measurement})"),
      y = dplyr::case_match(
        .x = df$measurement,
        'value' ~ 'Percent',
        'numerator' ~ 'Count of patients',
        'denominator' ~ 'Count of patients'
      ),
      colour = 'Group'
    ) +
    ggplot2::theme(
      #axis.title.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = '#768692', fill = NA),
      strip.background = ggplot2::element_rect(fill = '#768692'),
      strip.text = ggplot2::element_text(colour = 'white')
    )
  
  return(p)
  
}
