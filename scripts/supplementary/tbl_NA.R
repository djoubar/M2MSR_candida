tblNA <- tbl_custom_summary(
  df_base,
  include = c(-id_hemoc, -iep),
  stat_fns = ~missing,
  statistic = everything() ~ "{N_miss} ({p_miss} %)"
) |>
  add_overall() |>
  modify_footnote_header(
    footnote = "Nombre de NA (% de NA)",
    columns = all_stat_cols()
  )
