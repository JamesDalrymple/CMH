
# question 1
# agg$result$q1$dt

# question 2
# agg$result$q2$dt
agg$result$q2$p_dx

# question 3
# agg$result$q3$pct_visits_dt
agg$result$q3$p_pct_visits

# question 4
# agg$result$q4$total
# agg$result$q4$place

# question 5
# agg$result$q5$
# agg$result$q5

xport <- new.env(parent = .GlobalEnv)
wb <- createWorkbook()
# bold option and underline
xport$cs <- CellStyle(wb) + Font(wb, isBold = TRUE) + Border()
# question 1
xport$q1s <- createSheet(wb, sheetName = "q1")
addDataFrame(x = agg$result$q1$dt, sheet = xport$q1s, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xport$cs)
# question 2
xport$q2s <- createSheet(wb, sheetName = "q2")
addDataFrame(x = agg$dx_freq, sheet = xport$q2s, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xport$cs)
addDataFrame(x = modify$missing_dx, sheet = xport$q2s, showNA = FALSE,
  row.names = FALSE, startRow = nrow(agg$dx_freq) + 3, startColumn = 1,
  colnamesStyle = xport$cs)
# question 3
xport$q3s <- createSheet(wb, sheetName = "q3")
addDataFrame(x = agg$result$q3$pct_visits_dt, sheet = xport$q3s, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xport$cs)
# question 4: total
xport$q4s_t <- createSheet(wb, sheetName = "q4 total")
addDataFrame(x = agg$result$q4$total_summary, sheet = xport$q4s_t, showNA = FALSE,
  row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xport$cs)
addDataFrame(x = agg$result$q4$total_binned, sheet = xport$q4s_t, showNA = FALSE,
  row.names = FALSE, startRow = nrow(agg$result$q4$total_summary) + 3,
  startColumn = 1, colnamesStyle = xport$cs)
# question 4
xport$q4s_l <- createSheet(wb, sheetName = "q4 place")
addDataFrame(x = agg$result$q4$loc_summary, sheet = xport$q4s_l, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xport$cs)
addDataFrame(x = agg$result$q4$loc_binned, sheet = xport$q4s_l, showNA = FALSE,
             row.names = FALSE, startRow = nrow(agg$result$q4$loc_summary) + 3,
             startColumn = 1, colnamesStyle = xport$cs)
# question 5
xport$q5s <- createSheet(wb, sheetName = "q5")
addDataFrame(x = agg$result$q5$total_dt, sheet = xport$q5s, showNA = FALSE,
             row.names = FALSE, startRow = 1, startColumn = 1, colnamesStyle = xport$cs)
addDataFrame(x = agg$result$q5$place_dt, sheet = xport$q5s, showNA = FALSE,
             row.names = FALSE, startRow = nrow(agg$result$q5$total_dt) + 3,
             startColumn = 1, colnamesStyle = xport$cs)

# # create sheet data information ---
# sheet_info = createSheet(wb_sum, sheetName="about")
# addDataFrame(x = about_file, sheet = sheet_info, showNA = FALSE,
#              row.names = FALSE, col.names = TRUE, startRow = 1,
#              startColumn = 1, colnamesStyle = xport$cs)
# save workbook ----
saveWorkbook(wb = wb, file = file.path(project_wd$results,
  paste0("uofm & cmh ", input$run_date, ".xlsx")))



ggsave(plot = graph$q4_loc_freq,
       filename = file.path(project_wd$results, "q4_loc_freq.png"),
       width = 8.5, height = 9)
ggsave(plot = graph$q4_total_bp,
       filename = file.path(project_wd$results, "q4_total_bp.png"),
       width = 3, height = 4)
ggsave(plot = graph$q4_total_freq,
       filename = file.path(project_wd$results, "q4_total_freq.png"),
       width = 6.5, height = 4.5)



