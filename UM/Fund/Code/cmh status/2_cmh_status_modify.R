modify <- new.env(parent = .GlobalEnv)

cmh_adm <- copy(sql$output$cmh_adm)
modify$open_cases <- cmh_adm[, unique(case_no)]

# load one sheet at a time and save to wb file if in input list ---------------

# bold option and underline
cs3 <- CellStyle(wb_fund) + Font(wb_fund, isBold=TRUE) + Border()

for (i in seq_along(sql$fund_names)) { # i = 1
  modify$cur_tab <- sql$fund_names[i]
  modify$cur_data <- read.xlsx2(file.path(project_wd$data,
                                          "Medicare TPP 12 21 15.xlsx"), modify$cur_tab,
                                stringsAsFactors = FALSE, check.names = FALSE)
  modify$cur_data <- data.table(modify$cur_data)
  modify$cur_data[, case_no := as.integer(case_no)]

  if (modify$cur_tab %in% input$add_cmh_status) {
    setkey(modify$cur_data, case_no)[
      J(modify$open_cases), cmh_status := "Open"]
    setkey(modify$cur_data, case_no)[
      !J(modify$open_cases), cmh_status := "Closed"]
    # modify workbook
    removeSheet(wb_fund, sheetName = modify$cur_tab)
    sheet_add <- createSheet(wb_fund, sheetName = modify$cur_tab)
    addDataFrame(x = modify$cur_data, sheet = sheet_add, showNA = FALSE,
                 row.names = FALSE, startRow = 1, startColumn = 1,
                 colnamesStyle = cs3)
  }
}

