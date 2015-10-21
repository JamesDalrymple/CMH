# funding bucket -- must download manually -- ranged point in  time data
sql <- new.env(parent=as.environment("aux"))
assign(x = "search_dates", value =
  format(date_convert(c(
    user_input$end_date, user_input$run_date
  )), "%m_%d_%y"),
  envir = sql
)

with(sql, search_dates)

gsub(x = user_input$run_date, pattern = "/", replace = "_")

# insure -- CMH Open Ins 2046 sheet1 -- point in  time data
# ins_detail -- CMH Open Ins 2046 sheet2 -- point in time data
# case_load -- case load 2002 sheet2 -- point in time data
# court -- 2061 sheet1, court order repetition & PRR -- point in time data
# demo -- CMH demo 2105 -- point in time data
# diagnoses -- diagnoses 2157 -- point in time data
# locus -- locus 2227 -- ranged point in time data