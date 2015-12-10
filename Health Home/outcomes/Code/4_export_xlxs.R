hh_doc <- docx(title = "Health Home Outcomes")
hh_doc <- addTOC(hh_doc)

hh_doc <- addTitle(hh_doc, value = "BMI")
hh_doc <- addParagraph(hh_doc, pot("BMI: CMH Teams", textBold()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$print$bmi_wide_team[, .SD,
  .SDcols = c("team", "decreased", "improved", "maintained", "eligible_cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("... continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
                       FlexTable(saved$print$bmi_wide_team[, .SD,
  .SDcols = c("team", "rate_improve", "rate_imp/maint", "rate_data")]))
hh_doc <- addParagraph(hh_doc, value = "")

hh_doc <- addParagraph(hh_doc, pot("BMI: Health Home Team Status", textBold()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bmi_hh[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained", "eligible_cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("... continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bmi_hh[, .SD,
  .SDcols = c("hh_team", "rate_improve", "rate_imp/maint", "rate_data")]))
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addPageBreak(hh_doc)
hh_doc <- addTitle(hh_doc, value = "Blood Pressure: Detailed Categorization")
# detailed systolic categorization
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addParagraph(hh_doc,
  value = pot("Systolic Detailed Categorization", textBold()),
  stylename = "Normal")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_sys_detail[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
              "eligible_cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("...continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_sys_detail[, .SD,
  .SDcols = c("hh_team", "rate_improved", "rate_imp/maint", "rate_data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# detailed diastolic categorization
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addParagraph(hh_doc,
  value = pot("Diastolic Detailed Categorization", textBold()),
  stylename = "Normal")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_dia_detail[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
  "eligible_cases")]))
hh_doc <- addParagraph(hh_doc, value = "...continued...")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_dia_detail[, .SD,
  .SDcols = c("hh_team", "rate_improved", "rate_imp/maint", "rate_data")]))
hh_doc <- addPageBreak(hh_doc)
hh_doc <- addTitle(hh_doc, value = "Blood Pressure: Systolic Distance Method")
# distance from ideal mean: systolic
hh_doc <- addParagraph(hh_doc,
  value = pot("Systolic Distance", textBold()), stylename = "Normal")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_sys_dist[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
  "eligible_cases")]))
hh_doc <- addParagraph(hh_doc, value = "...continued...")
hh_doc <- addFlexTable(hh_doc,
 FlexTable(saved$bp_sys_dist[, .SD,
 .SDcols = c("hh_team", "rate_improved", "rate_imp/maint", "rate_data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# distance from ideal mean: diastolic
hh_doc <- addParagraph(hh_doc,
  value = pot("Diastolic Distance", textBold()), stylename = "Normal")
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_dia_dist[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
  "eligible_cases")]))
hh_doc <- addParagraph(hh_doc, value = "...continued...")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bp_dia_dist[, .SD,
  .SDcols = c("hh_team", "rate_improved", "rate_imp/maint", "rate_data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# hh_doc = addPageBreak(hh_doc)
if (!dir.exists(project_wd$results)) {
  dir.create(project_wd$results)
  p_msg("directory created", project_wd$results)
}
writeDoc(hh_doc, file = file.path(project_wd$results, "outcomes.docx"))

# #### create workbook ####
# wb <- createWorkbook()
# # bold option and underline
# cs3 <- CellStyle(wb) + Font(wb, isBold=TRUE) + Border()
# ### create xx ###
# xx_sheet <- createSheet(wb, sheetName="xx")
# addDataFrame(x=xx, sheet=xx_sheet, showNA=FALSE,
#              row.names = FALSE, startRow=1, startColumn=1, colnamesStyle=cs3)
# ### create sheet data information ###
# sheet_info = createSheet(wb, sheetName="Data Info")
# addDataFrame(x=about_file, sheet=sheet_info, showNA=FALSE,
#              row.names=FALSE, col.names=TRUE, startRow=1,
#              startColumn = 1, colnamesStyle=cs3)
#
#   ### save workbook ###
#   # create result folder
#   ### R 3.2.0 approach
#   if(!dir.exists(file.path(project_wd$results))) {
#     dir.create(file.path(project_wd$results))
#     paste("folder created", file.path(project_wd$results),
#           sep="...")
#   }
#
#   # file name
#   export_file <- paste0(
#     aux$fund_name(keep_funds),
#     "_service_array_", input$run_par, ".xlsx")
#
#   gc(reset = TRUE)
#   saveWorkbook(wb=wb, file=file.path(project_wd$results,
#                                      export_file))
#   print(paste(keep_funds, "completed"))
#   rm(wb)
# }
