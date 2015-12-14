hh_doc <- docx(title = "Health Home Outcomes")
hh_doc <- addTOC(hh_doc)
# title ---
hh_doc <- addParagraph(hh_doc, value = pot("Health Home Outcomes", textProperties(font.size = 16, font.weight = "bold")))
# Introductory Paragraph ------------------------------------------------------
hh_doc <- addParagraph(hh_doc,
  value = "As of December 2015, we only have health home data going back to 7/1/2014. This means that we have little more than a year's worth of data. For this reason, when we are analyzing any particular outcome, such as BMI, we look at the first value in the date range and compare it to the last value in the date range. This means that for the time being, we only have one data point (using the aforementioned methodology). Therefore, it is not helpful to show a single number graphically, as there is no comparison to be made. We can definitively state that data collection needs to improve across the organization, and that health home consumers do not have more data recorded that their counterparts that are not enrolled in the health home program. It does not make sense to compute statistics if we cannot confidently claim that we have a vast majority of acceptable cases in each of our respective consumer pools (health home consumers and other CMH consumers). We can, however, safely present summary statistics, which we do, as long as we show the amount of missing data.")
hh_doc <- addParagraph(hh_doc, value = pot("Document Definitions:",
  textProperties(font.size = 12, font.weight = "bold", underlined = TRUE)))
hh_doc <- addParagraph(hh_doc,
  value = "'before': the first value in the date range found for a consumer for a particular outcome.
'after': the last value in the date range found for a consumer for a particular outcome.
'eligible cases': the number of consumers in an ideal world that would have qualified to be analyzed (some, due to age, were dropped: age must be at least 18 years).
  'rate imp/maintain': the combined rate of either maintaining their outcome value or improving their outcome value.")
hh_doc <- addParagraph(hh_doc, value = pot("Technical Details:",
  textProperties(font.size = 12, font.weight = "bold", underlined = TRUE)))
hh_doc <- addParagraph(hh_doc,
  value = "1. We define a 'before' and 'after' as having minimally a difference of 30 days.
  'rate data': the rate at which a group of consumers has sufficient data collected as to have a 'before' and 'after.' A consumer without such a 'before' and 'after' would not be placed in an improvement category.
  2. Systolic values needs to be greater than diastolic values, or the record is considered to be an error and discarded in the code.
  3. Records must have both systolic and diastolic values; if either is missing the record is considered to be an error and discarded in the code.")

hh_doc <- addParagraph(hh_doc, value = pot("Blood Pressure Chart Sources"))
hh_doc <- addParagraph(hh_doc,
  value = "http://jama.jamanetwork.com/article.aspx?articleid=1791497
  http://www.disabled-world.com/artman/publish/bloodpressurechart.shtml")

hh_doc <- addPageBreak(hh_doc)
# BMI -------------------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Body Mass Index (BMI)")
# BMI: CMH teams ---
hh_doc <- addParagraph(hh_doc, pot("BMI: CMH Teams", textBold()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(saved$bmi_team[, .SD,
  .SDcols = c("team", "decreased", "improved", "maintained", "eligible cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("... continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
                       FlexTable(saved$bmi_team[, .SD,
  .SDcols = c("team", "rate improved", "rate_imp/maint", "rate data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# BMI: Health Home team status ---
hh_doc <- addParagraph(hh_doc, pot("BMI: Health Home Team Status", textBold()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bmi_hh[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained", "eligible cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("... continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bmi_hh[, .SD,
  .SDcols = c("hh_team", "rate improved", "rate_imp/maint", "rate data")]))
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addPageBreak(hh_doc)
# Blood Pressure: dw categorization -------------------------------------
hh_doc <- addTitle(hh_doc,
  value = "Blood Pressure: Disabled World (DW) Categorization")
hh_doc <- addParagraph(hh_doc,
  value = "We use the following table to categorize blood pressures into ten groups. Our data sources are CMH document 'chronic health.docx' and the following weblink:
http://www.disabled-world.com/artman/publish/bloodpressurechart.shtml. The disabled world blood pressure table is similar to the American Heart Association blood pressure chart, which does not take age into consideration. As age should be considered, we augment the disabeled world table with the 'chronic health.docx' information, which most likely is derived from the 2013 JAMA, see the next section for details.")
# dw systolic categorization
hh_doc <- addParagraph(hh_doc,
  value = "DW Chart, modified for age per CMH chronic health documentation")
hh_doc <- addFlexTable(hh_doc, FlexTable(aux$bp_dw))
hh_doc <- addParagraph(hh_doc,
  value = pot("Systolic DW Categorization", textBold()),
  stylename = "Normal")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_dw_sys[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
              "eligible cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("...continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_dw_sys[, .SD,
  .SDcols = c("hh_team", "rate improved", "rate imp/maint", "rate data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# dw diastolic categorization
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addParagraph(hh_doc,
  value = pot("Diastolic Detailed Categorization", textBold()),
  stylename = "Normal")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_dw_dia[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
  "eligible cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("...continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_dw_dia[, .SD,
  .SDcols = c("hh_team", "rate improved", "rate imp/maint", "rate data")]))
hh_doc <- addPageBreak(hh_doc)
# Blood Pressure: JAMA Method ---------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Blood Pressure: JAMA Hybrid Approach")
hh_doc <- addParagraph(hh_doc,
                       value = "We use the 2013 Journal of American Medical Association paper regarding outcomes for people ages '18-59' and 'at least 60.' The following table uses their expert recommendations and opinions, and we fill in the blanks where we must (the hypotension categories). This is the best we can do to give credit to people improving and yet not penalize consumers for
slight, natural variations in blood pressure. Our data sources are CMH document 'chronic health.docx' and the following weblink:
                       http://jama.jamanetwork.com/article.aspx?articleid=1791497
                       Subject to change per medical staff review.")
hh_doc <- addParagraph(hh_doc,
  value = "JAMA 2013 Blood Pressure Table, missing ranges assigned sensible values.")
hh_doc <- addFlexTable(hh_doc, FlexTable(aux$bp_jama))

# jama categorization: systolic
hh_doc <- addParagraph(hh_doc,
  value = pot("Systolic Distance", textBold()), stylename = "Normal")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_jama_sys[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
  "eligible cases")]))
hh_doc <- addParagraph(hh_doc, value = "...continued...")
hh_doc <- addFlexTable(hh_doc,
 FlexTable(analyze$bp_jama_sys[, .SD,
 .SDcols = c("hh_team", "rate improved", "rate imp/maint", "rate decreased",
             "rate data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# jama categorization: diastolic
hh_doc <- addParagraph(hh_doc,
  value = pot("Diastolic Distance", textBold()), stylename = "Normal")
hh_doc <- addParagraph(hh_doc, value = "")
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_jama_dia[, .SD,
  .SDcols = c("hh_team", "decreased", "improved", "maintained",
  "eligible cases")]))
hh_doc <- addParagraph(hh_doc, value = pot("...continued...", textItalic()))
hh_doc <- addFlexTable(hh_doc,
  FlexTable(analyze$bp_jama_dia[, .SD,
  .SDcols = c("hh_team", "rate improved", "rate imp/maint", "rate decreased",
              "rate data")]))
hh_doc <- addParagraph(hh_doc, value = "")
# Wellness Notes --------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Wellness Notes")
hh_doc <- addParagraph(hh_doc, value = "Under Construction, coming soon!")
# Conclusions -----------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Conclusions")
hh_doc <- addParagraph(hh_doc,
  value = "There are a few points we must make:
  1. Data collection is poor in the entire organization, Health Home included, and must be improved if we are to trust any analysis. The exception to this seems to be blood pressure.
  2. The rate of improvement is poor in every outcome category, regardless of Health Home team status.
  3. People are maintaing blood pressure values moreso than improving.")

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
