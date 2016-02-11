# document setup --------------------------------------------------------------
hh_doc <- docx(title = "Health Home Outcomes")
hh_doc = declareTitlesStyles(hh_doc,
  stylenames = c("Titre1", "Titre2", "Titre3", "Titre4", "TitleDoc", "Normal"))
# title
hh_doc <- addTitle(hh_doc, value = "Health Home Outcomes", level = 5)
hh_doc <- addTOC(hh_doc)
hh_doc <- addPageBreak(hh_doc)
# Introductory Paragraph ------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Introduction", level = 1)
hh_doc <- addParagraph(hh_doc,
  value = "As of December 2015, we only have health home data going back to 7/1/2014. This means that we have little more than a year's worth of data. For this reason, when we are analyzing any particular outcome, such as BMI, we look at the first value in the date range and compare it to the last value in the date range. This means that for the time being, we only have one data point (using the aforementioned methodology). Therefore, it is not helpful to show a single number graphically, as there is no comparison to be made. We can definitively state that data collection needs to improve across the organization, and that health home consumers do not have more data recorded that their counterparts that are not enrolled in the health home program. It does not make sense to compute statistics if we cannot confidently claim that we have a vast majority of acceptable cases in each of our respective consumer pools (health home consumers and other CMH consumers). We can, however, safely present summary statistics, which we do, as long as we show the amount of missing data.")
hh_doc <- addTitle(hh_doc, value = "Document Definitions:", level = 2)
hh_doc <- addParagraph(hh_doc,
  value = "'before': the first value in the date range found for a consumer for a particular outcome.
'after': the last value in the date range found for a consumer for a particular outcome.
'eligible cases': the number of consumers in an ideal world that would have qualified to be analyzed (some, due to age, were dropped: age must be at least 18 years).
  'rate imp/maintain': the combined rate of either maintaining their outcome value or improving their outcome value.")
hh_doc <- addTitle(hh_doc, value = "Technical Details", level = 2)
hh_doc <- addParagraph(hh_doc,
  value = "1. We define a 'before' and 'after' as having minimally a difference of 30 days.
  'rate data': the rate at which a group of consumers has sufficient data collected as to have a 'before' and 'after.' A consumer without such a 'before' and 'after' would not be placed in an improvement category.
  2. Systolic values needs to be greater than diastolic values, or the record is considered to be an error and discarded in the code.
  3. Records must have both systolic and diastolic values; if either is missing the record is considered to be an error and discarded in the code.")

hh_doc <- addTitle(hh_doc, value = "Blood Pressure Chart Sources", level = 2)
hh_doc <- addParagraph(hh_doc,
  value = "http://jama.jamanetwork.com/article.aspx?articleid=1791497")

hh_doc <- addPageBreak(hh_doc)
# BMI -------------------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Body Mass Index (BMI)", level = 1)
# BMI: CMH teams ---
hh_doc <- addTitle(hh_doc, value = "BMI: Adult CMH Teams", level = 2)
hh_doc <- addPlot(hh_doc, # vector.graphic = TRUE,
  fun = function() print(analyze$bmi_team$plot), width = 7, height = 4)
hh_doc <- addFlexTable(hh_doc, FlexTable(analyze$bmi_team$table))
hh_doc <- addPageBreak(hh_doc)
# BMI: Health Home team status ---
hh_doc <- addTitle(hh_doc, value = "BMI: Health Home Team Status", level = 2)
hh_doc <- addPlot(hh_doc, # vector.graphic = TRUE,
  fun = function() print(analyze$bmi_hh$plot), width = 7, height = 3.5)
hh_doc <- addFlexTable(hh_doc, FlexTable(analyze$bmi_hh$table))
hh_doc <- addPageBreak(hh_doc)
# Blood Pressure: JAMA Method ---------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Blood Pressure", level = 1)
hh_doc <- addTitle(hh_doc,
  value = "Approach of Analysis: JAMA Hybrid", level = 2)
hh_doc <- addParagraph(hh_doc,
  value = "We use the 2013 Journal of American Medical Association (JAMA) paper regarding outcomes for people ages '18-59' and 'at least 60.' The following table uses their expert recommendations and opinions, and we fill in the blanks where we must (the hypotension categories) using in-house clinical judgement. This is the best we can do to give credit to people improving and yet not penalize consumers for slight, natural variations (about +/- 10 for systolic) in blood pressure. The following table has been created by the WCCMH statistician and reviewed and approved by two WCCMH nurses.")
hh_doc <- addTitle(hh_doc, value = "JAMA 2013 Blood Pressure Table", level = 3)
hh_doc <- addFlexTable(hh_doc, FlexTable(aux$bp_jama))

# jama categorization: systolic
hh_doc <- addTitle(hh_doc, value = "Systolic Blood Pressure", level = 2)
hh_doc <- addPlot(hh_doc, # vector.graphic = TRUE,
  fun = function() print(analyze$bp$sys$plot), width = 7, height = 3.5)
hh_doc <- addFlexTable(hh_doc, FlexTable(analyze$bp$sys$table))
hh_doc <- addPageBreak(hh_doc)
# jama categorization: diastolic
hh_doc <- addTitle(hh_doc, value = "Diastolic Blood Pressure", level = 2)
hh_doc <- addPlot(hh_doc, # vector.graphic = TRUE,
  fun = function() print(analyze$bp$dia$plot), width = 7, height = 3.5)
hh_doc <- addFlexTable(hh_doc, FlexTable(analyze$bp$dia$table))
hh_doc <- addPageBreak(hh_doc)
# Wellness Notes --------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Wellness Notes", level = 1)
# wellness note: team ---
hh_doc <- addTitle(hh_doc, value = "Wellness Notes: Adult CMH Teams", level = 2)
hh_doc <- addPlot(hh_doc, # vector.graphic = TRUE,
  fun = function() print(analyze$wn$team$plot), width = 7, height = 4)
hh_doc <- addFlexTable(hh_doc, FlexTable(analyze$wn$team$table))
hh_doc <- addPageBreak(hh_doc)
# wellness note: hh_team ---
hh_doc <- addTitle(hh_doc,
  value = "Wellness Notes: Health Home vs CMH", level = 2)
hh_doc <- addPlot(hh_doc, # vector.graphic = TRUE,
  fun = function() print(analyze$wn$hh_team$plot), width = 7, height = 3.5)
hh_doc <- addFlexTable(hh_doc, FlexTable(analyze$wn$hh_team$table))
hh_doc <- addPageBreak(hh_doc)
# Conclusions -----------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Conclusions", level = 1)
hh_doc <- addParagraph(hh_doc, value = pot("Data Collection", textBold()),
par.properties = parProperties(list.style = 'ordered', level = 1 ))
hh_doc <- addParagraph(hh_doc, value = "Data collection regarding BMI and the overall health question in the wellness note is poor in the entire organization, Health Home included, and is being addressed by WCCMH administration. Staff report they do not always weigh a person for various reasons: (1) consumer refuses, (2) consumer is in a wheelchair, (3) consumer is using a walker and/or is unstable, (3) staff that are not in the office do not have a scale large enough to weigh the consumer, (5) closest scale(s) are insufficiently capable of measuring beyond 250-300 pounds. We do note that we have sufficient BMI data worth considering, but we are not at the level required for a sound statistical analysis for both BMI and consumer's overall health, and we will refrain from doing so until data collection improves.")
hh_doc <- addParagraph(hh_doc, value = pot("Outcome Improvement", textBold()),
 par.properties = parProperties(list.style = 'ordered', level = 1 ))
hh_doc <- addParagraph(hh_doc, value = "The rate of improvement is poor in every outcome category, regardless of Health Home team status. We must consider that if people are maintaining, this may be sufficient. Future analysis may determine which consumers are safely maintaining and which consumers are unsafely maintaining. If if every consumer that is maintaining is safely doing so, there is still more data that needs to be collected so that we have 'before' and 'after' data for every eligible consumer.")
hh_doc <- addParagraph(hh_doc, value = pot("Future Considerations", textBold()),
  par.properties = parProperties(list.style = 'ordered', level = 1 ))
hh_doc <- addParagraph(hh_doc, value = "This analysis was conducted on a pool of currently open consumers, which is most likely appropriate at this time. At the two year period (7/1/2016), we will want to alter the pool of eligible consumers to be anyone over the age of 18 for at least one year, regardless of CMH open status at the time of the Health Outcome Analysis. Additionally, we will want to require that the consumer be admitted to MI Adult, ACT, DD Adult, or Access, as we are not investigating outcomes on children (note that the health home team does not serve consumers less than 18 years of age). It would not be productive to put into place this new pool until at least two years have passed since Health Home started.")
# saving document
if (!dir.exists(project_wd$results)) {
  dir.create(project_wd$results)
  p_msg("directory created", project_wd$results)
}


writeDoc(hh_doc, file = file.path(project_wd$results,
  paste0("outcomes run ", format(input$run_date, "%m_%d_%y"), ".docx")))

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
