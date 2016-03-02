# document setup --------------------------------------------------------------
hh_doc <- docx(title = "Health Home Utilization Levels")
hh_doc = declareTitlesStyles(hh_doc,
                             stylenames = c("Titre1", "Titre2", "Titre3", "Titre4", "TitleDoc", "Normal"))
# title
hh_doc <- addTitle(hh_doc, value = "Health Utilization Levels", level = 5)
hh_doc <- addTOC(hh_doc)
hh_doc <- addPageBreak(hh_doc)
# Introductory Paragraph ------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Introduction", level = 1)
hh_doc <- addParagraph(hh_doc,
                       value = "We have 12 point in time datasets created jointly by the PHIP and WCCMH data analysts. This provides us an opportunity to evaluate utilization level categorization over time by team assignment (Health Home team versus the rest of WCCMH teams combined). The data used for ER visits spans two years (FY 2014, FY 2015) and the data used for utilization movement covers one year (FY 2015).")
hh_doc <- addTitle(hh_doc, value = "Document Definitions:", level = 2)
hh_doc <- addParagraph(hh_doc,
                       value = "'non-HH': a consumer on a WCCMH team that is not 'Health Home.'")
hh_doc <- addTitle(hh_doc, value = "Technical Details", level = 2)
hh_doc <- addParagraph(hh_doc,
                       value = "Please see Jessica's data dictionary OR Snow's E2 report 2271 for how utilization level is calculated. When Jessica moved from CSTS/WCCMH, she no longer provided utilization level, and Snow resumed this duty via E2 report 2271.")
hh_doc <- addPageBreak(hh_doc)
# ER Visits -------------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "ER Visits", level = 2)
hh_doc <- addParagraph(hh_doc,
                       value = "The consumers in a given month, say 'Sep', were present both in September 2015 and September 2014. We conduct a Mann-Whitney test, which is a nonparametric test for comparing two datasets. A t-test is not appropriate in this case since the distribution of the ER count data is heavy-tailed toward the left.

Legend
***  convincing evidence to reject the null hypothesis with a p-value less than or equal to 0.001.
**  strong evidence to reject the null hypothesis with a p-value in the interval (0.001, 0.01].
*  moderate evidence to reject the null hypothesis with a p-value in the interval (0.01, 0.05].
+  weak evidence to reject the null hypothesis with a p-value in the interval (0.05, 0.10].
-  no evidence to reject the null hypothesis with a p-value in the interval (0.1, 1].")
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$util_er), width = 6, height = 6)
hh_doc <- addPageBreak(hh_doc)
# Utilization Level Movement --------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Utilization Level Movement", level = 1)
hh_doc <- addParagraph(hh_doc,
                       value = "Consumers need to show up consecutive months or they are discarded. This means that new consumers assigned a utilization level show up in next month")
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$movement_a), width = 6.5, height = 3.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$movement_b), width = 6.5, height = 3.5)
# saving document
if (!dir.exists(project_wd$results)) {
  dir.create(project_wd$results)
  p_msg("directory created", project_wd$results)
}

writeDoc(hh_doc, file = file.path(project_wd$results,
  paste0("ER util_levels run ", format(input$run_date, "%m_%d_%y"), ".docx")))