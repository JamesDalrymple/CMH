# document setup --------------------------------------------------------------
hh_doc <- docx(title = "Quarterly Medication Report")
hh_doc = declareTitlesStyles(hh_doc,
  stylenames = c("Titre1", "Titre2", "Titre3", "Titre4", "TitleDoc", "Normal"))
# title
hh_doc <- addTitle(hh_doc, value = "Quarterly Medication Report", level = 5)
hh_doc <- addTOC(hh_doc)
hh_doc <- addPageBreak(hh_doc)
# Medications Incidents -------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Medication Incidents", level = 3)
hh_doc <- addParagraph(hh_doc,
  value = "Medication incidents are unsubstantiated incident reports (IRs) regarding medications.")
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$med_inc$fy), width = 5, height = 4.0)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$med_inc$qtr), width = 5.5, height = 5.5)
hh_doc <- addPageBreak(hh_doc)
# Missed Medications ----------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Missed Medication IRs", level = 3)
hh_doc <- addParagraph(hh_doc,
  value = "A consumer can miss a medication for a variety of reasons. All missed medications discussed below are (1) unsubstantiated and (2) may not be the fault of any clinical staff (CMH and/or provider staff). A single consumer may have multiple missed medication IRs.")
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$mm_ir_ven_fy), width = 7.5, height = 5.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() {
    gt <- ggplot_gtable(ggplot_build(graph$mm_ir_ven_qtr))
    gt$layout$clip[gt$layout$name == "panel"] <- "off"
    print(grid::grid.draw(gt), width = 6.0, height = 6.5)
  })
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$mm_ir_con_fy), width = 6, height = 5.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$mm_ir_con_qtr), width = 6, height = 5.5)
hh_doc <- addPageBreak(hh_doc)
# Current Medications ---------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "Current Medications", level = 3)
hh_doc <- addParagraph(hh_doc,
                       value = "We are only able to see what consumers are on on the report run date, which is why current Medications are medications consumers are currently prescribed to take at that particular point in time. We cannot know if they fill the prescription, if they take the prescription, or any other such informtation.")
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$benzo), width = 5.5, height = 4.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
                  fun = function() print(graph$stim), width = 5.5, height = 3.8)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(graph$benzo_stim), width = 5.5, height = 3.8)
writeDoc(hh_doc, file = file.path(proj_wd$results,
  paste0("outcomes run ", input$run_dt, ".docx")))
p_msg("quarterly medication report complete")
