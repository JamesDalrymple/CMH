doc <- new.env(parent = .GlobalEnv)
# document setup --------------------------------------------------------------
doc <- docx(title = "New CMH Core Consumers")
doc = declareTitlesStyles(doc,
  stylenames = c("Titre1", "Titre2", "Titre3", "Titre4", "TitleDoc", "Normal"))
# title
doc <- addTitle(doc, value = "New CMH Core Consumers", level = 5)
doc <- addTOC(doc)
doc <- addPageBreak(doc)

# Adm/Disc/Active consumers ---------------------------------------------------
doc <- addTitle(doc,
  value = "Admissions, Discharges, & Active Consumers", level = 3)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$adm_status), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$adm_status))
doc <- addPageBreak(doc)
# Locus 3+6 month -------------------------------------------------------------
doc <- addTitle(doc,
  value = "Services for New CMH Core Consumers", level = 3)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc_3mon), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$loc_3mon))
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc_6mon), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$loc_6mon))
doc <- addPageBreak(doc)
# new core CMH admissions -----------------------------------------------------
doc <- addTitle(doc,
  value = "Services for New CMH Core Consumers", level = 3)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$new_adm), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$new_adm))
# save document ---------------------------------------------------------------
writeDoc(doc, file = file.path(project_wd$results,
  paste0("new CMH consumers", format(date_convert(input$end_dt),
                                     "%b_%d_%Y"), ".docx")))
p_msg("word document complete")
