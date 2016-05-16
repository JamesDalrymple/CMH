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
# 3 months LOCUS
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc_3mon$all), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc_3mon$mi), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc_3mon$dd), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc_3mon$access), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$loc_3mon$all))
doc <- addFlexTable(doc, FlexTable(agg$loc_3mon$prog))
doc <- addPageBreak(doc)
# 6 months LOCUS
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc_6mon$all), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc_6mon$mi), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc_6mon$dd), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc_6mon$access), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$loc_6mon$all))
doc <- addFlexTable(doc, FlexTable(agg$loc_6mon$prog))
doc <- addPageBreak(doc)
# new core CMH admissions -----------------------------------------------------
doc <- addTitle(doc,
  value = "Services for New CMH Core Consumers", level = 3)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$new_adm$all), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$new_adm$mi), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$new_adm$dd), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$new_adm$yf), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$new_adm$access), width = 5.5, height = 6.0)
doc <- addFlexTable(doc, FlexTable(agg$new_adm$all))
doc <- addFlexTable(doc, FlexTable(agg$new_adm$prog))
# save document ---------------------------------------------------------------
writeDoc(doc, file = file.path(project_wd$results,
  paste0("new CMH consumers", format(date_convert(input$end_dt),
                                     "%b_%d_%Y"), ".docx")))
p_msg("word document complete")