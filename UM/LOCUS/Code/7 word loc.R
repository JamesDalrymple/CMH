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
  fun = function() print(graph$adm_status$all), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$adm_status$mi), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$adm_status$dd), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$adm_status$yf), width = 5.5, height = 6.0)
doc <- addParagraph(doc, "\nTable for Admission Status for Entire Agency")
doc <- addFlexTable(doc, FlexTable(agg$adm_status$all))
doc <- addParagraph(doc, "\nTable for Admission Status by Program")
doc <- addFlexTable(doc,
  FlexTable(agg$adm_status$prog[program %in% c("MI", "Y&F", "DD")]))
doc <- addPageBreak(doc)
# Locus 3+6 month -------------------------------------------------------------
doc <- addTitle(doc,
  value = "Services for New CMH Core Consumers", level = 3)
# 3 months LOCUS
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc$`MI3 2016`), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc$`MI3 2015`), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc$`WCCMH3 2016`), width = 5.5, height = 6.0)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc$`WCCMH3 2015`), width = 5.5, height = 6.0)
# doc <- addPlot(doc, vector.graphic = TRUE,
#   fun = function() print(graph$loc_3mon$access), width = 4.25, height = 6.0)
doc <- addParagraph(doc, "\nTable for three month service array for new consumers with a LOCUS 2 or less for entire agency")
doc <- addFlexTable(doc, FlexTable(agg$loc_3mon$all))
doc <- addParagraph(doc, "\nTable for three month service array for new consumers wiuth a LOCUS 2 or less by program")
doc <- addFlexTable(doc, FlexTable(agg$loc_3mon$prog[program %in% c("MI")]))
doc <- addPageBreak(doc)
# 6 months LOCUS
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc$`MI6 2016`), width = 5.25, height = 6.5)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc$`MI6 2015`), width = 5.25, height = 6.5)
doc <- addPlot(doc, vector.graphic = TRUE,
  fun = function() print(graph$loc$`WCCMH6 2016`), width = 5.5, height = 6.5)
doc <- addPlot(doc, vector.graphic = TRUE,
               fun = function() print(graph$loc$`WCCMH6 2015`), width = 5.5, height = 6.5)
# doc <- addPlot(doc, vector.graphic = TRUE,
#  fun = function() print(graph$loc_6mon$access), width = 4.25, height = 6.5)
doc <- addParagraph(doc, "\nTable for six month service array for new consumers wiuth a LOCUS 2 or less for Entire Agency")
doc <- addFlexTable(doc, FlexTable(agg$loc_6mon$all))
doc <- addParagraph(doc, "\nTable for six month service array for new consumers wiuth a LOCUS 2 or less by program")
doc <- addFlexTable(doc, FlexTable(agg$loc_6mon$prog[program %in% c("MI")]))
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
# doc <- addPlot(doc, vector.graphic = TRUE,
#   fun = function() print(graph$new_adm$access), width = 5.5, height = 6.0)
doc <- addParagraph(doc, "\nTable for New Admissions for Entire Agency")
doc <- addFlexTable(doc, FlexTable(agg$new_adm$all))
doc <- addParagraph(doc, "\nTable for New Admissions by Program")
doc <- addFlexTable(doc, FlexTable(agg$new_adm$prog[program %in% c("MI", "DD", "Y&F")]))
# save document ---------------------------------------------------------------
writeDoc(doc, file = file.path(project_wd$results,
  paste0("new CMH consumers 3yrs", format(date_convert(input$end_dt),
                                     "%b_%d_%Y"), ".docx")))
p_msg("word document complete")