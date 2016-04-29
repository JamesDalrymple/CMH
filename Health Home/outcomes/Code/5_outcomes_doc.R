doc <- new.env(parent = .GlobalEnv)
# document setup --------------------------------------------------------------
hh_doc <- docx(title = "Health Outcomes 1.0")
hh_doc = declareTitlesStyles(hh_doc,
  stylenames = c("Titre1", "Titre2", "Titre3", "Titre4", "TitleDoc", "Normal"))
# title
hh_doc <- addTitle(hh_doc, value = "Health Outcomes 1.0", level = 5)
hh_doc <- addTOC(hh_doc)
hh_doc <- addPageBreak(hh_doc)
# Graphs ----------------------------------------------------------------------
hh_doc <- addTitle(hh_doc, value = "HH vs CMH only", level = 3)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
                  fun = function() print(cmg$graphs$hh), width = 6.5, height = 8.5)
hh_doc <- addPageBreak(hh_doc)
hh_doc <- addTitle(hh_doc, value = "HH with Levels vs CMH only", level = 3)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
                  fun = function() print(cmg$graphs$hh_lev), width = 6.5, height = 8.5)
hh_doc <- addPageBreak(hh_doc)
hh_doc <- addTitle(hh_doc, value = "HH with Levels vs CMH only by Team", level = 3)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(cmg$graphs$hh_lev_cmh$MI), width = 6.5, height = 8.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(cmg$graphs$hh_lev_cmh$ACT), width = 6.5, height = 8.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(cmg$graphs$hh_lev_cmh$Child), width = 6.5, height = 8.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(cmg$graphs$hh_lev_cmh$`Child HB`), width = 6.5, height = 8.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(cmg$graphs$hh_lev_cmh$DD), width = 6.5, height = 8.5)
hh_doc <- addPlot(hh_doc, vector.graphic = TRUE,
  fun = function() print(cmg$graphs$hh_lev_cmh$Access), width = 6.5, height = 8.5)
hh_doc <- addPageBreak(hh_doc)
# save document ---------------------------------------------------------------
writeDoc(hh_doc, file = file.path(project_wd$results,
  paste0("outcomes run ", format(input$run_date, "%b_%d_%Y"), ".docx")))
p_msg("HH outcomes report complete")
