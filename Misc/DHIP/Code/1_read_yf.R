# read in files
cafas <- read.csv(file.path(project_wd$data_wd, input$cafas_file),
                  stringsAsFactors = FALSE)
cafas <- data.table(cafas)
pecfas <- read.csv(file.path(project_wd$data_wd, input$pecfas_file),
                   stringsAsFactors = FALSE)
pecfas <- data.table(pecfas)
