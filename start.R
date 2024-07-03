phyloseq_app <- shiny::runApp(appDir = getwd(),
                               #launch.browser = FALSE,
                               port = 3838,
                               host = "0.0.0.0")
phyloseq_app
