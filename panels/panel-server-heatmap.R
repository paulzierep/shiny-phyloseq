################################################################################
# UI
################################################################################
output$heat_sample_label <- renderUI({
  uivar("sample.label", "Sample", vars("samples"))
})
output$heat_taxa_label <- renderUI({
  uivar("taxa.label", "Taxa", vars("taxa"))
})
output$heat_sample_order <- renderUI({
  uivar("sample.order", "Samples", vars("samples"))
})
output$heat_taxa_order <- renderUI({
  uivar("taxa.order", "Taxa", vars("taxa"))
})
################################################################################
# heatmap plot definition
################################################################################
physeq_heat = reactive({
  return(
    switch({input$transform_heat},
           Counts = physeq(),
           Prop = physeqProp(),
           RLog = physeqRLog(),
           CLR = physeqCLR(),
           physeq()
    )
  )
}) 
make_heatmap = reactive({
  if(input$actionb_heat < 1){
    return(NULL)
  }
  p3 = NULL
  isolate({
    try(p3 <- plot_heatmap(physeq_heat(), method=input$ord_method_heat, distance=input$dist_heat,
                           sample.label=av(input$sample.label),
                           taxa.label=av(input$taxa.label),
                           sample.order=av(input$sample.order),
                           taxa.order=av(input$taxa.order),
                           low = input$locolor_heat,
                           high = input$hicolor_heat,
                           na.value = input$NAcolor_heat),
        silent=TRUE)
  })
  return(p3)
})
# Render plot in panel and in downloadable file with format specified by user selection
output$heatmap <- renderPlot({
  shiny_phyloseq_print(make_heatmap())
}, width=function(){72*input$width_heat}, height=function(){72*input$height_heat})
output$download_heat <- downloadHandler(
  filename = function(){paste0("Heatmap_", simpletime(), ".", input$downtype_heat)},
  content = function(file){
    ggsave2(filename=file,
            plot=make_heatmap(),
            device=input$downtype_heat,
            width=input$width_heat, height=input$height_heat, dpi=300L, units="in")
  }
)

save_and_upload_plot <- function(suffix, plot_func) {
  observeEvent(input[[paste0("store_", suffix)]], {
    # Define the output file path and name based on the suffix
    output_file <- file.path(Sys.getenv('SHINY_OUTPUT_DIR'), 
                             paste0(ucfirst(suffix), "_", simpletime(), ".", input[[paste0("downtype_", suffix)]]))

    # Use ggsave2 to save the plot, using the suffix to determine input values
    ggsave2(output_file,
            plot = plot_func(),  # dynamic plot function based on suffix
            device = input[[paste0("downtype_", suffix)]],
            width = input[[paste0("width_", suffix)]], 
            height = input[[paste0("height_", suffix)]], 
            dpi = 300L, 
            units = "in")

    # Run system command to upload the file
    system(paste('put -p', output_file))
  })
}

# Helper function to capitalize the suffix for use in filenames
ucfirst <- function(string) {
  paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))
}

save_and_upload_plot("heat", make_heatmap())

# observeEvent(input$store_heat, {
#       output_file <- file.path(Sys.getenv('SHINY_OUTPUT_DIR'), paste0("Heatmap_", simpletime(), ".", input$downtype_heat))

#       ggsave2(output_file,
#             plot=make_heatmap(),
#             device=input$downtype_heat,
#             width=input$width_heat, height=input$height_heat, dpi=300L, units="in")
#       system(paste('put -p ', output_file))
# })