
observeEvent(input$file3, {
  
  buma <<- 0
  
  if(is.null(input$file3))  return(NULL)
  
  up3 <<-input$file3
  if (length(up3$name) < 1) {
    
    showModal(modalDialog(
      title = "Warning!!!",
      "Please select file before to continue!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
    
    buma <<- 1
    
    
  }
  
  
  else {
    upfile3 <<-list()
    
    # Upload of samples
    
    tmp_e <- new.env()
    load(up3$datapath, tmp_e)
    
    NMRData_plot <<- tmp_e[["nmrdata"]]
    NMRData <<- tmp_e[["nmrdata"]]
    CS_values_real <<- tmp_e[["cs_values_real"]]
    file_names <<- tmp_e[["file_names"]]
    
    if ( min(NMRData) < 0 ) {
      
      NMRData <<- NMRData + (abs(min(NMRData))+1)
      
    }
    #
    
    refreshval()
    
    updateSelectInput(session, "spectrum_list_multi", choices = file_names[])
    
    updateSelectInput(session, "spectrum_list_stocsy_i", choices = file_names[])
    
    updateSelectInput(session, "spectrum_list_stocsy_is", choices = file_names[])
    
    updateSelectInput(session, "spectrum_list_stocsy_rt", choices = file_names[])
    
    updateTabsetPanel(session, "main_bar", "Plot Spectra")
    
  }
  
})



output$imputa3 <- renderUI({
  
  tagList(
    
    (fluidPage(theme = 'zoo.css',
               
               fluidRow(align = "center",
                        
                        fluidRow(div(style="height:30px")),
                        
                        fileInput("file3", "Choose RData Files",
                                  multiple = F,
                                  accept = c(".Rdata")
                        ),
                        
                        fluidRow(div(style="height:30px")),
                        
                        fluidRow( tags$b("IMPORTANT: the RData files must contain only nmrdata (matrix with intensity) cs_values_real (matrix of chemical shift) and file_names (list of sample names) all variables names need to be in low case.")),
                        
                        
                        mainPanel(
                          
                        )
                        
               )
               
    )))
  
})
