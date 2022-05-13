
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


    load(up3$datapath, envir = .GlobalEnv)

    NMRData_plot <<- NMRData




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

                        fluidRow( tags$b("IMPORTANT: the RData files must contain only NMRData (matrix with intensity) CS_Values_Real (matrix of chemical shift) and file_names (list of sample names).")),


                        mainPanel(

                        )

               )

    )))

})
