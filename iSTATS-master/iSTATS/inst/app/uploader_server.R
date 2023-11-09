# Import and build matrix
observeEvent(input$file1, {
  buma <<- 0

  if(is.null(input$file1))  return(NULL)
  up <<-input$file1

  if (length(up$name) < 2) {
    showModal(modalDialog(
      title = "Warning!!!",
      "Please select two or more files before to continue!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
    buma <<- 1
  }

  else {
    upfile <<-list()

    for(l in 1:length(up$name)) {
      upfile[[l]] <<- data.table::fread(up$datapath[l],
                            header = FALSE, sep = ",",data.table=FALSE)
    }

    lslim = as.numeric(input$ls_lims)
    hslim = as.numeric(input$hs_lims)
    file_names_full <<- up$name
    file_names <<- substr(basename(file_names_full),1, nchar(basename(file_names_full))-4)
    file_names_full <<- gtools::mixedsort(file_names_full)
    file_names <<- gtools::mixedsort(file_names)
    upfile <<- lapply(upfile, function(k) if(anyNA(k)) k[-1,c(4,2)] else k[,c(4,2)])
    #upfile_ <<- lapply(upfile, function(k) if(anyNA(k)) k[-1,c(4,2)] else k[,c(4,2)])
    list_len <- length(upfile)
    CS_values <<- unlist((upfile[[1]][1]),use.names = FALSE, recursive = FALSE)
    print(upfile[[1]][2])
    NMRData_temp <<- t(mapply(upfile, function(k) k[,2]))
    hspoint <- which(abs(CS_values-lslim)==min(abs(CS_values-lslim)))[1]
    lspoint <- which(abs(CS_values-hslim)==min(abs(CS_values-hslim)))[1]
    npf = hspoint - lspoint
    np = npf + 1
    CS_values_temp <- lapply(upfile, function(k) k[,1])
    hspoint <- sapply(CS_values_temp, function (v) which(abs(v-lslim)==min(abs(v-lslim)))[1])
    CS_ind <- which.min(abs(mapply(function(x,y) x[hspoint[y]],CS_values_temp, 1:list_len)))
    CS_values <<- CS_values_temp[[CS_ind]][(hspoint[CS_ind]-npf):hspoint[CS_ind]]
    NMRData <<- t(mapply(function(x,y) x[(hspoint[y]-npf):hspoint[y]],NMRData_temp, 1:list_len))
    CS_values_real <<- rbind(CS_values,CS_values)
    NMRData_plot <<- NMRData
    NMRData <<- NMRData_temp + abs(min(NMRData_temp))
   
    refreshval()
    updateSelectInput(session, "spectrum_list_multi", choices = file_names[])
    updateSelectInput(session, "spectrum_list_stocsy_i", choices = file_names[])
    updateSelectInput(session, "spectrum_list_stocsy_is", choices = file_names[])
    updateSelectInput(session, "spectrum_list_stocsy_rt", choices = file_names[])
    updateTabsetPanel(session, "main_bar", "Plot Spectra")
  }
})

# GUI import
output$imputa <- renderUI({
  tagList(
    (fluidPage(theme = 'zoo.css',
               fluidRow(align = "center",
                        fluidRow(div(style="height:30px")),
                        fileInput("file1", "Choose ASC Bruker Files",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,
                    text/plain",".csv",".CSV")
                        ),
                        fluidRow(div(style="height:30px")),
                        fluidRow( tags$b("Choose a minimum and a maximum frequency (ppm) for the working spectra:")),
                        fluidRow(div(style="height:15px")),
                        fluidRow(column(5),
                                 column(1, align="center",
                                        textInput("ls_lims", "min","0.0",
                                                  width = "150px")
                                 ),

                                 column(1, align="center",
                                        textInput("hs_lims", "max","10.0", width = "150px")
                                 ),
                                 column(5)
                        ),
                        fluidRow(div(style="height:30px")),
                        fluidRow( tags$b("IMPORTANT: The ASC files should be export by Topspin (Bruker software) using command convbin2asc.")),
                        mainPanel(
                        )
               )
    )))
})
