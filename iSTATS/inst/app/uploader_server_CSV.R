
observeEvent(input$file2, {

  buma <<- 0

  if(is.null(input$file2))  return(NULL)

  up2 <<-input$file2

  if (length(up2$name) < 2) {

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

    upfile1 <<-list()

    # Upload of samples

    for(l in 1:length(up2$name)) {

      upfile1[[l]] <<- data.table::fread(up2$datapath[l],
                            header = FALSE, sep = ",",data.table=FALSE)

    }


    lslim = as.numeric(input$ls_lims)
    hslim = as.numeric(input$hs_lims)

    file_names_full <<- up2$name

    file_names <<- substr(basename(file_names_full),1, nchar(basename(file_names_full))-4)

    file_names_full <<- gtools::mixedsort(file_names_full)

    file_names <<- gtools::mixedsort(file_names)

    # upfile1 <<- lapply(upfile1, function(k) if(anyNA(k)) k[-1,c(4,2)] else k[,c(4,2)])

    list_len <- length(upfile1)

    CS_values <<- unlist((upfile1[[1]][1]),use.names = FALSE, recursive = FALSE)

    NMRData_temp <<- t(lapply(upfile1, function(k) k[,2]))

    hspoint <- which(abs(CS_values-lslim)==min(abs(CS_values-lslim)))[1]

    lspoint <- which(abs(CS_values-hslim)==min(abs(CS_values-hslim)))[1]

    npf = hspoint - lspoint

    np = npf + 1

    CS_values_temp <- lapply(upfile1, function(k) k[,1])

    hspoint <- sapply(CS_values_temp, function (v) which(abs(v-lslim)==min(abs(v-lslim)))[1])

    CS_ind <- which.min(abs(mapply(function(x,y) x[hspoint[y]],CS_values_temp, 1:list_len)))

    CS_values <<- CS_values_temp[[CS_ind]][(hspoint[CS_ind]-npf):hspoint[CS_ind]]

    NMRData <<- t(mapply(function(x,y) x[(hspoint[y]-npf):hspoint[y]],NMRData_temp, 1:list_len))

    CS_values_real <<- rbind(CS_values,CS_values)

    NMRData_plot <<- NMRData

    NMRData <<- NMRData + abs(min(NMRData))


    refreshval()

    updateSelectInput(session, "spectrum_list_multi", choices = file_names[])

    updateSelectInput(session, "spectrum_list_stocsy_i", choices = file_names[])

    updateSelectInput(session, "spectrum_list_stocsy_is", choices = file_names[])

    updateSelectInput(session, "spectrum_list_stocsy_rt", choices = file_names[])

    updateTabsetPanel(session, "main_bar", "Plot Spectra")

  }

})


# observeEvent(input$import1,{
#
#   X <- rstudioapi::selectDirectory(caption = "Select the directory that contains the CSV files")
#
#   if(!is.null(X)) {
#
#     setwd(X)
#
#     ext="\\.(csv|CSV)$"
#
#     FilesX <- list.files(pattern = ext)
#
#     if(length(FilesX)>2) {
#
#       upfile1 <<- lapply(FilesX, function(x) data.table::fread(x, header = FALSE, sep = ",",data.table=FALSE) )
#
#       lslim = as.numeric(input$ls_lims)
#
#       hslim = as.numeric(input$hs_lims)
#
#       file_names_full <<- FilesX
#
#       file_names <<- substr(basename(file_names_full),1, nchar(basename(file_names_full))-4)
#
#       file_names_full <<- gtools::mixedsort(file_names_full)
#
#       file_names <<- gtools::mixedsort(file_names)
#
#       list_len <- length(upfile1)
#
#       CS_values <<- unlist((upfile1[[1]][1]),use.names = FALSE, recursive = FALSE)
#
#       NMRData_temp <<- t(lapply(upfile1, function(k) k[,2]))
#
#       hspoint <- which(abs(CS_values-lslim)==min(abs(CS_values-lslim)))[1]
#
#       lspoint <- which(abs(CS_values-hslim)==min(abs(CS_values-hslim)))[1]
#
#       npf = hspoint - lspoint
#
#       np = npf + 1
#
#       CS_values_temp <- lapply(upfile1, function(k) k[,1])
#
#       hspoint <- sapply(CS_values_temp, function (v) which(abs(v-lslim)==min(abs(v-lslim)))[1])
#
#       CS_ind <- which.min(abs(mapply(function(x,y) x[hspoint[y]],CS_values_temp, 1:list_len)))
#
#       CS_values <<- CS_values_temp[[CS_ind]][(hspoint[CS_ind]-npf):hspoint[CS_ind]]
#
#       NMRData <<- t(mapply(function(x,y) x[(hspoint[y]-npf):hspoint[y]],NMRData_temp, 1:list_len))
#
#       CS_values_real <<- rbind(CS_values,CS_values)
#
#       NMRData <<- NMRData + abs(min(NMRData))
#
#
#       refreshval()
#
#       updateSelectInput(session, "spectrum_list_multi", choices = file_names[])
#
#       updateSelectInput(session, "spectrum_list_stocsy_i", choices = file_names[])
#
#       updateSelectInput(session, "spectrum_list_stocsy_is", choices = file_names[])
#
#       updateSelectInput(session, "spectrum_list_stocsy_rt", choices = file_names[])
#
#       updateTabsetPanel(session, "main_bar", "Plot Spectra")
#
#     }
#
#   }
#
# }  )


output$imputa2 <- renderUI({

  tagList(

    (fluidPage(theme = 'zoo.css',

               fluidRow(align = "center",

                        fluidRow(div(style="height:30px")),

                        fileInput("file2", "Choose CSV Files",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,
                    text/plain",".csv",".CSV")
                        ),

                        # tags$button(id = "import1",
                        #             class = "btn action-button btn_lmax",
                        #             tags$img(src = "csv_i.png",
                        #                      height = "50px", width = "50px")
                        # ),

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

                        fluidRow( tags$b("IMPORTANT: the CSV files must contain only NMR chemical shift and intensity columns, respectively, separated by commas.")),


                        mainPanel(

                        )

               )

    )))

})
