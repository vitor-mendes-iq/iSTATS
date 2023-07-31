
# a fazer: ler primeira linha da matriz de regions e colocar estas areas no menu select
regions_sel <<-c()

observeEvent(input$file2, {

  buma <<- 0

  if(is.null(input$file2))  return(NULL)

  up2 <<-input$file2
  print(up2)

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
    #file_names <<- file_names_full

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

    #updateTabsetPanel(session, "main_bar", "Plot Spectra")

  }

})

# regions_selected_input
observeEvent(input$file_regions, {
  data_path_regions <- input$file_regions
  dataset <- read.csv(data_path_regions$datapath, header = FALSE)
  reg_selec_old <<- dataset
  sel_ind <<- 0
  old_sel <<-  1
  
})

# import botton
observeEvent(input$import, {
  updateTabsetPanel(session, "main_bar", "Plot Spectra")
  if(old_sel == 1) {
    reg_selec <<- as.matrix(reg_selec_old)
    pos_map <<- matrix(data = NA,nrow=1,ncol = 2)
    matr_selec <<- matrix(data = NA,dim(NMRData)[1],ncol = 1) 
    for (i in 1:dim(reg_selec)[1]) {
      hlim <- which(abs(CS_values_real[1,]-reg_selec_old[i,1])==min(abs(CS_values_real[1,]-reg_selec_old[i,1])))
      llim <- which(abs(CS_values_real[1,]-reg_selec_old[i,2])==min(abs(CS_values_real[1,]-reg_selec_old[i,2])))
      col_select_old <<- c(col_select_old, seq(llim, hlim, 1))
      pos_low <- which(col_select_old==llim)
      pos_high <- which(col_select_old==hlim)
      pos_map <<- rbind(pos_map, matrix(c(pos_low,pos_high), 1, 2))
      matr_selec <<- cbind(matr_selec,rowSums(matrix(data = NMRData[,llim:hlim],dim(NMRData)[1], length(seq(llim, hlim, 1)))))
      sel_ind <<- sel_ind + 1
    }
    pos_map <<- pos_map[-c(1),]
    matr_selec <<- matr_selec[,-c(1)]
    col_select <<- col_select_old
    CS_selection$vranges <<- CS_values_real[1,col_select]
    old_sel <<- 0
  }
  else{
    col_select_old <<- c()
  }
})

# UI
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
                        # import seleted regions
                        fluidRow(align = "center",
                                 
                                 fluidRow(div(style="height:30px")),
                                 
                                 fileInput("file_regions", "Choose your selected regions/areas",
                                           multiple = TRUE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,
                                                       text/plain",".csv",".CSV")
                                 )),
                        # start import
                        fluidRow(align = "center",
                                 fluidRow(div(style="height:30px")),
                                 actionButton("import", "import data")
                        ),


                        fluidRow(div(style="height:30px")),

                        fluidRow( tags$b("IMPORTANT: the CSV files must contain only NMR chemical shift and intensity columns, respectively, separated by commas.")),


                        mainPanel(

                        )

               )

    )))

})
