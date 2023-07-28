# PAREI NA PARTE DE COLOCAR NOVAS SELECÕES ONDE JA TEM AS SELECIONADAS

 # Variables
  NMRData_Mean <- colMeans(NMRData_plot[,])
  col_select <<- c()
  alr_click <<- 0
  sel_ind <<- 0
  exran <<- c()
  exp_click <<- 0
  ysup <- max(NMRData_plot[,])
  col_select_old <- c()


  ############# olhar yinf ###################################
  # yinf <- ysup*-0.03
  yinf <<- -100
  ysup <- ysup + ysup*0.03
  cor_cutoff_p <- 0.9
  cor_cutoff_n <- -0.9
  chkzoom <<- 1
  idb <<- 0
  p_value <- 0.05
  s_stoc <<- 0

  # Data.frame
  testy <<- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[1,])
  #CS_selection <<- reactiveValues(vranges = c(-13131313,-131313))
  CS_selection <<- reactiveValues(vranges_t = c(-13131313,-131313))
  ranges <- reactiveValues(x = c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift))), y = c(yinf,ysup))
  ranges_sel <- reactiveValues(x = c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift))), y = c(yinf,ysup))
  spectrums <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_Mean[]))
  spectrums_sel <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum= NMRData_Mean[]))

  # Radio button plot type
  observeEvent(input$radio_s, {
    value <<- (input$radio_s)

    # Plot spectra average
    if (value == 1){

     # Plot left
      output$plot1 <- renderPlot({
        ggplot2::ggplot(spectrums$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') + ggplot2::scale_x_reverse() +
          ggplot2::coord_cartesian(xlim = c(ranges$x[2],ranges$x[1]), ylim = ranges$y, expand = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                        axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                        title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                        axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
          ) +
        ggplot2::labs(x = "Chemical Shift", y = "Intensity") +
        ggplot2::geom_vline(xintercept=CS_selection$vranges_t, color = "red", size = 0.1, alpha=0.01)
  })

  # Plot right
      output$plot2 <- renderPlot({
        ggplot2::ggplot(spectrums_sel$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') +
          ggplot2::coord_cartesian(xlim = c(ranges_sel$x[2],ranges_sel$x[1]), ylim = ranges_sel$y, expand = FALSE) + ggplot2::scale_x_reverse() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                        axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                        title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                        axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
        ) +
        ggplot2::labs(x = "Chemical Shift", y = "Intensity")
      })
    }

    # Plot all samples
    if (value == 2){

      # Plot left
      output$plot1 <- renderPlot({
        p <- ggplot2::ggplot() + ggplot2::scale_x_reverse() +
          ggplot2::coord_cartesian(xlim =  c(ranges$x[2],ranges$x[1]), ylim = ranges$y, expand = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                        axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                        title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                        axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
          ) +
          ggplot2::labs(x = "Chemical Shift", y = "Intensity") +
          ggplot2::geom_vline(xintercept=CS_selection$vranges, color = "red", size = 0.1, alpha=0.01)

        for (i in 1:length(file_names)) {
          spectrums_multi <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[i,])
          p <- p + ggplot2::geom_line(data=spectrums_multi, mapping=ggplot2::aes(x=Chemical_Shift,y=Spectrum),color='blue')
        }
        p
      })

  # Plot right
      output$plot2 <- renderPlot({
        k <- ggplot2::ggplot()  + ggplot2::scale_x_reverse() +
          ggplot2::coord_cartesian(xlim =  c(ranges_sel$x[2],ranges_sel$x[1]), ylim = ranges_sel$y, expand = FALSE) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                        axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                        title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                        axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
          ) +
          ggplot2::labs(x = "Chemical Shift", y = "Intensity")

        for (i in 1:length(file_names)) {
          spectrums_sel <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[i,])
          k <- k + ggplot2::geom_line(data=spectrums_sel, mapping=ggplot2::aes(x=Chemical_Shift,y=Spectrum),color='blue')
        }
        k
      })
    }
  })

  # Brush
  observeEvent(input$plot_brush,{
    brush <- input$plot_brush

    if (!is.null(brush)) {
      ranges_sel$x <- c(brush$xmin, brush$xmax)
      ranges_sel$y <- c(brush$ymin, brush$ymax)
      spectrums_sel$dat$Spectrum <- testy$Spectrum*chkzoom
      peran_sel <<- (ranges_sel$x[2] - ranges_sel$x[1])*0.2
    }

    else {
      ranges$x <- c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift)))
    }
  })

  # Brush select signal
  observeEvent(input$sel_brush,{

  })

  # x2
  observeEvent(input$x2_sel1, {
    chkzoom <<- chkzoom*2
    spectrums$dat$Spectrum <- spectrums$dat$Spectrum*2
    })

  # x8
  observeEvent(input$x8_sel1, {
    chkzoom <<- chkzoom*8
    spectrums$dat$Spectrum <- spectrums$dat$Spectrum*8
  })

  # /2
  observeEvent(input$q2_sel1, {
    chkzoom <<- chkzoom/2
    spectrums$dat$Spectrum <- spectrums$dat$Spectrum/2
  })

  # /8
  observeEvent(input$q8_sel1, {
    chkzoom <<- chkzoom/8
    spectrums$dat$Spectrum <- spectrums$dat$Spectrum/8
  })
  
  # Download point selected to save coluns select
  output$downloadpoints_save <- downloadHandler(
    filename = function() {
      paste("Selected_Regions_saved", ".csv", sep = "")
    },
    content = function(file) {
      write.table(reg_selec,file, sep = ",",col.names = FALSE,row.names = FALSE)
    }
  )
  
  

  # Download point selected
  output$downloadpoints <- downloadHandler(
    filename = function() {
        paste("Selected_Regions", ".csv", sep = "")
      },
      content = function(file) {
        col_select <<- col_select[order(col_select)]
        CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
        matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
        colnames(matr_cor) <<- CS_sel_real
        write.table(matr_cor, file,sep = ",",col.names = TRUE,row.names = FALSE)
        write.table(col_select,file, sep = ",",col.names = FALSE,row.names = FALSE)
      }
  )


  # Download bucket selected
  output$downloadareas <- downloadHandler(
    filename = function() {
      paste("Selected_Regions", ".csv", sep = "")},
        content = function(file) {

          if (length(reg_selec) > 2) {
          matrarea <- matr_selec[,rank(reg_selec[,1])]
          regelec <- round(reg_selec[rank(reg_selec[,1]),],2)
          nambu <- paste(as.character(regelec[,1]),as.character(regelec[,2]),sep = "-")
          colnames(matrarea) <- nambu
          write.table(matrarea, file,sep = ",",col.names = TRUE,row.names = FALSE)
          }

          else {
            showModal(modalDialog(
              title = "Warning!!!",
              "Please selected more than one region to download this area",
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }
        }
  )

  observeEvent(input$rsh_sel1, {
    xsup <- max(CS_values_real[1,])
    xinf <- min(CS_values_real[1,])
    ranges$x <- c(xinf,xsup)
    idb <<- 0
    })


  # Show all
  observeEvent(input$all_h, {
    ysup <- max (NMRData_Mean[])
    yinf <- ysup*-0.03
    ysup <- ysup + ysup*0.03
    ranges$y <- c(yinf,ysup)
    spectrums$dat$Spectrum <- spectrums$dat$Spectrum/chkzoom
    chkzoom <<- 1
    idb <<- 0
  })

  # Button sel
  observeEvent(input$exp_sel1, {
    exp_click <<- exp_click + 1
    exran[exp_click] <<- abs(ranges$x[2] - ranges$x[1])*0.2
    ranges$x <<- c((ranges$x[1] + exran[exp_click] ), (ranges$x[2] - exran[exp_click]))
  })

  # Paint selected region (red)
  observeEvent(input$cont_sel1, {

    if ((ranges$x[1] <= min(testy$Chemical_Shift)) || (ranges$x[2] >= max(testy$Chemical_Shift))) {}

    else {

      if ((exp_click - 1) <= 0) {
        ranges$x <- c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift)))
      }

      else {
      exp_click <<- exp_click - 1
      ranges$x <<- c((ranges$x[1] - exran[exp_click]), (ranges$x[2] + exran[exp_click]))
      }
    }

  })


  # Move left
  observeEvent(input$s_left_sel1, {

    if (!(ranges$x[1] <= min(testy$Chemical_Shift))) {
      ranges$x[1] <<- (ranges$x[1] - exran[exp_click] )
      ranges$x[2] <<- (ranges$x[2] - exran[exp_click] )
    }
  })


  # Move right
  observeEvent(input$s_right_sel1, {

    if (!(ranges$x[2] >= max(testy$Chemical_Shift))) {
      ranges$x[1] <<- (ranges$x[1] + exran[exp_click] )
      ranges$x[2] <<- (ranges$x[2] + exran[exp_click] )
    }
  })


  # Move full left
  observeEvent(input$s_left_f_sel1, {

    if (exp_click > 0 && ranges$x[1] > min(testy$Chemical_Shift)) {
      das <<- abs(ranges$x[1] - ranges$x[2])
      ranges$x[1] <- min(testy$Chemical_Shift)
      ranges$x[2] <- (ranges$x[1] + das)
    }

    else {

    }
  })


  # Move full right
  observeEvent(input$s_right_f_sel1, {

    if (exp_click > 0 && ranges$x[2] < max(testy$Chemical_Shift)) {
      das <<- abs(ranges$x[1] - ranges$x[2])
      ranges$x[2] = max(testy$Chemical_Shift)
      ranges$x[1] <- (ranges$x[2] - das)
    }

    else {

    }
  })

  # Start STOCSY-I
  observeEvent(input$s_stocsy, {


      if (!(sel_ind == 0)) {
        col_select <<- col_select[order(col_select)]
        CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
        matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
        cor_pearson <<- cor(matr_cor[,])
        drv_pk <<- which.max(matr_cor[1,])
        rr <<- vector(mode="character")

        norm_test(p_value,matr_cor)
        r_critical(p_value)

        updateSliderInput(session, "cutoff_stocsy_i", min = -1,
                          max = 1, value = c(-0.9,0.9) , step= 0.01)

        for (k in 1:dim(NMRData)[2]) {

          if (k %in% col_select) {
            z <<- which(col_select[] == k)

            if (cor_pearson[drv_pk,z] >= cor_cutoff_p) { #trocar linha com coluna
              rr[k] <<- "A"
            }

            else {
              rr[k] <<- "B"
            }

            if (cor_pearson[drv_pk,z] <= cor_cutoff_n) {
              rr[k] <<- "C"
            }
          }

          else {
              rr[k] <<- "B"
          }
        }
        cor_spearman <<- cor(matr_cor[,], method = "spearman")
        facts$fac_stocsy_i <<- rr[]
        facts_is$fac_stocsy_is <<- rr[]
        facts_rt$fac_stocsy_rt <<- rr[]
        updateTabsetPanel(session, "main_bar", "STOCSY-I")
      }

      else {
        showModal(modalDialog(
          title = "Warning!!!",
          "No region was selected. You must first select the desired region(s) before to start STOSCY analysis!",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
        ))
      }

    stop_menssager <- 1
  })
  
  # plot regions selected
  observeEvent(input$region_selected,{
    
    
  })
  

  # Select regions
  observeEvent(input$sel_cor, {
      brush <- input$sel_brush
      if(old_sel == 1) {
        old_sel <<- 0
        reg_selec <<- as.matrix(reg_selec_old)
        pos_map <<- matrix(data = NA,nrow=1,ncol = 2)
        for (i in 1:dim(reg_selec)[1]) {
          print(i)
          hlim <- which(abs(CS_values_real[1,]-reg_selec_old[i,1])==min(abs(CS_values_real[1,]-reg_selec_old[i,1])))
          llim <- which(abs(CS_values_real[1,]-reg_selec_old[i,2])==min(abs(CS_values_real[1,]-reg_selec_old[i,2])))
          col_select_old <<- c(col_select_old, seq(llim, hlim, 1))
          pos_low <- which(col_select_old==llim)
          pos_high <- which(col_select_old==hlim)
          pos_map <<- rbind(pos_map, matrix(c(pos_low,pos_high), 1, 2))
        }
        pos_map <<- pos_map[-c(1),]
        col_select <<- col_select_old
        CS_selection$vranges_t <<- CS_values_real[1,col_select]
        print(pos_map)
        matr_selec <<- rowSums(matrix(data = NMRData[,llim:hlim],dim(NMRData)[1], length(CS_selection$vranges_t)))
        
      }
      else{
        col_select_old <<- c()
      }

      if (!is.null(brush)) {

        if (sel_ind == 0) {
          sel_ind <<- 1
          CS_selection$vranges_t <<- c()
          hlim <- which(abs(CS_values_real[1,]-brush$xmin)==min(abs(CS_values_real[1,]-brush$xmin)))
          llim <- which(abs(CS_values_real[1,]-brush$xmax)==min(abs(CS_values_real[1,]-brush$xmax)))
          col_select <<- c(seq(llim, hlim, 1),col_select_old)
          #print(col_select)
          CS_selection$vranges_t <<- CS_values_real[1,col_select]
          matr_selec <<- rowSums(matrix(data = NMRData[,llim:hlim],dim(NMRData)[1], length(CS_selection$vranges_t)))
          reg_selec <<- matrix(data = c(brush$xmin,brush$xmax), 1, 2)
          pos_map <<- matrix(c(1,length(col_select)), 1, 2)
          
        }

        else {
          sel_ind <<- sel_ind + 1
          s_ind <<- 0
          
          if (sel_ind == 2) {

            if (((brush$xmin >= reg_selec[1,1]) && (brush$xmin <= reg_selec[1,2])) || ((brush$xmax >= reg_selec[1,1]) && (brush$xmax <= reg_selec[1,2])) || ((reg_selec[1,1] >= brush$xmin) && (reg_selec[1,1] <= brush$xmax)) || ((reg_selec[1,2] >= brush$xmin) && (reg_selec[1,2] <= brush$xmax))  ) {
            s_ind <<- 1
            sel_ind <<- sel_ind - 1
            }
          }

          else if (sel_ind > 2) {
            ind_gol <- 0
            
            for (i in 1:dim(reg_selec)[1]) {
              if (((brush$xmin >= reg_selec[i,1]) && (brush$xmin <= reg_selec[i,2])) || ((brush$xmax >= reg_selec[i,1]) && (brush$xmax <= reg_selec[i,2])) || ((reg_selec[i,1] >= brush$xmin) && (reg_selec[i,1] <= brush$xmax)) || ((reg_selec[i,2] >= brush$xmin) && (reg_selec[i,2] <= brush$xmax))  ) {
                ind_gol <- 1
              }
            }

            if (ind_gol == 1) {
              s_ind <<- 1
              sel_ind <<- sel_ind - 1
            }
          }
          
          if (s_ind == 0) {
            print(sel_ind)
            hlim <- which(abs(CS_values_real[1,]-brush$xmin)==min(abs(CS_values_real[1,]-brush$xmin)))
            llim <- which(abs(CS_values_real[1,]-brush$xmax)==min(abs(CS_values_real[1,]-brush$xmax)))
            col_select_2 <<- seq(llim, hlim, 1)
            col_select <<- c(col_select, col_select_2)
            reg_selec <<- rbind(reg_selec, c(brush$xmin,brush$xmax))
            pos_low <- which(col_select==llim)
            pos_high <- which(col_select==hlim)
            pos_map <<- rbind(pos_map, matrix(c(pos_low,pos_high), 1, 2))
            #beg_ind <- pos_map[(dim(reg_selec)[1]),2]
            #act_map <- c((beg_ind+1),(beg_ind+length(col_select_2)))
            #pos_map <<- rbind(pos_map,act_map)
            print(pos_map)
            dyn_brush <- c(brush$xmin,brush$xmax)
            #reg_selec <<- rbind(reg_selec, dyn_brush)
            print('reg_selec')
            print(reg_selec)
            CS_selection$vranges_t <<- c(CS_selection$vranges_t, CS_values_real[1,col_select])
            matr_selec <<- cbind(matr_selec,rowSums(matrix(data = NMRData[,llim:hlim],dim(NMRData)[1], length(col_select))))
          }

          else {
            showModal(modalDialog(
              title = "Warning!!!",
              "Part of the region you want to load was already loaded. Please, check the loaded regions and select a non superimposed one!!!",
              easyClose = TRUE,
              footer = modalButton("Close"),
              size = "l"
            ))
          }

        }
      }

      else {
        showModal(modalDialog(
          title = "Warning!!!",
          "No region was selected. You must first select the desired region before to click on the button!",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l"
          ))
      }
      
      

    observeEvent(input$sel_click, {
        if (alr_click == 1) {
         exc_val <- input$sel_click$x
         print(exc_val)

         if (dim(reg_selec)[1] == 1) {
            print('passei')
           if ((exc_val >= reg_selec[1,1]) && (exc_val <= reg_selec[1,2])) {
             reg_selec <<- matrix (0, 1, 2)
             col_select <<- c()
             CS_selection$vranges_t <<- c(-13131313,-131313)
             sel_ind <<- 0
           }
        }

         if (dim(reg_selec)[1] > 1) {
           row_reg <<- 0

           for (i in 1:dim(reg_selec)[1]) {

             if ((exc_val >= reg_selec[i,1]) && (exc_val <= reg_selec[i,2])) {
                row_reg <<- i

             }
           }

           if (row_reg != 0) {
             b_point <- pos_map[row_reg,1]
             e_point <- pos_map[row_reg,2]
             cut_all <- seq(b_point, e_point, 1)
             print(cut_all)

            if (sel_ind == 2) {
               reg_selec <<- matrix(reg_selec[-row_reg,], 1, 2)
             }

             else {
             reg_selec <<- reg_selec[-row_reg,]
             }
             col_select <<- col_select[-c(cut_all)]
             print(length(CS_selection$vranges_t))
             CS_selection$vranges_t <<- CS_selection$vranges_t[-c(cut_all)]
             print(length(CS_selection$vranges_t))  
             if (row_reg == 1) {
             pos_delta <<- as.integer(pos_map[1,2] - pos_map[1,1] + 1)

             if (sel_ind == 2) {
               pos_map_temp <<- matrix(pos_map[-1,], 1, 2)
             }

             else {
               pos_map_temp <<- pos_map[-1,]
             }
             pos_map <<- (pos_map_temp - pos_delta)
             }

             if (row_reg == 2) {
               pos_delta <- (pos_map[2,2] - pos_map[2,1] + 1)
               pos_map_temp <- pos_map[-c(1,2),] - pos_delta
               pos_map <<- rbind(pos_map, pos_map_temp)
             }

             if ((row_reg > 2) && (row_reg < dim(pos_map)[1])) {
               pos_delta <- (pos_map[row_reg,2] - pos_map[row_reg,1] + 1)
               pos_map_temp <- pos_map[-c(1:row_reg),] - pos_delta
               pos_map <<- rbind(pos_map[1:(row_reg-1),], pos_map_temp)
             }

             if (row_reg == dim(pos_map)[1]) {
               pos_map <<- pos_map[-dim(pos_map)[1],]
             }
             sel_ind <<- sel_ind - 1
           }
         alr_click <<- 0
        }
      }
    })

    # Delete region
    observeEvent(input$exc_reg, {
        if (!(sel_ind == 0)) {
        alr_click <<- 1
        }

        else {
          showModal(modalDialog(
            title = "Warning!!!",
            "No region was loaded, so exclusion it is not possible!",
            easyClose = TRUE,
            footer = modalButton("Close"),
            size = "l"
          ))
        }
    })
  })

  observeEvent(input$plot_dblclick, {
    ranges_sel$x <- c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift)))
    ranges$x <- c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift)))
    ysup <- max(testy$Spectrum)
    yinf <- ysup*-0.03
    ysup <- ysup + ysup*0.03
    ranges$y <- c(yinf,ysup)
    ranges_sel$y <- c(yinf,ysup)
    spectrums$dat$Spectrum <- testy$Spectrum
    spectrums_sel$dat$Spectrum <- testy$Spectrum
    chkzoom <<- 1
    idb <<- 0
    peran <<- 0
    })
