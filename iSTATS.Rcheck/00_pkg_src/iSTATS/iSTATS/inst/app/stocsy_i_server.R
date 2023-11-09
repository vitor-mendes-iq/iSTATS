  #Variables
  ndpi <<- 72
  ncor <<-  1
  rr <<- c()
  rr2 <<- c()
  chkzoom_stocsy_i <<- 1
  idb_stocsy_i <<- 0
  peran_stocsy_i <<- 0
  ysup_stocsy_i <- max(NMRData_plot[1,])
  yinf_stocsy_i <- ysup_stocsy_i*-0.03
  ysup_stocsy_i <- ysup_stocsy_i + ysup_stocsy_i*0.03
  R_CRITICAL_n <- 0

  # Dataframe
  testy_stocsy_i <<- data.frame(Chemical_Shift=CS_values_real[1,])
  ranges_stocsy_i <- reactiveValues(x = c((min(testy_stocsy_i$Chemical_Shift)), max(testy_stocsy_i$Chemical_Shift)), y = c(yinf_stocsy_i,ysup_stocsy_i))
  spectrums_stocsy_i <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[1,])) #, facs = data.frame(fac_stocsy_i = rr[])
  facts <<- reactiveValues(fac_stocsy_i = c())

  #Plot
  output$plot_stocsy_i <- renderPlot({
    ggplot2::ggplot(spectrums_stocsy_i$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') +
      ggplot2::geom_line(ggplot2::aes(colour=facts$fac_stocsy_i, group=1)) + ggplot2::scale_colour_manual(values =c("red","blue","green")) +
      ggplot2::coord_cartesian(xlim = c(ranges_stocsy_i$x[2],ranges_stocsy_i$x[1]), ylim = ranges_stocsy_i$y, expand = FALSE) +
      ggplot2::scale_x_reverse() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                     axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                     title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                     axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15),
                     legend.position="none"
      ) +
      ggplot2::labs(x = "Chemical Shift", y = "Intensity")
    })

  # Brush Zoom
  observeEvent(input$plot_brush_stocsy_i,{
    brush <- input$plot_brush_stocsy_i

    if (!is.null(brush)) {
      ranges_stocsy_i$x <- c(brush$xmin, brush$xmax)
      ranges_stocsy_i$y <- c(brush$ymin, brush$ymax)
      idb_stocsy_i <<- 1
      peran_stocsy_i <<- (ranges_stocsy_i$x[2] - ranges_stocsy_i$x[1])*0.2
      }

      else {
        ranges_stocsy_i$x <- NULL
      }
    })


  # Intensity x2
  observeEvent(input$x2_stocsy_i, {
    tryton <<- facts$fac_stocsy_i
    chkzoom_stocsy_i <<- chkzoom_stocsy_i*2
    spectrums_stocsy_i$dat$Spectrum <- spectrums_stocsy_i$dat$Spectrum*2
  })


  # Intensity x8
  observeEvent(input$x8_stocsy_i, {
    chkzoom_stocsy_i <<- chkzoom_stocsy_i*8
    spectrums_stocsy_i$dat$Spectrum <- spectrums_stocsy_i$dat$Spectrum*8
  })


  # Intensity /2
  observeEvent(input$q2_stocsy_i, {
    chkzoom_stocsy_i <<- chkzoom_stocsy_i/2
    spectrums_stocsy_i$dat$Spectrum <- spectrums_stocsy_i$dat$Spectrum/2
  })


  # Intensity /8
  observeEvent(input$q8_stocsy_i, {
    chkzoom_stocsy_i <<- chkzoom_stocsy_i/8
    spectrums_stocsy_i$dat$Spectrum <- spectrums_stocsy_i$dat$Spectrum/8
  })


  # Show all
  observeEvent(input$all_stocsy_i, {
    ranges_stocsy_i$x <- c((min(testy_stocsy_i$Chemical_Shift)), max(testy_stocsy_i$Chemical_Shift))
    freshnum_stocsy_i <- which(file_names[] == input$spectrum_list_stocsy_i)
    ysup_stocsy_i <- max (NMRData_plot[freshnum_stocsy_i,])
    yinf_stocsy_i <- ysup_stocsy_i*-0.03
    ysup_stocsy_i <- ysup_stocsy_i + ysup_stocsy_i*0.03
    ranges_stocsy_i$y <- c(yinf_stocsy_i,ysup_stocsy_i)
    spectrums_stocsy_i$dat$Spectrum <- spectrums_stocsy_i$dat$Spectrum/chkzoom_stocsy_i
    chkzoom_stocsy_i <<- 1
    idb_stocsy_i <<- 0
  })


  # Move left
  observeEvent(input$s_left_stocsy_i, {
    if (!(ranges_stocsy_i$x[1] <= min(testy_stocsy_i$Chemical_Shift))) {
    ranges_stocsy_i$x[1] <<- (ranges_stocsy_i$x[1] - peran_stocsy_i)
    ranges_stocsy_i$x[2] <<- (ranges_stocsy_i$x[2] - peran_stocsy_i)
    }
  })


  # Move right
  observeEvent(input$s_right_stocsy_i, {
    if (!(ranges_stocsy_i$x[2] >= max(testy_stocsy_i$Chemical_Shift))) {
    ranges_stocsy_i$x[1] <<- (ranges_stocsy_i$x[1] + peran_stocsy_i)
    ranges_stocsy_i$x[2] <<- (ranges_stocsy_i$x[2] + peran_stocsy_i)
    }
  })


  # Move full left
  observeEvent(input$s_left_f_stocsy_i, {
    if (!(ranges_stocsy_i$x[1] <= min(testy_stocsy_i$Chemical_Shift))) {
    das_stocsy_i <<- (ranges_stocsy_i$x[2] - ranges_stocsy_i$x[1])
    ranges_stocsy_i$x[1] <<- min(testy_stocsy_i$Chemical_Shift)
    ranges_stocsy_i$x[2] <<- (min(testy_stocsy_i$Chemical_Shift) + das_stocsy_i)
    }
  })


  # Move full right
  observeEvent(input$s_right_f_stocsy_i, {
    if (!(ranges_stocsy_i$x[2] >= max(testy_stocsy_i$Chemical_Shift))) {
      das_stocsy_i <<- (ranges_stocsy_i$x[2] - ranges_stocsy_i$x[1])
      ranges_stocsy_i$x[2] <<- max(testy_stocsy_i$Chemical_Shift)
      ranges_stocsy_i$x[1] <<- (max(testy_stocsy_i$Chemical_Shift) - das_stocsy_i)
    }
  })


  ## Download plot


  # DPI
  observeEvent(input$slide_dpi, {

    n_dpi <<- input$slide_dpi

  })

  # Botton download plot
  output$plot_download <- downloadHandler(
    filename = function() {
      paste0('stocsy-i.',input$data_input)
    },
    content = function(file1) {
      ggplot2::ggsave(file1,width=295, device = input$data_input,height=205, units="mm", dpi = n_dpi)

    }
  )


  # List of samples for plot
  observeEvent(input$spectrum_list_stocsy_i,{
    freshnum_stocsy_i <- which(file_names[] == input$spectrum_list_stocsy_i)
    spectrums_stocsy_i$dat <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[freshnum_stocsy_i,])

    if (!idb_stocsy_i && chkzoom_stocsy_i == 1) {
    ysup_stocsy_i <- max (NMRData_plot[freshnum_stocsy_i,])
    yinf_stocsy_i <- ysup_stocsy_i*-0.03
    ysup_stocsy_i <- ysup_stocsy_i + ysup_stocsy_i*0.03
    ranges_stocsy_i$y <- c(yinf_stocsy_i, ysup_stocsy_i)
    }

    else {
        spectrums_stocsy_i$dat$Spectrum <- spectrums_stocsy_i$dat$Spectrum*chkzoom_stocsy_i
    }
  })


  # Slide bar cor-cutoff
  observeEvent(input$cutoff_stocsy_i, {
    cor_cutoff_i <<- input$cutoff_stocsy_i

    for (g in 1:length(cor_cutoff_i)) {

       if (cor_cutoff_i[g] > 0) {
        cor_cutoff_value_p <<- cor_cutoff_i[g]   #positive values
      }

      else {
        cor_cutoff_value_n <<- cor_cutoff_i[g]     #negative values
      }
    }

    for (k in 1:dim(NMRData)[2]) {

      if (k %in% col_select) {
        z <<- which(col_select[] == k)

        if (cor_pearson[drv_pk,z] >= cor_cutoff_value_p) {
          rr[k] <<- "A"
        }

        else {
          rr[k] <<- "B"
        }

        if (cor_pearson[drv_pk,z] <= cor_cutoff_value_n) {
          rr[k] <<- "C"
        }}

      else {
        rr[k] <<- "B"  #falta transformar 'rr' em variavel reativa
      }
    }
    facts$fac_stocsy_i <<- rr[]
  })


  # Radio Button choose type correlation.
  observeEvent(input$radio, {
    value <<- (input$radio)

    # Pearson
    if (value == 1){

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {
          z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= cor_cutoff_value_p) {
            rr[k] <<- "A"
          }

          else {
            rr[k] <<- "B"
          }

          if (cor_pearson[drv_pk,z] <= cor_cutoff_value_n) {
            rr[k] <<- "C"
          }}

        else {
          rr[k] <<- "B"  #falta transformar 'rr' em variavel reativa
        }
      }
      facts$fac_stocsy_i <<- rr[]
    }

    # Spearman
    if (value == 2) {

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {
          z <<- which(col_select[] == k)

          if (cor_spearman[drv_pk,z] >= cor_cutoff_value_p) {
            rr[k] <<- "A"
          }

          else {
            rr[k] <<- "B"
          }

          if (cor_spearman[drv_pk,z] <= cor_cutoff_value_n) {
            rr[k] <<- "C"
          }
        }

        else {
          rr[k] <<- "B"  #falta transformar 'rr' em variavel reativa
        }
      }
      facts$fac_stocsy_i <<- rr[]
    }

    # PearSpear
    if (value == 3) {

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {
          z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= cor_cutoff_value_p) {
            rr[k] <<- "A"
          }

          else {
            rr[k] <<- "B"
          }

          if (cor_pearson[drv_pk,z] <= cor_cutoff_value_n) {
            rr[k] <<- "C"
          }
        }

        else {
          rr[k] <<- "B"  #falta transformar 'rr' em variavel reativa
        }

          if (cor_spearman[drv_pk,z] >= cor_cutoff_value_p) {
            rr[k] <<- "A"
          }

          else {
            rr[k] <<- "B"
          }

          if (cor_spearman[drv_pk,z] <= cor_cutoff_value_n) {
            rr[k] <<- "C"
          }

        else {
          rr[k] <<- "B"  #falta transformar 'rr' em variavel reativa
        }
      }
      facts$fac_stocsy_i <<- rr[]
      }
  })


  # R critical by P-value
  observeEvent(input$pv, {
    p_value_n <- as.numeric(input$pv)
    r_critical(p_value_n)
    output$r_critical <- renderText(paste("R critical = +/-",abs(R_CRITICAL[1])))
  })


  # Drive peak
  observeEvent(input$dblclick_stocsy_i, {
    drv_pk_oi <<- which(abs(CS_values_real[1,]-input$dblclick_stocsy_i$x)==min(abs(CS_values_real[1,]-input$dblclick_stocsy_i$x)))

    if (drv_pk_oi %in% col_select) {
      drv_pk <<- which(col_select[] == drv_pk_oi)

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {
          z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= cor_cutoff_value_p) {
            rr[k] <<- "A"
          }

          else {
            rr[k] <<- "B"
          }

          if (cor_pearson[drv_pk,z] <= cor_cutoff_value_n){
            rr[k] <<- "C"
          }

        }

        else {
          rr[k] <<- "B"  #falta transformar 'rr' em variavel reativa
        }

        }
        facts$fac_stocsy_i <<- rr[]
      }

      else {
        showModal(modalDialog(
          title = "Warning!!!",
          "The selected point is not inside the previously loaded regions. Please, click on another point or load a new group of signals!",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l",
          drv_pk <- drv_pk_oi
        ))
      }
  })
