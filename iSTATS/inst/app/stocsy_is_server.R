  ndpis <<- 72

  ncor <<-  1

  Scaling_cor <<- matrix()

  rr_is <<- c()

  peran_stocsy_is <<- 0

  chkzoom_stocsy_is <<- 1

  idb_stocsy_is <<- 0


  ysup_stocsy_is <- max(NMRData_plot[1,])

  yinf_stocsy_is <- ysup_stocsy_is*-0.03

  ysup_stocsy_is <- ysup_stocsy_is + ysup_stocsy_is*0.03


  testy_stocsy_is <- data.frame(Chemical_Shift=CS_values_real[1,])

  ranges_stocsy_is <- reactiveValues(x = c((min(testy_stocsy_is$Chemical_Shift)), max(testy_stocsy_is$Chemical_Shift)), y = c(yinf_stocsy_is,ysup_stocsy_is))

  normalize <- (NMRData[1,] - min(NMRData[1,]))

  spectrums_stocsy_is <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[1,])) #, facs = data.frame(fac_stocsy_is = rr_is[])

  facts_is <<- reactiveValues(fac_stocsy_is = c())

  output$plot_stocsy_is <- renderPlot({

    ggplot2::ggplot(spectrums_stocsy_is$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') +

      ggplot2::geom_line(ggplot2::aes(colour=facts_is$fac_stocsy_is,group=1)) + ggplot2::guides(colour = FALSE) + ggplot2::scale_colour_manual(values = c("red","blue")) +

      ggplot2::coord_cartesian(xlim = c(ranges_stocsy_is$x[2],ranges_stocsy_is$x[1]), ylim = ranges_stocsy_is$y, expand = FALSE) +

      ggplot2::scale_x_reverse() +

      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                     axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                     title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                     axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
       ) +

    ggplot2::labs(x = "Chemical Shift", y = "Intensity")

})


  observeEvent(input$stocsy_s, {

    for (k in 1:dim(cor_pearson)[2]) {

      Scaling_cor <<- (1 - (cor_pearson[drv_pk,k]*cor_pearson[drv_pk,k]))

      s <<- col_select[k]

      if(drv_pk != k) {

        spectrums_stocsy_is$dat$Spectrum[s] <<- spectrums_stocsy_is$dat$Spectrum[s]*Scaling_cor

      }

      else {

        spectrums_stocsy_is$dat$Spectrum[s] <<- spectrums_stocsy_is$dat$Spectrum[s]*0.02

      }
    }

  })


  observeEvent(input$radio_is, {

    value <<- (input$radio_is)

    if (value == 1) {

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {

          z <<- which(col_select[] == k)



          if (cor_pearson[drv_pk,z] >= cor_cutoff) {

            rr_is[k] <<- "A"

          }

          else {

            rr_is[k] <<- "B"

          }

        }

      }

      facts_is$fac_stocsy_is <<- rr_is[]

    }

    if (value == 2) {

      for (k in 1:dim(NMRData)[2]) {

      if (k %in% col_select) {

        z <<- which(col_select[] == k)


        if (cor_spearman[drv_pk,z] >= cor_cutoff) {

          rr_is[k] <<- "A"

        }

        else {

          rr_is[k] <<- "B"

        }}

    else {

        rr_is[k] <<- "B"  #falta transformar 'rr_is' em variavel reativa

    }
    }

    facts_is$fac_stocsy_is <<- rr_is[]

    }


  if (value == 3) {

    for (k in 1:dim(NMRData)[2]) {

      if (k %in% col_select) {

        z <<- which(col_select[] == k)

        if (cor_pearson[drv_pk,z] >= cor_cutoff) {

          rr_is[k] <<- "A"

        }

        else {  }


        if (cor_spearman[drv_pk,z] >= cor_cutoff) {

          rr_is[k] <<- "A"

        }
        else {

        }
        }

      else {  }

    }


    facts_is$fac_stocsy_is <<- rr_is[]

    }

  })

  observeEvent(input$plot_brush_stocsy_is,{
    brush <- input$plot_brush_stocsy_is
    if (!is.null(brush)) {

      ranges_stocsy_is$x <- c(brush$xmin, brush$xmax)

      ranges_stocsy_is$y <- c(brush$ymin, brush$ymax)

      idb_stocsy_is <<- 1

      peran_stocsy_is <<- (ranges_stocsy_is$x[2] - ranges_stocsy_is$x[1])*0.2



    }

    else {

      ranges_stocsy_is$x <- NULL

    }
  })

  observeEvent(input$x2_stocsy_is, {

    tryton <<- facts_is$fac_stocsy_is

    chkzoom_stocsy_is <<- chkzoom_stocsy_is*2

    spectrums_stocsy_is$dat$Spectrum <- spectrums_stocsy_is$dat$Spectrum*2

  })


  observeEvent(input$x8_stocsy_is, {

    chkzoom_stocsy_is <<- chkzoom_stocsy_is*8

    spectrums_stocsy_is$dat$Spectrum <- spectrums_stocsy_is$dat$Spectrum*8

  })


  observeEvent(input$q2_stocsy_is, {

    chkzoom_stocsy_is <<- chkzoom_stocsy_is/2

    spectrums_stocsy_is$dat$Spectrum <- spectrums_stocsy_is$dat$Spectrum/2

  })


  observeEvent(input$q8_stocsy_is, {

    chkzoom_stocsy_is <<- chkzoom_stocsy_is/8

    spectrums_stocsy_is$dat$Spectrum <- spectrums_stocsy_is$dat$Spectrum/8

  })


  observeEvent(input$all_stocsy_is, {

    ranges_stocsy_is$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))

    freshnum_stocsy_is <- which(file_names[] == input$spectrum_list_stocsy_is)

    ysup_stocsy_is <- max (NMRData_plot[freshnum_stocsy_is,])

    yinf_stocsy_is <- ysup_stocsy_is*-0.03

    ysup_stocsy_is <- ysup_stocsy_is + ysup_stocsy_is*0.03

    ranges_stocsy_is$y <- c(yinf_stocsy_is,ysup_stocsy_is)

    spectrums_stocsy_is$dat$Spectrum <- spectrums_stocsy_is$dat$Spectrum/chkzoom_stocsy_is

    chkzoom_stocsy_is <<- 1

    idb_stocsy_is <<- 0

  })


  observeEvent(input$s_left_stocsy_is, {

    if (!(ranges_stocsy_is$x[1] <= min(testy_stocsy_is$Chemical_Shift))) {

      ranges_stocsy_is$x[1] <<- (ranges_stocsy_is$x[1] - peran_stocsy_is)

      ranges_stocsy_is$x[2] <<- (ranges_stocsy_is$x[2] - peran_stocsy_is)

    }

  })


  observeEvent(input$s_right_stocsy_is, {

    if (!(ranges_stocsy_is$x[2] >= max(testy_stocsy_is$Chemical_Shift))) {

      ranges_stocsy_is$x[1] <<- (ranges_stocsy_is$x[1] + peran_stocsy_is)

      ranges_stocsy_is$x[2] <<- (ranges_stocsy_is$x[2] + peran_stocsy_is)

    }

  })


  observeEvent(input$s_left_f_stocsy_is, {

    if (!(ranges_stocsy_is$x[1] <= min(testy_stocsy_is$Chemical_Shift))) {

      das_stocsy_is <<- (ranges_stocsy_is$x[2] - ranges_stocsy_is$x[1])

      ranges_stocsy_is$x[1] <<- min(testy_stocsy_is$Chemical_Shift)

      ranges_stocsy_is$x[2] <<- (min(testy_stocsy_is$Chemical_Shift) + das_stocsy_is)

    }

  })


  observeEvent(input$s_right_f_stocsy_is, {

    if (!(ranges_stocsy_is$x[2] >= max(testy_stocsy_is$Chemical_Shift))) {

      das_stocsy_is <<- (ranges_stocsy_is$x[2] - ranges_stocsy_is$x[1])

      ranges_stocsy_is$x[2] <<- max(testy_stocsy_is$Chemical_Shift)

      ranges_stocsy_is$x[1] <<- (max(testy_stocsy_is$Chemical_Shift) - das_stocsy_is)

    }

  })


  observeEvent(input$spectrum_list_stocsy_is,{

    freshnum_stocsy_is <- which(file_names[] == input$spectrum_list_stocsy_is)

    normalize <<- (NMRData_plot[freshnum_stocsy_is,] - min(NMRData_plot[freshnum_stocsy_is,]))

    spectrums_stocsy_is$dat <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=normalize)


    if (!idb_stocsy_is && chkzoom_stocsy_is == 1) {

      ysup_stocsy_is <- max (normalize[])

      yinf_stocsy_is <- ysup_stocsy_is*-0.03

      ysup_stocsy_is <- ysup_stocsy_is + ysup_stocsy_is*0.03

      ranges_stocsy_is$y <- c(yinf_stocsy_is, ysup_stocsy_is)

    }

    else {

      spectrums_stocsy_is$dat$Spectrum_ <- spectrums_stocsy_is$dat$Spectrum*chkzoom_stocsy_is

    }

  })

  observeEvent(input$dblclick_stocsy_is, {

    drv_pk_ois <<- which(abs(CS_values_real[1,]-input$dblclick_stocsy_is$x)==min(abs(CS_values_real[1,]-input$dblclick_stocsy_is$x)))

      if (drv_pk_ois %in% col_select) {

        drv_pk <<- which(col_select[] == drv_pk_ois)

        for (k in 1:dim(NMRData)[2]) {

          if (k %in% col_select) {

          z <<- which(col_select[] == k)

            if (cor_pearson[drv_pk,z] >= input$cutoff_stocsy_is) {

            rr_is[k] <<- "A"

            }

            else {

              rr_is[k] <<- "B"

            }

          }

          else {

            rr_is[k] <<- "B"

          }

    }

      facts_is$fac_stocsy_is <<- rr_is[]

    }

    else {

    showModal(modalDialog(
      title = "Warning!!!",
      "The selected point is not inside the previously loaded regions. Please, click on another point or load a new group of signals!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l",

      drv_pk <- drv_pk_ois


    ))

    }

  })



  ## Download plot


  # DPI
  observeEvent(input$slide_dpi_is, {

    n_dpi <<- input$slide_dpi_is

  })

  # Botton download plot
  output$plot_download_is <- downloadHandler(
    filename = function() {
      paste0('stocsy-i.',input$data_input_is)
    },
    content = function(file1) {
      ggplot2::ggsave(file1,width=295, device = input$data_input,height=205, units="mm", dpi = n_dpi)

    }
  )


  observeEvent(input$cutoff_stocsy_is, {

    cor_cutoff <<- input$cutoff_stocsy_is


    for (k in 1:dim(NMRData)[2]) {

      if (k %in% col_select) {

        z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= cor_cutoff) {

            rr_is[k] <<- "A"

          }

          else {

            rr_is[k] <<- "B"

          }

        }

      else {

        rr_is[k] <<- "B"  #falta transformar 'rr_is' em variavel reativa

      }

      }

        facts_is$fac_stocsy_is <<- rr_is[]


  })

