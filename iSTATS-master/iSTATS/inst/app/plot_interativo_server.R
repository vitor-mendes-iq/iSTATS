  # Variables
  peran_multi <<- 0
  ysup_multi <- max(NMRData_plot[1,])
  yinf_multi <- -100



  ysup_multi <- ysup_multi + ysup_multi*0.03
  chkzoom_multi <<- 1
  idb_multi <<- 0


  # Data.frame
  testy_multi <<- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[1,])
  ranges_multi <- reactiveValues(x = c((min(testy_multi$Chemical_Shift)), max(testy_multi$Chemical_Shift)), y = c(yinf_multi,ysup_multi)) ## ESSE É O ORIGINAL
  # ranges_multi_plot <- reactiveValues(x = sort(c((min(testy_multi$Chemical_Shift)), max(testy_multi$Chemical_Shift)), decreasing = TRUE), y = c(yinf_multi,ysup_multi))
  spectrums_multi <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[1,]))

  # Plot
    output$plot_multi <- plotly::renderPlotly({

      ggplot2::ggplot(spectrums_multi$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') + ggplot2::scale_x_reverse() +

        ggplot2::coord_cartesian(xlim = c(ranges_multi$x[2],ranges_multi$x[1]), ylim = ranges_multi$y, expand = FALSE) +

        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                       axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                       title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                       axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
                       ) +

        ggplot2::labs(x = "Chemical Shift", y = "Intensity")

    })





  # x2
  observeEvent(input$x2_multi, {

    chkzoom_multi <<- chkzoom_multi*2

    spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum*2

    })


  # x8
  observeEvent(input$x8_multi, {

    chkzoom_multi <<- chkzoom_multi*8

    spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum*8

    })


  # /2
  observeEvent(input$q2_multi, {

    chkzoom_multi <<- chkzoom_multi/2

    spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum/2

    })


  # /8
  observeEvent(input$q8_multi, {

    chkzoom_multi <<- chkzoom_multi/8

    spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum/8

    })


  # Show all
  observeEvent(input$all_multi, {

    ranges_multi$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))

    freshnum_multi <- which(file_names[] == input$spectrum_list_multi)

    ysup_multi <- max(NMRData_plot[freshnum_multi,])

    yinf_multi <- ysup_multi*-0.03

    ysup_multi <- ysup_multi + ysup_multi*0.03

    ranges_multi$y <- c(yinf_multi,ysup_multi)

    spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum/chkzoom_multi

    chkzoom_multi <<- 1

    idb_multi <<- 0

    peran_multi <<- 0

    })


  # Move left
  observeEvent(input$s_left_multi, {

    if (!(ranges_multi$x[1] <= min(testy_multi$Chemical_Shift))) {

      ranges_multi$x[1] <- (ranges_multi$x[1] - peran_multi)

      ranges_multi$x[2] <- (ranges_multi$x[2] - peran_multi)

    }

  })


  # Move right
  observeEvent(input$s_right_multi, {

    if (!(ranges_multi$x[2] >= max(testy_multi$Chemical_Shift))) {

      ranges_multi$x[1] <- (ranges_multi$x[1] + peran_multi)

      ranges_multi$x[2] <- (ranges_multi$x[2] + peran_multi)

    }

  })


  # Move full left
  observeEvent(input$s_left_f_multi, {

    if (!(ranges_multi$x[1] <= min(testy_multi$Chemical_Shift))) {

      das_multi <<- (ranges_multi$x[2] - ranges_multi$x[1])

      ranges_multi$x[1] <- min(testy_multi$Chemical_Shift)

      ranges_multi$x[2] <- (min(testy_multi$Chemical_Shift) + das_multi)

    }

  })


  # Move full right
  observeEvent(input$s_right_f_multi, {

    if ((ranges_multi$x[2] <= max(testy_multi$Chemical_Shift))) {

      das_multi <<- abs(ranges_multi$x[1] - ranges_multi$x[2])

      ranges_multi$x[2]  =  max(testy_multi$Chemical_Shift)

      ranges_multi$x[1] <- (ranges_multi$x[2] - das_multi)

    }

  })


  # View samples plot
  observeEvent(input$spectrum_list_multi,{

    freshnum_multi <- which(file_names[] == input$spectrum_list_multi)

    spectrums_multi$dat <<- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[freshnum_multi,])

    if (!idb_multi && chkzoom_multi == 1) {

      ysup_multi <- max (NMRData_plot[freshnum_multi,])

      yinf_multi <- ysup_multi*-0.03

      ysup_multi <- ysup_multi + ysup_multi*0.03

      ranges_multi$y <- c(yinf_multi, ysup_multi)

    }

    else {

        spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum*chkzoom_multi

    }

  })


  # Second show all (double click)
  observeEvent(input$plot_dblclick_multi, {

    ranges_multi$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))

    freshnum_multi <- which(file_names[] == input$spectrum_list_multi)

    ysup_multi <- max (NMRData_plot[freshnum_multi,])

    yinf_multi <- ysup_multi*-0.03

    ysup_multi <- ysup_multi + ysup_multi*0.03

    ranges_multi$y <- c(yinf_multi,ysup_multi)

    spectrums_multi$dat$Spectrum <- spectrums_multi$dat$Spectrum/chkzoom_multi

    chkzoom_multi <<- 1

    idb_multi <<- 0

    peran_multi <<- 0

    })



