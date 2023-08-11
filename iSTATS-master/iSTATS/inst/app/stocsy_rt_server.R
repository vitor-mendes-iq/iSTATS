  ncor_rt <<-  1

  rr_rt <<- c()

  chkzoom_stocsy_rt <<- 1

  idb_stocsy_rt <<- 0

  peran_stocsy_rt <<- 0

  ysup_stocsy_rt <- max(NMRData_plot[1,])

  yinf_stocsy_rt <- ysup_stocsy_rt*-0.03

  ysup_stocsy_rt <- ysup_stocsy_rt + ysup_stocsy_rt*0.03

  testy_stocsy_rt <- data.frame(Chemical_Shift=CS_values_real[1,])

  ranges_stocsy_rt <- reactiveValues(x = c((min(testy_stocsy_rt$Chemical_Shift)), max(testy_stocsy_rt$Chemical_Shift)), y = c(yinf_stocsy_rt,ysup_stocsy_rt))

  spectrums_stocsy_rt <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[1,])) #, facs = data.frame(fac_stocsy_rt = rr_rt[])

  facts_rt <<- reactiveValues(fac_stocsy_rt = c())

  #Plot

  output$plot_stocsy_rt <- renderPlot({

    ggplot2::ggplot(spectrums_stocsy_rt$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') +

    ggplot2::geom_line(ggplot2::aes(colour=facts_rt$fac_stocsy_rt,group=1)) + ggplot2::guides(colour = FALSE) + ggplot2::scale_colour_manual(values = c("red","blue")) +

    ggplot2::coord_cartesian(xlim = c(ranges_stocsy_i$rt[2],ranges_stocsy_i$rt[1]), ylim = ranges_stocsy_rt$y, expand = FALSE) +

    ggplot2::scale_x_reverse() +

      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
            axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
            title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
            axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
            ) +

      ggplot2::labs(x = "Chemical Shift", y = "Intensity")

    })


  observeEvent(input$radio_rt, {

    value <<- (input$radio_rt)

    if (value == 1) {

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {

            z <<- which(col_select[] == k)



            if (cor_pearson[drv_pk,z] >= cor_cutoff_rt) {

              rr_rt[k] <<- "A"

            }

            else {

              rr_rt[k] <<- "B"


            }
          }

      }

        facts_rt$fac_stocsy_rt <<- rr_rt[]

      }

      if (value == 2) {

        for (k in 1:dim(NMRData)[2]) {

          if (k %in% col_select) {

            z <<- which(col_select[] == k)


            if (cor_spearman[drv_pk,z] >= cor_cutoff_rt) {

              rr_rt[k] <<- "A"

            }

            else {

              rr_rt[k] <<- "B"


            }
          }

         else {

              rr_rt[k] <<- "B"

         }

          }

            facts_rt$fac_stocsy_rt <<- rr_rt[]

          }


      if (value == 3) {

        for (k in 1:dim(NMRData)[2]) {

          if (k %in% col_select) {

            z <<- which(col_select[] == k)

            if (cor_pearson[drv_pk,z] >= cor_cutoff_rt) {

              rr_rt[k] <<- "A"

            }

            else {
            }


              if (cor_spearman[drv_pk,z] >= cor_cutoff_rt) {

              rr_rt[k] <<- "A"

                  }
              else {

              }
            }

           else {
          }
        }


          facts_rt$fac_stocsy_rt <<- rr_rt[]

        }

    })

  observeEvent(input$plot_brush_stocsy_rt,{
    brush <- input$plot_brush_stocsy_rt
    if (!is.null(brush)) {

      ranges_stocsy_rt$x <- c(brush$xmin, brush$xmax)

      ranges_stocsy_rt$y <- c(brush$ymin, brush$ymax)

      idb_stocsy_rt <<- 1

      peran_stocsy_rt <<- (ranges_stocsy_rt$x[2] - ranges_stocsy_rt$x[1])*0.2

      }

    else {

      ranges_stocsy_rt$x <- NULL

    }

  })




  observeEvent(input$x2_stocsy_rt, {

    tryton <<- facts_rt$fac_stocsy_rt

    chkzoom_stocsy_rt <<- chkzoom_stocsy_rt*2


    spectrums_stocsy_rt$dat$Spectrum <- spectrums_stocsy_rt$dat$Spectrum*2

  })


  observeEvent(input$x8_stocsy_rt, {

    chkzoom_stocsy_rt <<- chkzoom_stocsy_rt*8

    spectrums_stocsy_rt$dat$Spectrum <- spectrums_stocsy_rt$dat$Spectrum*8

  })


  observeEvent(input$q2_stocsy_rt, {

    chkzoom_stocsy_rt <<- chkzoom_stocsy_rt/2

    spectrums_stocsy_rt$dat$Spectrum <- spectrums_stocsy_rt$dat$Spectrum/2

  })


  observeEvent(input$q8_stocsy_rt, {

    chkzoom_stocsy_rt <<- chkzoom_stocsy_rt/8

    spectrums_stocsy_rt$dat$Spectrum <- spectrums_stocsy_rt$dat$Spectrum/8

  })


  observeEvent(input$all_stocsy_rt, {

    ranges_stocsy_rt$x <- c((min(testy_stocsy_rt$Chemical_Shift)), max(testy_stocsy_rt$Chemical_Shift))

    freshnum_stocsy_rt <- which(file_names[] == input$spectrum_list_stocsy_rt)

    ysup_stocsy_rt <- max (NMRData_plot[freshnum_stocsy_rt,])

    yinf_stocsy_rt <- ysup_stocsy_rt*-0.03

    ysup_stocsy_rt <- ysup_stocsy_rt + ysup_stocsy_rt*0.03

    ranges_stocsy_rt$y <- c(yinf_stocsy_rt,ysup_stocsy_rt)

    spectrums_stocsy_rt$dat$Spectrum <- spectrums_stocsy_rt$dat$Spectrum/chkzoom_stocsy_rt

    chkzoom_stocsy_rt <<- 1

    idb_stocsy_rt <<- 0

  })

  observeEvent(input$s_left_stocsy_rt, {

    if (!(ranges_stocsy_rt$x[1] <= min(testy_stocsy_rt$Chemical_Shift))) {

      ranges_stocsy_rt$x[1] <<- (ranges_stocsy_rt$x[1] - peran_stocsy_rt)

      ranges_stocsy_rt$x[2] <<- (ranges_stocsy_rt$x[2] - peran_stocsy_rt)

     }

  })

  observeEvent(input$s_right_stocsy_rt, {

    if (!(ranges_stocsy_rt$x[2] >= max(testy_stocsy_rt$Chemical_Shift))) {

      ranges_stocsy_rt$x[1] <<- (ranges_stocsy_rt$x[1] + peran_stocsy_rt)

      ranges_stocsy_rt$x[2] <<- (ranges_stocsy_rt$x[2] + peran_stocsy_rt)

      }

  })

  observeEvent(input$s_left_f_stocsy_rt, {

    if (!(ranges_stocsy_rt$x[1] <= min(testy_stocsy_rt$Chemical_Shift))) {

      das_stocsy_rt <<- (ranges_stocsy_rt$x[2] - ranges_stocsy_rt$x[1])

      ranges_stocsy_rt$x[1] <<- min(testy_stocsy_rt$Chemical_Shift)

      ranges_stocsy_rt$x[2] <<- (min(testy_stocsy_rt$Chemical_Shift) + das_stocsy_rt)

      }

  })

  observeEvent(input$s_right_f_stocsy_rt, {

    if (!(ranges_stocsy_rt$x[2] >= max(testy_stocsy_rt$Chemical_Shift))) {

        das_stocsy_rt <<- (ranges_stocsy_rt$x[2] - ranges_stocsy_rt$x[1])

        ranges_stocsy_rt$x[2] <<- max(testy_stocsy_rt$Chemical_Shift)

        ranges_stocsy_rt$x[1] <<- (max(testy_stocsy_rt$Chemical_Shift) - das_stocsy_rt)

      }

  })


  observeEvent(input$spectrum_list_stocsy_rt,{

    freshnum_stocsy_rt <- which(file_names[] == input$spectrum_list_stocsy_rt)

    spectrums_stocsy_rt$dat <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_plot[freshnum_stocsy_rt,])

    if (!idb_stocsy_rt && chkzoom_stocsy_rt == 1) {

      ysup_stocsy_rt <- max (NMRData_plot[freshnum_stocsy_rt,])

      yinf_stocsy_rt <- ysup_stocsy_rt*-0.03

      ysup_stocsy_rt <- ysup_stocsy_rt + ysup_stocsy_rt*0.03

      ranges_stocsy_rt$y <- c(yinf_stocsy_rt, ysup_stocsy_rt)

     }

    else {

        spectrums_stocsy_rt$dat$Spectrum <- spectrums_stocsy_rt$dat$Spectrum*chkzoom_stocsy_rt

      }

    })

  observeEvent(input$dblclick_stocsy_rt, {

    drv_pk_ort <<- which(abs(CS_values_real[1,]-input$dblclick_stocsy_rt$x)==min(abs(CS_values_real[1,]-input$dblclick_stocsy_rt$x)))

    if (drv_pk %in% col_select) {

    drv_pk <<- which(col_select[] == drv_pk)

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {

          z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= input$cutoff_stocsy_rt) {

            rr_rt[k] <<- "A"

          }

          else {

            rr_rt[k] <<- "B"

          }

        }

        else {

          rr_rt[k] <<- "B"  #falta transformar 'rr_rt' em variavel reativa

        }

        }

        facts_rt$fac_stocsy_rt <<- rr_rt[]

      }

      else {

        showModal(modalDialog(
          title = "Warning!!!",
          "The selected point is not inside the previously loaded regions. Please, click on another point or load a new group of signals!",
          easyClose = TRUE,
          footer = modalButton("Close"),
          size = "l",

          drv_pk <- drv_pk_ort



          )
        )

      }

    })


    observeEvent(input$cutoff_stocsy_rt, {

      cor_cutoff_rt <<- input$cutoff_stocsy_rt

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {

          z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= cor_cutoff_rt) {

            rr_rt[k] <<- "A"

          }

          else {

            rr_rt[k] <<- "B"

          }

        }

        else {

          rr_rt[k] <<- "B"  #falta transformar 'rr_rt' em variavel reativa

        }

      }

      facts_rt$fac_stocsy_rt <<- rr_rt[]


       })

    # Plot_PDF I-STOCSY
    observeEvent(input$stocsy_rt_print_PDF, {
      ggplot2::ggsave("stocsy-rt.pdf",path = choose.dir(),width=195, height=105, units="mm", dpi = 300)
    })


  observeEvent(input$hover_stocsy_rt, {

    drv_pk <<- which(abs(CS_values_real[1,]-input$hover_stocsy_rt$x)==min(abs(CS_values_real[1,]-input$hover_stocsy_rt$x)))

    if (drv_pk %in% col_select) {

      drv_pk <<- which(col_select[] == drv_pk)

      for (k in 1:dim(NMRData)[2]) {

        if (k %in% col_select) {

          z <<- which(col_select[] == k)

          if (cor_pearson[drv_pk,z] >= 0.8) {

             rr_rt[k] <<- "A"

            }

           else {

                rr_rt[k] <<- "B"

              }

            }

          else {

              rr_rt[k] <<- "B"  #falta transformar 'rr_rt' em variavel reativa

            }

      }

        facts_rt$fac_stocsy_rt <<- rr_rt[]

      }

        else {

        }

    })


