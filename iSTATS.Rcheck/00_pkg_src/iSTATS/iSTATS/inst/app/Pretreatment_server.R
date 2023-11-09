## Module Pretreatment ###############################

# Variables
NMRData_Mean <- colMeans(NMRData[,])
ysup <- max(NMRData[,])
yinf <- ysup*-0.03
ysup <- ysup + ysup*0.03
p_value <- 0.05


# Data.frame
testy_norm <<- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData[1,])
CS_norm <<- reactiveValues(vranges = c(-13131313,-131313))
ranges <- reactiveValues(x = c(min(testy_norm$Chemical_Shift),(max(testy_norm$Chemical_Shift))), y = c(yinf,ysup))
ranges_sel <- reactiveValues(x = c(min(testy_norm$Chemical_Shift),(max(testy_norm$Chemical_Shift))), y = c(yinf,ysup))
spectrums <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData[1,]))
spectrums_sel <- reactiveValues(dat = data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData[1,]))
rr_n <<- reactiveValues(csranges = c(-13131313,-131313))



# Show Normalize section
observeEvent(input$pretreatment, {
  updateTabsetPanel(session, "main_bar", "Normalize Selection")
  removeModal()
  .mean_centering()
})


# Plot Normalization

# Plot spectra average
output$plot_norm <- renderPlot({
  ggplot2::ggplot(spectrums$dat,ggplot2::aes(Chemical_Shift,Spectrum)) + ggplot2::geom_line(color='blue') + ggplot2::scale_x_reverse() +
    ggplot2::coord_cartesian(xlim = c(ranges$x[2],ranges$x[1]), ylim = ranges$y, expand = FALSE) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 12, color = "#000000"),
                    axis.text.y = ggplot2::element_text(size = 12, color = "#000000"),
                    title = ggplot2::element_text(face = "bold", color = "#000000", size = 17),
                    axis.title = ggplot2::element_text(face = "bold", color = "#000000", size = 15)
    ) +
    ggplot2::labs(x = "Chemical Shift", y = "Intensity") +
    ggplot2::geom_vline(xintercept=rr_nn$csranges, color = "green", size = 0.1, alpha=0.3) +
    ggplot2::geom_vline(xintercept=rr_n$csranges, color = "red", size = 0.1, alpha=0.3)
})



# Brush
observeEvent(input$plot_brush_norm,{
  brush <- input$plot_brush_norm

  if (!is.null(brush)) {
    ranges$x <- c(brush$xmin, brush$xmax)
    ranges$y <- c(brush$ymin, brush$ymax)
    spectrums$dat$Spectrum <- testy$Spectrum*chkzoom
    peran <<- (ranges$x[2] - ranges$x[1])*0.2
  }

  else {
    ranges$x <- c(min(testy$Chemical_Shift),(max(testy$Chemical_Shift)))
  }
})


# Mean centering
observeEvent(input$mean_norm,{
  # s_stoc <<- 0


  .mean_centering()

  choose_norm <<- 7

})


# Scaling
observeEvent(input$scaling_norm,{
  if (!(sel_ind == 0)) {

  rr_n$csranges <<- c()
  col_sel_norm <<- c()

  .autoscaling()

  f <- 0
  output$norm_cond <- renderText(paste("After normalize: ", dif,"%"))

  for (j in 1:length(norm_var)) {

    if (norm_var[j] == "NO") {
      f <- f + 1
      col_sel_norm[f] <<- col_select[j]
    }
    else{

    }
  }
  rr_n$csranges <- CS_values_real[1,col_sel_norm]
  choose_norm <<- 6
  }

  else{
    showModal(modalDialog(
      title = "Warning!!!",
      "No region was selected. You must first select the desired region(s) before to start Normalize section!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }

})


# Pareto
observeEvent(input$pareto_norm,{

  if (!(sel_ind == 0)) {

  rr_n$csranges <<- c()
  .pareto()
  col_sel_norm <<- c()
  f <- 0


  output$norm_cond <- renderText(paste("Affter normalize: ", dif,"%"))

  for (j in 1:length(norm_var)) {

    if (norm_var[j] == "NO") {
      f <- f + 1
      col_sel_norm[f] <<- col_select[j]
    }
    else{

    }
  }
  rr_n$csranges <- CS_values_real[1,col_sel_norm]
  choose_norm <<- 5
  }
  else{
    showModal(modalDialog(
      title = "Warning!!!",
      "No region was selected. You must first select the desired region(s) before to start Normalize section!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }

})

# Square Root
observeEvent(input$sqr_norm,{
  if (!(sel_ind == 0)) {

  rr_n$csranges <<- c()
  .sqr_norm()
  col_sel_norm <<- c()
  f <- 0

  output$norm_cond <- renderText(paste("Affter normalize: ", dif,"%"))

  for (j in 1:length(norm_var)) {

    if (norm_var[j] == "NO") {
      f <- f + 1
      col_sel_norm[f] <<- col_select[j]
    }
    else{

    }
  }

  rr_n$csranges <- CS_values_real[1,col_sel_norm]
  choose_norm <<- 4
  }

  else{
    showModal(modalDialog(
      title = "Warning!!!",
      "No region was selected. You must first select the desired region(s) before to start Normalize section!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }

})


# Inverse Square Root
observeEvent(input$i_sqr_norm,{
  if (!(sel_ind == 0)) {

  rr_n$csranges <<- c()
  .isqr_norm()
  col_sel_norm <<- c()
  f <- 0

  output$norm_cond <- renderText(paste("Affter normalize: ", dif,"%"))

  for (j in 1:length(norm_var)) {

    if (norm_var[j] == "NO") {
      f <- f + 1
      col_sel_norm[f] <<- col_select[j]
    }
    else{

    }
  }

  rr_n$csranges <- CS_values_real[1,col_sel_norm]
  choose_norm <<- 3
  }

  else{
    showModal(modalDialog(
      title = "Warning!!!",
      "No region was selected. You must first select the desired region(s) before to start Normalize section!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }

})


# Log
observeEvent(input$log_norm,{

  if (!(sel_ind == 0)) {

  rr_n$csranges <<- c()
  .log_norm()
  col_sel_norm <<- c()
  f <- 0

  output$norm_cond <- renderText(paste("Affter normalize: ", dif,"%"))

  for (j in 1:length(norm_var)) {

    if (norm_var[j] == "NO") {
      f <- f + 1
      col_sel_norm[f] <<- col_select[j]
    }
    else{

    }
  }

  rr_n$csranges <- CS_values_real[1,col_sel_norm]
  choose_norm <<- 2
  }

  else{
    showModal(modalDialog(
      title = "Warning!!!",
      "No region was selected. You must first select the desired region(s) before to start Normalize section!",
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }

})



# Start STOCSY-I
observeEvent(input$s_stocsy_i_norm, {
  if (choose_norm == 1) {
    .s_stocsy(matr_cor)
  }

  if (choose_norm == 2) {
    .s_stocsy(NMR_log)
  }
  if (choose_norm == 3) {
    .s_stocsy(NMR_i_sqr)
  }
  if (choose_norm == 4) {
    .s_stocsy(NMR_sqr)
  }
  if (choose_norm == 5) {
    .s_stocsy(NMR_Pareto)
  }
  if (choose_norm == 6) {
    .s_stocsy(NMR_ASC)
  }
  if (choose_norm == 7) {
    .s_stocsy(NMR_MC)
  }

})


# R critical by P-value
observeEvent(input$pv_n, {
  p_value_n <- as.numeric(input$pv_n)
  p_value <- p_value_n
  r_critical(p_value_n)
  output$r_critical <- renderText(paste("R critical = +/-",abs(R_CRITICAL[1])))
})





# Download point selected
output$downloadpoints2 <- downloadHandler(
  filename = function() {
    paste("Selected_Regions", ".csv", sep = "")
  },
  content = function(file) {
    if (choose_norm == 1) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
      colnames(matr_cor) <<- CS_sel_real
      write.table(matr_cor, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
    if (choose_norm == 2) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      colnames(NMR_log) <<- CS_sel_real
      write.table(NMR_log, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
    if (choose_norm == 3) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      colnames(NMR_i_sqr) <<- CS_sel_real
      write.table(NMR_i_sqr, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
    if (choose_norm == 4) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      colnames(NMR_sqr) <<- CS_sel_real
      write.table(NMR_sqr, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
    if (choose_norm == 5) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      colnames(NMR_Pareto) <<- CS_sel_real
      write.table(NMR_Pareto, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
    if (choose_norm == 6) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      colnames(NMR_ASC) <<- CS_sel_real
      write.table(NMR_ASC, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
    if (choose_norm == 7) {
      col_select <<- col_select[order(col_select)]
      CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
      colnames(NMR_MC) <<- CS_sel_real
      write.table(NMR_MC, file,sep = ",",col.names = TRUE,row.names = FALSE)
    }
  }
)

# Download bucket selected
output$downloadareas2 <- downloadHandler(
  filename = function() {
    paste("Normalize_Regions", ".csv", sep = "")},
  content = function(file) {

    if (choose_norm == 1) {

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

    if (choose_norm == 2) {

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

    if (choose_norm == 3) {

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

    if (choose_norm == 4) {

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

    if (choose_norm == 5) {

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

    if (choose_norm == 6) {

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

    if (choose_norm == 7) {

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
  }
)

# Plot_PDF I-STOCSY
observeEvent(input$norm_print_PDF, {
  ggplot2::ggsave("normal_regions.pdf",path = choose.dir(),width=195, height=105, units="mm", dpi = 300)
})
