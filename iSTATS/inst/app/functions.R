

# Normalize test
norm_test <- function(p_value,m) {
  p_value <- 0.05
  norm_var <<- c()
  count_norm_y <- 0
  chknorm <- c()
  dif <<- 0

  ## Shapiro normalization test
  for (h in 1:dim(m)[2]) {
    chknorm <- shapiro.test(m[,h])
    if(is.null(chknorm$p.value)) {
      print("merda")
    }

    if (chknorm$p.value <= p_value) {
      norm_var[h] <<- "YES"
    }

    else{
      norm_var[h] <<- "NO"
    }
  }

  ## Count variable normal
  for (k in 1:length(norm_var)) {

    if (norm_var[k]=='YES') {
      count_norm_y <- count_norm_y + 1
    }
  }


  dif <<- round((count_norm_y/length(norm_var))*100,2)

  f <- 0
  g <- 0
  col_sel_norm <<- c()
  col_sel_norm2 <<- c()

  for (j in 1:length(norm_var)) {

    if (norm_var[j] == "NO") {
      f <- f + 1
      col_sel_norm[f] <<- col_select[j]
    }
    else{
      g <- g + 1
      col_sel_norm2[g] <<- col_select[j]
    }
  }
  rr_n$csranges <- CS_values_real[1,col_sel_norm]
  rr_nn$csranges <- CS_values_real[1,col_sel_norm2]

  if (stop_menssager != 1) {

    if (dif <= 70) {
      showModal(modalDialog(
        title = "Warning!!!",
        paste("Your data is ", dif,"% normalized, please apply the apodization
      functions and do a pre-treatment before starting the STOCSY analysis or do not trust R critical "),
        easyClose = FALSE,
        # footer = modalButton("Close"),
        footer = tagList(actionButton("pretreatment", "Normalize Selection"),
                         modalButton("Close")),
        size = "l"
      ))
    }

    else{
      showModal(modalDialog(
        title = "Warning!!!",
        paste("Your data is ", dif,"% normalized."),
        easyClose = FALSE,
        footer = modalButton("Close"),
        size = "l"
      ))
    }

  }
  # return(dif)
  }


###############################################################################

# Calculate of R critical by test t-student table
r_critical <- function(p_value) {

  if(is.null(file_names)){

  }
  else {
  p_value <- p_value
  df <- length(file_names) - 2
  if(p_value != 0) {
  tc <- qt(p_value,df)
  R_CRITICAL <- round(as.numeric(tc/(sqrt(df+tc^2))),3)
  R_CRITICAL <<- c(-abs(R_CRITICAL),abs(R_CRITICAL))
  }}
}
###############################################################################

# Refresh variables
refreshval <- function() {

  #############  PLOT_INTERATIVO_SERVER.R RESET  #############
  peran_multi <- 0
  chkzoom_multi <- 1
  idb_multi <- 0
  testy_multi <<- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData[1,])
  ranges_multi$x <<- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))
  ##############################################################################

  #############  SELECT_SIGNALS_SERVER.R RESET  #############
  # NMRData_plot <- NMRData
  NMRData_Mean <- colMeans(NMRData_plot[,])
  spectrums$dat <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=(NMRData_Mean[]))
  spectrums_sel$dat <- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=(NMRData_Mean[]))
  CS_selection$vranges <- c(-13131313,-131313)
  testy <<- data.frame(Chemical_Shift=CS_values_real[1,],Spectrum=NMRData_Mean[])
  col_select <- c()
  alr_click <- 0
  sel_ind <- 0
  peran <- 0
  exran <- c()
  exp_click <- 0
  ysup <- max(testy$Spectrum)
  yinf <- -1000000
  ysup <- ysup + ysup*0.03
  ranges_sel$y <- c(yinf, ysup)
  ranges_sel$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))
  ranges$y <- c(yinf, ysup)
  ranges$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))
  cor_cutoff <- 0.8
  ##############################################################################

  #############  STOCSY_I_SERVER.R RESET  #############
  ncor <-  1
  rr <- c()
  peran_stocsy_i <- 0
  chkzoom_stocsy_i <- 1
  idb_stocsy_i <- 0
  testy_stocsy_i <<- data.frame(Chemical_Shift=CS_values_real[1,])
  ranges_stocsy_i$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))
  facts <<- reactiveValues(fac_stocsy_i = c())
  ##############################################################################

  #############  STOCSY_IS_SERVER.R RESET  #############
  ncor <-  1
  Scaling_cor <- matrix()
  rr_is <- c()
  peran_stocsy_is <- 0
  chkzoom_stocsy_is <- 1
  idb_stocsy_is <- 0
  testy_stocsy_is <<- data.frame(Chemical_Shift=CS_values_real[1,])
  ranges_stocsy_is$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))
  facts_is <<- reactiveValues(fac_stocsy_is = c())
  ##############################################################################

  #############  STOCSY_RT_SERVER.R RESET  #############
  ncor_rt <-  1
  rr_rt <<- c()
  peran_stocsy_rt <- 0
  chkzoom_stocsy_rt <- 1
  idb_stocsy_rt <- 0
  testy_stocsy_rt <<- data.frame(Chemical_Shift=CS_values_real[1,])
  ranges_stocsy_rt$x <- c(min(CS_values_real[1,]),(max(CS_values_real[1,])))
  facts_rt <<- reactiveValues(fac_stocsy_rt = c())
  ##############################################################################

  ###################### Pretreatment_server ###################
  rr_n <<- reactiveValues(csranges = c(-13131313,-131313))
  rr_nn <<- reactiveValues(csranges = c(-13131313,-131313))

  ############################################################


}
####################################################################################

## Module Pré-treatment


###################### Mean centering ##################
.mean_centering <- function() {

  if (!(sel_ind == 0)) {

  # rr_n$csranges <<- c()
  # rr_nn$csranges <- c()


  CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
  matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
  NMR_MC <<- matrix(data = 0, nrow = dim(matr_cor)[1], ncol = dim(matr_cor)[2])
  X_mean <- colMeans(matr_cor)


  for (col in 1:dim(matr_cor)[2]){

    for (lin in 1:dim(matr_cor)[1]){
      NMR_MC[lin,col] <<- matr_cor[lin,col] - X_mean[col]
    }
  }

  norm_test(p_value,m=NMR_MC)
  output$norm_cond <- renderText(paste("Affter normalize: ", dif,"%"))

  # f <- 0
  # g <- 0
  #
  # for (j in 1:length(norm_var)) {
  #
  #   if (norm_var[j] == "NO") {
  #     f <- f + 1
  #     col_sel_norm[f] <<- col_select[j]
  #   }
  #   else{
  #     g <- g + 1
  #     col_sel_norm2[g] <<- col_select[j]
  #   }
  # }
  # rr_n$csranges <- CS_values_real[1,col_sel_norm]
  # rr_nn$csranges <- CS_values_real[1,col_sel_norm2]
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
}
###################### Scaling #########################
.autoscaling <- function() {


  CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
  matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
  NMR_ASC <<- matrix(data = 0, nrow = dim(matr_cor)[1], ncol = dim(matr_cor)[2])
  rows_temp <<- vector(mode="numeric", length=0)
  NMR_MC <<- matrix(data = 0, nrow = dim(matr_cor)[1], ncol = dim(matr_cor)[2])
  X_mean <- colMeans(matr_cor)

  for (col in 1:dim(matr_cor)[2]){

    for (lin in 1:dim(matr_cor)[1]){
      NMR_MC[lin,col] <<- matr_cor[lin,col] - X_mean[col]
    }
  }

  for (col in 1:dim(NMR_MC)[2]){

    for (lin in 1:dim(NMR_MC)[1]) {
      rows_temp[lin] <<- (NMR_MC[lin,col])^2
    }
    sum_MC <- sum(rows_temp)
    sum_MC <- (sum_MC)/(dim(NMR_MC)[1]-1)
    s <- sqrt(sum_MC)

    for (lin in 1:dim(NMR_MC)[1]) {
      NMR_ASC[lin,col] <<- (NMR_MC[lin,col])/(s)
    }
  }

  norm_test(p_value,m=NMR_ASC)

}


##################### Pareto ##########################
.pareto <- function() {

  CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
  matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
  NMR_Pareto <<- matrix(data = 0, nrow = dim(matr_cor)[1], ncol = dim(matr_cor)[2])
  rows_temp <<- vector(mode="numeric", length=0)
  NMR_MC <<- matrix(data = 0, nrow = dim(matr_cor)[1], ncol = dim(matr_cor)[2])
  X_mean <- colMeans(matr_cor)

  for (col in 1:dim(matr_cor)[2]){

    for (lin in 1:dim(matr_cor)[1]){
      NMR_MC[lin,col] <<- matr_cor[lin,col] - X_mean[col]
    }
  }

  for (col in 1:dim(NMR_MC)[2]){

    for (lin in 1:dim(NMR_MC)[1]) {
      rows_temp[lin] <<- (NMR_MC[lin,col])^2
    }
    sum_MC <- sum(rows_temp)
    sum_MC <- (sum_MC)/(dim(NMR_MC)[1]-1)
    s <- sqrt(sum_MC)
    s <- sqrt(s)

    for (lin in 1:dim(NMR_MC)[1]) {
      NMR_Pareto[lin,col] <<- (NMR_MC[lin,col])/(s)
    }
  }

  norm_test(p_value,m=NMR_Pareto)

}


##################### Square root ##########################
.sqr_norm <- function() {

  CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
  matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
  NMR_sqr <<- sqrt(matr_cor)
  norm_test(p_value,m=NMR_sqr)

}


##################### Inverse Square root ##########################
.isqr_norm <- function() {
  CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
  matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
  NMR_i_sqr <<- 1/sqrt(matr_cor)
  norm_test(p_value,m=NMR_i_sqr)

}


##################### Log ##########################
.log_norm <- function() {

  CS_sel_real <<- CS_selection$vranges[order(CS_selection$vranges,decreasing = TRUE)]
  matr_cor <<- matrix(data = NMRData[,col_select],dim(NMRData)[1], length(CS_sel_real))
  NMR_log <<- log(matr_cor)
  norm_test(p_value,m=NMR_log)

}

##################### Start STOCSY-I #################
.s_stocsy <-  function(matr) {
  cor_cutoff_p <<- 0.9
  cor_cutoff_n <<- -0.9

  if (!(sel_ind == 0)) {
    col_select <<- col_select[order(col_select)]
    cor_pearson <<- cor(matr[,])
    drv_pk <<- which.max(matr[1,])
    rr <<- vector(mode="character")
    norm_test(p_value,matr)
    r_critical(p_value)

    updateSliderInput(session, "cutoff_stocsy_i", min = -1,
                      max = 1, value = c(-0.9,0.9) , step= 0.01)


    for (k in 1:dim(NMRData)[2]) {

      if (k %in% col_select) {
        z <<- which(col_select[] == k)


        if (cor_pearson[drv_pk,z] >= cor_cutoff_p) {
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

}
