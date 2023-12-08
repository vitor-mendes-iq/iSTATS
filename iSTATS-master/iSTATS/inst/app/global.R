
dc <- c()
file_names <- c()
NMRData <- matrix(0, 2,2)
CS_values_real <- matrix(0, 2,2)
R_CRITICAL <<- 0.8
matr_cor <- matrix(seq(1,10,1), 10,10)
NMRData_plot <- matrix(seq(1,10,1), 10,10)
p_value <<- 0.05
choose_norm <<- 1
stop_menssager <<- 0

# não esquecer de voltar matr_cor
onStop(function() {

  message("Stopping iSTATS ...\n")

  clear_list <- c("stop_menssager","n_dpi","rr_nn","norm_var","col_sel_norm2","yinf","NMRData_plot","peran","rows_temp","p_value","choose_norm","testy_norm","NMR_sqr",
                  "NMR_Pareto","NMR_MC","NMR_log","NMR_i_sqr","NMR_ASC","matr_cor","s_stoc","rr2",
                  "rowns_temp","dif","cor_cutoff_value_p","cor_cutoff_value_n","col_sel_norm",
                  "test_norm","R_CRITICAL","rr_n","CS_norm","CS_selection","gg","gr","buma","CS_values_real",
                  "NMRData","NMRData_temp", "facts","facts_is","facts_rt","Scaling_cor", "up2", "up","up3",
                  "testy","testy_multi","testy_stocsy_i","alr_click","upfile3",
                  "chkzoom","chkzoom_multi","chkzoom_stocsy_i","chkzoom_stocsy_is",
                  "chkzoom_stocsy_rt", "col_select","cor_cutoff","cor_cutoff_i",
                  "cor_cutoff_rt","dc","exp_click","exran","file_names","idb",
                  "idb_multi","idb_stocsy_i","idb_stocsy_is","idb_stocsy_rt",
                  "ncor","ncor_rt","ndpi","ndpis","peran_multi","peran_stocsy_i",
                  "peran_stocsy_is","peran_stocsy_rt","rr","rr_is","rr_rt","sel_ind",
                  "value","cor_pearson","cor_spearman","matr_selec",
                  "pos_map","reg_selec","up","col_select_2","CS_sel_real",
                  "CS_values","drv_pk","file_names_full","file_names_full",
                  "peran_sel","s_ind","z","das_multi","das_stocsy_i","das_stocsy_is",
                  "das_stocsy_rt","drv_pk_ois","drv_pk_oi","drv_pk_ort","s","tryton", "old_sel", "regions_sel")

suppressWarnings(
  suppressMessages({
    garb <- try(sapply(clear_list, function(x) if (exists(x, envir = .GlobalEnv)) rm(list = x, envir = .GlobalEnv)), silent = TRUE)
    rm(garb)
  })
)

stopApp()

})
