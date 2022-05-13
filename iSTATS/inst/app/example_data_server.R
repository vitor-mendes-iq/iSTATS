

observeEvent(input$example_but,{

   file_names <<- iSTATS::file_names
   CS_values_real <<- iSTATS::CS_values_real
   NMRData <<- iSTATS::NMRData
   NMRData_plot <<- NMRData
   NMRData <<- NMRData + (abs(min(NMRData))+1)


   refreshval()



   updateSelectInput(session, "spectrum_list_multi", choices = file_names[])

   updateSelectInput(session, "spectrum_list_stocsy_i", choices = file_names[])

   updateSelectInput(session, "spectrum_list_stocsy_is", choices = file_names[])

   updateSelectInput(session, "spectrum_list_stocsy_rt", choices = file_names[])
   #
   # if (NMRData_plot[1,] <= -100 ) {
   #    showModal(modalDialog(
   #       title = "Warning!!!",
   #       "Please apply the apodization functions like FT, ABS and APK and import again",
   #       easyClose = TRUE,
   #       footer = modalButton("Close"),
   #       size = "l"
   #    ))
   # }

   updateTabsetPanel(session, "main_bar", "Plot Spectra")


})

