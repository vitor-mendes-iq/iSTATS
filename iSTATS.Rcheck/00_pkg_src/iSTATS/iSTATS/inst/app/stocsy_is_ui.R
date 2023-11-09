  fluidPage(theme = 'zoo.css',

    fluidRow(

        column(3,align = "center",
               sidebarPanel(width = 12,

           radioButtons("radio_is", label = h5("Correlation Type"),selected = 1,
                                       choices = list("Pearson" = 1, "Spearman" = 2))
          )),

        column(9,align = "center",
               sidebarPanel(width = 12,

         selectInput("spectrum_list_stocsy_is","Which spectrum do you want to see?",file_names[], selectize = FALSE),

         p(strong("To perform STOCSY scaling, double click on the desired driver peak and then click on the button",em("START STOCSY-is"),"at the left menu.")))

        )),

      fluidRow(align = "center",

         column(3, align="center",
          sidebarPanel(width = 12,

           fluidRow(

             sliderInput("cutoff_stocsy_is", label = h5("Correlation Cutoff"), min = 0,width = '250px',
                         max = 1, value = 0.9 , step= 0.01)
            ),

           fluidRow(

             tags$button(id = "stocsy_s",
                         class = "btn action-button",
                         tags$img(src = "s_stocsy_is.png",
                                  height = "35px", width = "40px")
                            )
            ),

           br(),

           fluidRow(

             tags$button(id = "x2_stocsy_is",
                         class = "btn action-button",
                         tags$img(src = "bx2.png",
                                  height = "35px", width = "40px")

             ),

             tags$button(id = "x8_stocsy_is",
                         class = "btn action-button",
                         tags$img(src = "bx8.png",
                                  height = "35px", width = "40px")
              )
             ),

           fluidRow(div(style="height:5px")),

             fluidRow(

               tags$button(id = "q2_stocsy_is",
                           class = "btn action-button",
                           tags$img(src = "bq2.png",
                                    height = "35px", width = "40px")
                ),

               tags$button(id = "q8_stocsy_is",
                           class = "btn action-button",
                           tags$img(src = "bq8.png",
                                    height = "35px", width = "40px")
                            )
               ),

           br(),


           fluidRow(

             tags$button(id = "s_left_stocsy_is",
                         class = "btn action-button",
                         tags$img(src = "s_left.png",
                                  height = "35px", width = "40px")

              ),

             tags$button(id = "s_right_stocsy_is",
                         class = "btn action-button",
                         tags$img(src = "s_right.png",
                                  height = "35px", width = "40px")
              )
             ),

           fluidRow(div(style="height:5px")),

             fluidRow(

               tags$button(id = "s_left_f_stocsy_is",
                           class = "btn action-button",
                           tags$img(src = "s_left_f.png",
                                    height = "35px", width = "40px")

                ),

               tags$button(id = "s_right_f_stocsy_is",
                           class = "btn action-button",
                           tags$img(src = "s_right_f.png",
                                    height = "35px", width = "40px"))),


           br(),


           fluidRow(

             tags$button(id = "all_stocsy_is",
                         class = "btn action-button",
                         tags$img(src = "all.png",
                                  height = "35px", width = "40px")

              ),

             br()

             ),
           br(),

           fluidRow(

             selectInput("data_input_is",label="File extension", width = '100px',
                         choices=c("png","tiff","jpeg","pdf")),
             bsTooltip("data_input_is", "Select type file to download plot",
                       "right", options = list(container = "body")),
             # ),

             # column(4, align="center",
             #        fluidRow(div(style="height:5px")),

             sliderInput("slide_dpi_is", label= 'DPI' ,min = 100, width = '250px',
                         max = 1000, value = 200 , step= 10),
             bsTooltip("slide_dpi", "Select dpi to download plot",
                       "right", options = list(container = "body")),
             # ),

             # column(4, align="center",

             downloadButton("plot_download_is","Download Plot", width = '100px',
                            class = "btn shiny-download-link")
           )
           )),



         column(9, align="center",

                sidebarPanel(width = 12,


           plotOutput("plot_stocsy_is", width = "100%", height = "500px", click = "click_stocsy_is", dblclick = "dblclick_stocsy_is",
                      brush = brushOpts(id = "plot_brush_stocsy_is",delay = 5000, fill = "#ccc", direction = "xy", resetOnNew = TRUE)

            )


           )
      ))
  )

