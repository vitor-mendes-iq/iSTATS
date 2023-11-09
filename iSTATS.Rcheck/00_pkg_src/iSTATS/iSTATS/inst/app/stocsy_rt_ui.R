  fluidPage(theme = 'zoo.css',

    fluidRow(

      column(12, align="center",

        column(3,align = "left",

          radioButtons("radio_rt", label = h5("Correlation Type"),selected = 1,
                       choices = list("Pearson" = 1, "Spearman" = 2)

          )

        ),

        column(9,align = "center",

        selectInput("spectrum_list_stocsy_rt","Which spectrum do you want to see?",file_names[], selectize = FALSE),

        p(strong("To perform real-time STOCSY analysis, place the cursor on the desired driver peak."))

        )

      )
     ),

      fluidRow(align = "center",

         column(3, align="center",
                sidebarPanel(width =12,

           fluidRow(

              sliderInput("cutoff_stocsy_rt", label = h5("Correlation Cutoff"), min = 0,
                          max = 1, value = 0.9 , step= 0.1)
            ),

            fluidRow(

              tags$button(id = "x2_stocsy_rt",
                          class = "btn action-button",
                          tags$img(src = "bx2.png",
                                   height = "35px", width = "40px")
               ),

              tags$button(id = "x8_stocsy_rt",
                          class = "btn action-button",
                          tags$img(src = "bx8.png",
                                   height = "35px", width = "40px")
                           )
            ),

     fluidRow(div(style="height:5px")),

       fluidRow(

         tags$button(id = "q2_stocsy_rt",
                     class = "btn action-button",
                     tags$img(src = "bq2.png",
                              height = "35px", width = "40px")
          ),

          tags$button(id = "q8_stocsy_rt",
                      class = "btn action-button",
                      tags$img(src = "bq8.png",
                      height = "35px", width = "40px")
                           )
         ),

     br(),

     fluidRow(

       tags$button(id = "s_left_stocsy_rt",
                   class = "btn action-button",
                   tags$img(src = "s_left.png",
                            height = "35px", width = "40px")

        ),

       tags$button(id = "s_right_stocsy_rt",
                   class = "btn action-button",
                   tags$img(src = "s_right.png",
                            height = "35px", width = "40px")
                           )
      ),

     fluidRow(div(style="height:5px")),

     fluidRow(

       tags$button(id = "s_left_f_stocsy_rt",
                   class = "btn action-button",
                   tags$img(src = "s_left_f.png",
                            height = "35px", width = "40px")

        ),

       tags$button(id = "s_right_f_stocsy_rt",
                   class = "btn action-button",
                   tags$img(src = "s_right_f.png",
                            height = "35px", width = "40px"))),


     br(),

     fluidRow(

       tags$button(id = "all_stocsy_rt",
                   class = "btn action-button",
                   tags$img(src = "all.png",
                            height = "35px", width = "40px")

        )

      ),

     br(),
     fluidRow(

       tags$button(id = "stocsy_rt_print_PDF",
                   class = "btn action-button",
                   tags$img(src = "exp_stocsy.png",
                            height = "35px", width = "40px")),
       bsTooltip("stocsy_rt_print_PDF", "Download PDF",
                 "right", options = list(container = "body")),
     )

      )),


      column(9, align="center",

             sidebarPanel(width =12,

         plotOutput("plot_stocsy_rt", width = "100%", height = "500px", click = "click_stocsy_rt", dblclick = "dblclick_stocsy_rt",
                    brush = brushOpts(id = "plot_brush_stocsy_rt",delay = 5000,
                    fill = "#ccc", direction = "xy", resetOnNew = TRUE),hover = hoverOpts(id="hover_stocsy_rt", delay = 750, delayType = "debounce")
          )


      ))
    )
  )





