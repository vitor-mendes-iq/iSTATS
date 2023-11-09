  fluidPage(theme = 'zoo.css',

     sidebarPanel(width = 12,

      fluidRow(div(style="width:300px;",align="center",
      selectInput("spectrum_list_multi","Which spectrum do you want to see?", file_names[], selectize = FALSE)

    )),

      fluidRow(

        tags$button(id = "x2_multi",
                    class = "btn action-button",
                    tags$img(src = "bx2.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "x8_multi",
                    class = "btn action-button",
                    tags$img(src = "bx8.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "q2_multi",
                    class = "btn action-button",
                    tags$img(src = "bq2.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "q8_multi",
                    class = "btn action-button",
                    tags$img(src = "bq8.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "all_multi",
                    class = "btn action-button",
                    tags$img(src = "all.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "s_left_multi",
                    class = "btn action-button",
                    tags$img(src = "s_left.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "s_right_multi",
                    class = "btn action-button",
                    tags$img(src = "s_right.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "s_left_f_multi",
                    class = "btn action-button",
                    tags$img(src = "s_left_f.png",
                             height = "35px", width = "40px")
        ),

        tags$button(id = "s_right_f_multi",
                    class = "btn action-button",
                    tags$img(src = "s_right_f.png",
                             height = "35px", width = "40px")
        )

      )
    ),

    sidebarPanel(width = 12,

    fluidRow(align = "center",

      plotly::plotlyOutput("plot_multi")


    ))



  )
