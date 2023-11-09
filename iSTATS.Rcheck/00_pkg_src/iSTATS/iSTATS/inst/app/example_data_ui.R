  (fluidPage(theme = 'zoo.css',

    fluidRow(align = "center",

      fluidRow(div(style="height:20px")),

      fluidRow( tags$b("Click on the below button to load an example data:")),

      fluidRow(div(style="height:20px")),

        tags$button(id = "example_but",
                    class = "btn action-button btn_lmax",
                    tags$img(src = "example.png",
                             height = "50px", width = "50px")

                    )


           )))
