(fluidPage(theme = 'zoo.css',

  fluidRow(align = "center",

    fluidRow(div(style="height:30px")),





                    tags$button(
                      id = "choose",
                      class = "btn action-button btn_lmax",
                      tags$img(src = "csv_i.png",
                               height = "50px", width = "50px")
                    ),

                    fluidRow(div(style="height:30px")),

                    fluidRow( tags$b("Choose a minimum and a maximum frequency (ppm) for the working spectra:")),

                    fluidRow(div(style="height:15px")),

                    fluidRow(

                      column(5),

                      column(1, align="center",

                      textInput("ls_lims", "min","0.0", width = "150px")

                    ),


                      column(1, align="center",

                           textInput("hs_lims", "max","10.0", width = "150px")
                           ),

                      column(5)


                    )

                    )

                    ))

