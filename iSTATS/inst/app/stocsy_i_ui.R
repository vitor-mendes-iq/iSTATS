 fluidPage(theme = 'zoo.css',
    fluidRow(align="center",
      column(3,align = "center",
             sidebarPanel(width = 12,
        radioButtons("radio", label = h5("Correlation Type"), selected = 1,
                     choices = list("Pearson" = 1, "Spearman" = 2)
        )
        )),
      column(9,align = "center",
             sidebarPanel(width = 12,
      selectInput("spectrum_list_stocsy_i","Which spectrum do you want to see?",file_names[], selectize = FALSE),
      p(strong("To perform STOCSY analysis, double click on the desired driver peak in the spectrum below."))
      )
    )
      ),

    fluidRow(

    column(3, align="center",

    sidebarPanel(width = 12,

      fluidRow(align = "center",



        fluidRow(


          sliderInput("cutoff_stocsy_i", label = h5("Correlation Cutoff"), min = -1, width = '250px',
                      max = 1, value = c(-0.9,0.9) , step= 0.01),
                      bsTooltip("cutoff_stocsy_i", "Set the minimum correlation coefficients",
                                "right", options = list(container = "body"))
        ),

        fluidRow(div(style="width:100px;", textInput("pv", "p-value", "0.05")
                    )),

        fluidRow(div(style="width:250px;", verbatimTextOutput("r_critical"),
                                    bsTooltip("r_critical", "R critical is calculate based on t-Student table, for the chosen p-value.",
                                              "right", options = list(container = "body")))),
        fluidRow(div(style="height:5px")),

        fluidRow(

          tags$button(id = "x2_stocsy_i",
                      class = "btn action-button",
                      tags$img(src = "bx2.png",
                               height = "35px", width = "40px")
          ),

          tags$button(id = "x8_stocsy_i",
                      class = "btn action-button",
                      tags$img(src = "bx8.png",
                               height = "35px", width = "40px")
                           )
          ),

           fluidRow(div(style="height:5px")),

             fluidRow(

                tags$button(id = "q2_stocsy_i",
                             class = "btn action-button",
                             tags$img(src = "bq2.png",
                                      height = "35px", width = "40px")
                ),

                tags$button(id = "q8_stocsy_i",
                            class = "btn action-button",
                            tags$img(src = "bq8.png",
                                     height = "35px", width = "40px")
                           )
                ),

            br(),

            fluidRow(

              tags$button(id = "s_left_stocsy_i",
                          class = "btn action-button",
                          tags$img(src = "s_left.png",
                                   height = "35px", width = "40px")
                           ),

                           tags$button(
                             id = "s_right_stocsy_i",
                             class = "btn action-button",
                             tags$img(src = "s_right.png",
                                      height = "35px", width = "40px")
                           )
              ),

            fluidRow(div(style="height:5px")),

            fluidRow(

              tags$button(id = "s_left_f_stocsy_i",
                          class = "btn action-button",
                          tags$img(src = "s_left_f.png",
                                   height = "35px", width = "40px")
              ),

              tags$button(id = "s_right_f_stocsy_i",
                          class = "btn action-button",
                          tags$img(src = "s_right_f.png",
                                   height = "35px", width = "40px")
               )
              ),

             br(),

             fluidRow(

               tags$button(id = "all_stocsy_i",
                           class = "btn action-button",
                           tags$img(src = "all.png",
                           height = "35px", width = "40px")),
                           bsTooltip("all_stocsy_i", "Show all spectrum",
                                     "right", options = list(container = "body")),

    ),

             br(),




          fluidRow(

            selectInput("data_input",label="File extension", width = '100px',
                        choices=c("png","tiff","jpeg","pdf")),
            bsTooltip("data_input", "Select type file to download plot",
                      "right", options = list(container = "body")),
            # ),

            # column(4, align="center",
            #        fluidRow(div(style="height:5px")),

            sliderInput("slide_dpi", label= 'DPI' ,min = 100, width = '250px',
                        max = 1000, value = 200 , step= 10),
            bsTooltip("slide_dpi", "Select dpi to download plot",
                      "right", options = list(container = "body")),
            # ),

            # column(4, align="center",

            downloadButton("plot_download","Download Plot", width = '100px',
                           class = "btn shiny-download-link")
            ))



        )),


        column(9, align="center",
               sidebarPanel(width = 12,

          plotOutput("plot_stocsy_i", width = "100%", height = "500px", click = "click_stocsy_i", dblclick = "dblclick_stocsy_i",
                     brush = brushOpts(id = "plot_brush_stocsy_i",delay = 5000,
                     fill = "#ccc", direction = "xy", resetOnNew = TRUE)

           ),

        ))
    )




  )







