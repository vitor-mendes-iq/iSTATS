  fluidPage(theme = 'zoo.css',

    fluidRow(div(style="height:20px"),

    tags$head(tags$style(type="text/css", "
                         #loadmessage {
                         position: fixed;
                         top: 65px;
                         left: 25px;
                         width: 95%;
                         padding: 5px 0px 5px 0px;
                         text-align: center;
                         font-weight: bold;
                         font-size: 100%;
                         color: #ffffff;
                         background-color: #000000;
                         z-index: 1;
                         }
                           ")),

     conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                     tags$div("Loading...",id="loadmessage"))),


     fluidRow(

       column(6,
          sidebarPanel(width = 12,

          fluidRow(align = "center",

            tags$button(id = "x2_sel1",
                        class = "btn action-button btn_lmax",
                        tags$img(src = "bx2.png",
                                 height = "35px", width = "40px")

             ),

            tags$button(id = "x8_sel1",
                        class = "btn action-button btn_r",
                        tags$img(src = "bx8.png",
                                 height = "35px", width = "40px")

            ),

            tags$button(id = "exp_sel1",
                        class = "btn action-button btn_l",
                        tags$img(src = "exp.png",
                                 height = "35px", width = "40px")

            ),

            tags$button(id = "cont_sel1",
                        class = "btn action-button btn_r",
                        tags$img(src = "cont.png",
                                 height = "35px", width = "40px")

            ),

            tags$button(id = "s_left_sel1",
                        class = "btn action-button btn_l",
                        tags$img(src = "s_left.png",
                                 height = "35px", width = "40px")

            ),

            tags$button(id = "s_right_sel1",
                        class = "btn action-button btn_rmax",
                        tags$img(src = "s_right.png",
                                 height = "35px", width = "40px"))



            ),

          fluidRow(div(style="height:5px")),


            fluidRow(align = "center",

               tags$button(id = "q2_sel1",
                           class = "btn action-button btn_lmax",
                           tags$img(src = "bq2.png",
                                    height = "35px", width = "40px")
               ),

               tags$button(id = "q8_sel1",
                           class = "btn action-button btn_r",
                           tags$img(src = "bq8.png",
                                    height = "35px", width = "40px")

               ),

               tags$button(id = "rsh_sel1",
                            class = "btn action-button btn_l",
                            tags$img(src = "rsh.png",
                                     height = "35px", width = "40px")
                ),

               tags$button(id = "exc_reg",
                            class = "btn action-button btn_r",
                            tags$img(src = "exc_reg.png",
                                     height = "35px", width = "40px")
               ),

               tags$button(id = "s_left_f_sel1",
                            class = "btn action-button btn_l",
                            tags$img(src = "s_left_f.png",
                                     height = "35px", width = "40px")

                            ),

               tags$button(id = "s_right_f_sel1",
                           class = "btn action-button btn_rmax",
                           tags$img(src = "s_right_f.png",
                                    height = "35px", width = "40px")
                            )

                   ))),

       column(1,align = "left",



              radioButtons("radio_s", label = h5("Spectrum Type"), selected = 1,
                           choices = list("Average" = 1, "Overlap" = 2))),






    column(5,align = "center",div(style="line-height: 100px; vertical-align: center",
       sidebarPanel(width = 12,


       tags$button(id = "sel_cor",
                   class = "btn action-button btn_t",
                   tags$img(src = "sel_cor.png",
                            height = "35px", width = "40px")),
                   bsTooltip("sel_cor", "Save selected region",
                             "right", options = list(container = "body")),

       tags$button(id = "s_stocsy",
                   class = "btn action-button btn_t",
                   tags$img(src = "s_stocsy_.png",
                            height = "35px", width = "40px")),
                   bsTooltip("s_stocsy", "Start STOCSY-I on raw data",
                            "right", options = list(container = "body")),

        downloadButton("downloadareas",icon("sakod"),
                      class = "btn btn-default shiny-download-link",
                      tags$img(src = "download_buckets.png",
                      height = "35px", width = "40px")),
                      bsTooltip("downloadareas", "Download selected areas",
                                "right", options = list(container = "body")),


        downloadButton("downloadpoints", icon("sakod"),
                      class = "btn shiny-download-link",
                      tags$img(src = "download_points.png",
                      height = "35px", width = "40px")),
                      bsTooltip("downloadpoints", "Download selected points",
                               "right", options = list(container = "body")),
       
       downloadButton("downloadpoints_save", icon("sakod"),
                      class = "btn shiny-download-link",
                      tags$img(src = "save_selected.png",
                               height = "35px", width = "40px")),
       bsTooltip("downloadpoints", "Download selected points",
                 "right", options = list(container = "body"))
       
       


        )

              ))),

    fluidRow(div(style="height:15px")),


      fluidRow(

        column(6, align="center",
          sidebarPanel(width = 12,

          fluidRow(p(strong("Select a region to be expanded at the right plot."))),



          plotOutput("plot1", height = "450px",click = "sel_click", dblclick = "plot_dblclick",
                     brush = brushOpts(id = "plot_brush",delay = 5000, fill = "#ccc", direction = "xy", resetOnNew = TRUE))
        )),

        column(6, align="center",
          sidebarPanel(width = 12,

           fluidRow(p(strong("Select each region of interest and click on top left button to save it, then click on START."))),

           plotOutput("plot2", height = "450px", click = "cor_click", dblclick = "cor_dblclick",
                        brush = brushOpts(id = "sel_brush",delay = 5000, fill = "#ccc", direction = "x", resetOnNew = TRUE))



        )
      ))

)


