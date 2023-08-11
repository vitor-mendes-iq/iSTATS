fluidPage(theme = 'zoo.css',
          fluidRow(align = "center",
                   column(2, align="center",

                          sidebarPanel(width = 12,

                          fluidRow(
                            actionButton("mean_norm", "Mean-Centering"),
                            fluidRow(div(style="height:5px")),
                            actionButton("scaling_norm", "Autoscaling"),
                            fluidRow(div(style="height:5px")),
                            actionButton("pareto_norm", "Pareto"),
                            fluidRow(div(style="height:5px")),
                            actionButton("sqr_norm", "Square root"),
                            fluidRow(div(style="height:5px")),
                            actionButton("i_sqr_norm", "Inverse square root"),
                            fluidRow(div(style="height:5px")),
                            actionButton("log_norm", "Log"),
                            fluidRow(div(style="height:5px")),
                            fluidRow(div(style="height:5px"))
                          ),




                        fluidRow(

                          fluidRow(div(style="width:150px;",textInput("pv_n", "p-value", "0.05"))),
                          fluidRow(div(style="width:150px;",verbatimTextOutput("norm_cond"))),
                          fluidRow(
                            tags$button(id = "s_stocsy_i_norm",
                                        class = "btn action-button btn_t",
                                        tags$img(src = "s_stocsy_.png",
                                                 height = "35px", width = "40px")),
                            bsTooltip("s_stocsy_i_norm", "Start STOCSY-I on normalize data",
                                      "right", options = list(container = "body")),


                          ))),

                          sidebarPanel(width = 12,

                          fluidRow(

                          downloadButton("downloadareas2",icon("sakod"),
                                         class = "btn btn-default shiny-download-link",
                                         tags$img(src = "download_buckets.png",
                                                  height = "35px", width = "40px")),
                                          bsTooltip("downloadareas2", "Download selected areas normalized",
                                                    "right", options = list(container = "body"))),
                          fluidRow(div(style="height:5px")),
                          fluidRow(

                          downloadButton("downloadpoints2", icon("sakod"),
                                         class = "btn btn-default shiny-download-link",
                                         tags$img(src = "download_points.png",
                                                  height = "35px", width = "40px")),
                                         bsTooltip("downloadpoints2", "Download selected points normalized",
                                                   "right", options = list(container = "body"))),
                          fluidRow(div(style="height:5px")),
                          fluidRow(

                            tags$button(id = "norm_print_PDF",
                                        class = "btn action-button",
                                        tags$img(src = "exp_stocsy.png",
                                                 height = "35px", width = "40px")),
                            bsTooltip("norm_print_PDF", "Download PDF",
                                      "right", options = list(container = "body")),
                          ))


                   ),


                   column(10, align="center",

                          sidebarPanel(width = 12,

                          fluidRow(
                            fluidRow(p(strong("Non-normal regions will be in red and normal regions in green."))),
                            plotOutput("plot_norm", height = "450px",click = "sel_click", dblclick = "plot_dblclick",
                                       brush = brushOpts(id = "plot_brush_norm",delay = 5000, fill = "#ccc", direction = "xy", resetOnNew = TRUE)
                            )
                          )
                   ))
))
