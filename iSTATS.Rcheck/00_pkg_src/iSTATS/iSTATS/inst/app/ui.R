
fluidPage(title="iSTATS",
  fluidRow(
    column(12, align="center",
           navbarPage(id = "main_bar",title = div(style = "vertical-align: top",img(src="iSTATS_logo.png", heigth = "80px",
                                                                                    width = "80px")), selected = "Home", inverse = T,
                      navbarMenu("Data",

                                  tabPanel("Import Bruker file - convbin2asc",
                                           uiOutput('imputa')),

                                  tabPanel("Import CSV file",
                                           uiOutput('imputa2')),

                                  tabPanel("Import Rdata file",
                                           uiOutput('imputa3')),

                                  tabPanel("Example Data",
                                          source('example_data_ui.R', local = T)[1])


                      ),

                      navbarMenu("Plot",

                                 tabPanel("Plot Spectra",
                                          source('plot_interativo_ui.R', local = T)[1])
                      ),

                      navbarMenu("Select",

                                 tabPanel("Select Signals",
                                          source('select_signals_ui.R', local = T)[1]
                                 ),

                                 tabPanel("Normalize Selection",
                                          source('Pretreatment_ui.R', local = T)[1]
                                 )
                      ),

                      navbarMenu("STOCSY",

                                 tabPanel("STOCSY-I",
                                          source('stocsy_i_ui.R', local = T)[1]
                                 ),

                                 tabPanel("STOCSY-IS",
                                          source('stocsy_is_ui.R', local = T)[1]
                                 ),

                                 tabPanel("STOCSY-RT",
                                          source('stocsy_rt_ui.R', local = T)[1]

                                 )
                      ),

                      navbarMenu("More",

                                 tabPanel("Home",
                                          source('home_ui.R', local = T)[1]),

                                 tabPanel("Contact",
                                          source('about_ui.R', local = TRUE)[1]
                                 )
                      )
           ),

           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            tags$div("Loading...",id="loadmessage")
           )
    )
  )
)
