## ui.R ##

# ui <- dashboardPage(
#         dashboardHeader(title = "Basic dashboard"),
#         dashboardSidebar(),
#         dashboardBody(
#           # Boxes need to be put in a row (or column)
#           fluidRow(
#             box(plotOutput("plot1")),
#             
#             box(
#               valueBoxOutput(),
#               
#               title = "Controls",
#               sliderInput("slider", "Number of observations:", 1, 100, 50)
#             )
#           ),
#           fluidRow(
#             box(
#                 plotOutput('plot')
#                 ),
#             box(
#               collapsibleTreeOutput('collaps_tree'),
#               tags$p("The node you most recently clicked:"),
#               textOutput("str")
#             )
#         
#           )
#         )
#      )

## ui.R ##
header  <- dashboardHeader(title = ""
                           
)

sidebar <- dashboardSidebar(
  sidebarMenu(id="sbmenu",
              menuItem("Automatico", icon('building')
                        
              ),
              menuItem("Manual", icon('shopping-cart')
                       
              ),
              menuItem("Referencias",
                       sliderInput("slider", 
                                   "Number of observations:", 1, 100, 50)
                       )
              )
  )
  




# sidebar <- dashboardSidebar(
#   sidebarMenu(id="sbmenu",menuItem("Segmentos",
#                                    menuItem("EMP2",icon = icon('building'),
#                                             menuSubItem("Desconto Duplicata",
#                                                         tabName = "desc_dup2",
#                                                         icon = icon('shopping-cart')
#                                             ),
#                                             menuSubItem("Giro Duplicata",
#                                                         tabName = "giro_dup2",
#                                                         icon = icon('shopping-cart')
#                                             ),
#                                             menuSubItem("Veiculos Leves",
#                                                         tabName = "veic_lev2",
#                                                         icon = icon('shopping-cart')
#                                             )
#                                    ),
#                                    menuItem("EMP3",icon = icon('building'),
#                                             menuSubItem("Desconto Duplicata",
#                                                         tabName = "desc_dup3",
#                                                         icon = icon('shopping-cart')
#                                             ),
#                                             menuSubItem("Giro Duplicata",
#                                                         tabName = "giro_dup3",
#                                                         icon = icon('shopping-cart')
#                                             ),
#                                             menuSubItem("Veiculos Leves",
#                                                         tabName = "veic_lev3",
#                                                         icon = icon('shopping-cart')
#                                             )
#                                    ),
#                                    menuItem("EMP4",icon = icon('building'),
#                                             menuSubItem("Desconto Duplicata",
#                                                         tabName = "desc_dup4",
#                                                         icon = icon('shopping-cart')
#                                             ),
#                                             menuSubItem("Giro Duplicata",
#                                                         tabName = "giro_dup4",
#                                                         icon = icon('shopping-cart')
#                                             ),
#                                             menuSubItem("Veiculos Leves",
#                                                         tabName = "veic_lev4",
#                                                         icon = icon('shopping-cart')
#                                             )
#                                    )
#   ),
#   menuItem("Referencias",
#            sliderInput("slider", 
#                        "Number of observations:", 1, 100, 50)
#   )
#   )
# )

body    <- dashboardBody(
    h3("CALCULADORA DE IMPACTOS"),
    fluidRow(
      mainPanel(width=6,
                box(width=12,
                    height = 800,
                    tabsetPanel(
                      tabPanel("Automático",
                               hr(),
                               radioButtons("perf_rbtn", 
                                            "Performance:",
                                            c("BR Física" = "perf_fis",
                                              "BR Financeira" = "perf_fin",
                                              "Demografia" = "cnt_qtd_cnpj"),
                                            inline   = TRUE
                               ),
                               # hr(),
                               plotOutput("plot1")
                      ),
                      tabPanel("Manual", 
                               verbatimTextOutput("summary")
                      )
                    )
                )
          
      ),
      box(width=6,
          height = 800,
          # hr(),
          selectInput("var_inpt", 
                      "Selecione uma variável:",
                      names(df)[!names(df) %in% c('pk_dt','pk_cd_cnpj','score','score_bins','vl_risco','vl_contr','vl_perf','perf')]
          ),
          radioButtons("perf2_rbtn", 
                       "Performance:",
                       c("BR Física"        = "perf_fis",
                         "BR Financeira"    = "perf_fin",
                         "Risco"            = "vl_risco",
                         "Valor Contratado" = "vl_contr",
                         "Demografia"       = "cnt_qtd_cnpj"),
                       inline   = TRUE
          ),
          box(width=11,
              # height = 800,
            DTOutput('tbl_train')
          ),
          box(width=11,
              # height = 800,
              
              DTOutput('tbl_test')
          ),
          
          hr()
      )
      # column(width=6,
      #     
      # ),
      # column(width=6,
      #   box(height = 800,
      #       hr(),
      #       #DTOutput('tbl_train'),
      #       #DTOutput('tbl_test'),
      #       hr()
      #   )
      # )
    )
)


# body    <- dashboardBody(
#   tabItems(
#     # DESCONTO DUPLICATA
#     tabItem("desc_dup2",
#             h1("Desconto Duplicata EMP2"),
#             fluidRow(
#               valueBoxOutput("cliBox"),
#               valueBoxOutput("prodBox"),
#               valueBoxOutput("overBox")
#             ),
#             fluidRow(
#               box(height = 500,
#                   plotOutput("plot1")
#               ),
#               box(height = 1200,
#                   # hr(),
#                   # DTOutput('tbl'),
#                   hr(),
#                   fluidRow(
#                     column(6, actionButton('addFilter', 'Adicionar Recorte')),
#                     offset = 6
#                   ),
#                   tags$hr(),
#                   
#                   tags$div(id = 'placeholderFilter'),
#                   # div(style="display:inline-block",actionButton('removeBtn', '-'),actionButton('insertBtn', '+'), style="float:right"),
#                   # tags$div(id = 'placeholder'),
#                   div( style="float:right",downloadButton('downloadData', 'Download Data'))
#                   
#               ) 
#             ),
#             fluidRow(
#               box(height = 500,
#                   plotOutput('plot')
#                   
#               )
#               # box(height = 500,
#               #     hr(),
#               #     collapsibleTreeOutput('collaps_tree'),
#               #     tags$p("The node you most recently clicked:"),
#               #     textOutput("str"),
#               #     hr()
#               #   
#               # )
#             )
#     ),
#     tabItem("desc_dup3",
#             h1("Desconto Duplicata EMP3")
#             
#     ),
#     tabItem("desc_dup4",
#             h1("Desconto Duplicata EMP4")
#             
#     ),
#     # GIRO DUPLICATA
#     tabItem("giro_dup2",
#             h1("Giro Duplicata EMP2")
#             
#     ),
#     tabItem("giro_dup3",
#             h1("Giro Duplicata EMP3")
#             
#     ),
#     tabItem("giro_dup4",
#             h1("Giro Duplicata EMP4")
#             
#     ),
#     # Veiculos Leves
#     tabItem("veic_lev2",
#             h1("Veiculos Leves EMP2")
#             
#     ),
#     tabItem("veic_lev3",
#             h1("Veiculos Leves EMP3")
#             
#     ),
#     tabItem("veic_lev4",
#             h1("Veiculos Leves EMP4")
#             
#     )
#   )
# )









ui <- dashboardPage(
  header,
  sidebar,
  body
)