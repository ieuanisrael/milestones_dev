# header footer


# header
header_row <- fluidRow(style = paste0("background-color: #002855; height: 120px; font-size: 30px; line-height:0.9; padding:10px;"),
                       column(3,
                              img(
                                src = 'www/cnsw logo.png',
                                align = "left",
                                height = 100,
                                width = 80 * 1
                              )
                       ),
                       column(6,
                              align = "center", 
                              strong(p(
                                span("Ball", style = 'color:#ffffff'),
                                span("Speeds", style = 'color:#00AEEF')
                              )),
                              strong(p(span("Performance", style = 'color:#ffffff; font-size: 18px'))),
                              img(
                                src = 'www/cnsw name.png',
                                align = "center",
                                height = 100 / 6.25,
                                width = 100 
                              )
                       ),
                       column(3,
                              img(
                                src = 'www/cnsw logo.png',
                                align = "right",
                                height = 100,
                                width = 80 * 1
                              )
                       )
)

# footer
footer_row <- fluidRow(style = paste0("background-color: #002855; height: 35px; padding: 10px;"),# height: 70px; font-size: 30px
                       column(6,
                              img(
                                src = 'www/cnsw name.png',
                                align = "left",
                                height = 100 / 6.25,
                                width = 100
                              )),
                       column(6,
                              p(span("Ieuan Israel | Powered by Ludis Analytics", style = 'color:#ffffff; font-size: 15px')),
                              align = "right"
                       )
)