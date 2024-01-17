 #aba para verificação do rumor (12-jan-2024, 14:35h)

 tabPanel(title = 'Verificação de rumor',
 
 tags$div(id = 'verifica', class = 'container',
        fluidRow(
            column(8,
                h4('Rumores em Verificação'),
                uiOutput('verific_lista_rumor') 
            ),
            column(4,
                uiOutput('verific_area_tecnica')
                
            )

        ), #endRow
        hr(),
        uiOutput('verific_opcoes'),
       
    br(),
    fluidRow(
    column(4, offset = 4,
    actionButton('verific_enviar', 'Confirmar\npreenchimento', width  = '100%')
    )
     ), #endrow
     br()
        ) #end div verifica
     ) #endtabPanel verific 
     
  