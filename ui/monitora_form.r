 #aba para monitoramento do rumor (25-jan-2024, 14:19h)

 tabPanel(title = 'Monitoramento',

 
 tags$div(id = 'monitora', class = 'container',
        fluidRow(
            column(8,
                h4('Monitoramento do rumor'),
                uiOutput('monitora_lista_rumor') 
            ),
            column(4,
                uiOutput('monitora_acoes')
                
            )

        ), #endRow
        hr(),
        uiOutput('monitora_desfecho'),
        
       
    br(),
    fluidRow(
    column(4, offset = 4,
    actionButton('monitora_enviar', 'Confirmar\npreenchimento', width  = '100%')
    )
     ), #endrow
     br()
        ) #end div monitora
     ) #endtabPanel monitora 
     
  