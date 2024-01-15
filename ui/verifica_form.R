 #aba para verificação do rumor (12-jan-2024, 14:35h)

 tabPanel(title = 'Verificação de rumor',
 
 tags$div(id = 'verifica', class = 'container',
        fluidRow(
            column(6,
                h4('Rumores em Verificação'),
                uiOutput('verific_lista_rumor') 
            ),
            column(4,
                uiOutput('verific_area_tecnica')
                
            )

        ), #endRow
        
        uiOutput('verific_opcoes')
        ) #end div verifica
     ) #endtabPanel verific 
     
  