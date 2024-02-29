 # tabs diversos para configuração do sistema (27-fev-2024, 16:39h)
 navbarMenu('Configurações', icon = icon('gear'),
 
  tabPanel(title = 'Agravos/Doenças',
  
   tags$div(class = 'container',
    
    tags$div(class = 'alert alert-secondary',
    'Aba para edição da lista de agravos/doenças disponíveis'
    ),
    br(),
    verbatimTextOutput('testei'),
    fluidRow(
        column(3,
        radioButtons(inputId  = 'config_opçoes',  choices = c('Adicionar', 'Remover'),label =   NULL, selected = 'Adicionar')
        ),
        column(4,
            tags$div(class = 'card',
                tags$div(class = 'card-body',
                 uiOutput('config_edit')
                )
            )
        )) #endrow
    
    
     
     
     )
     
  ) #end tabPanel Doenças 



 ) #end navbarMenu