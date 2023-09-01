 #ui para a aba formulário (30-ago-2023, 17:06h)

 tabPanel(title = 'Formulário',
 
  tags$div(class = 'container',
    tags$form(
    h4('Rumor'),
    br(),
    fluidRow(
     
      column(4,
      dateInput('rumor_data', 'Data:', format = 'dd-mm-yyyy', language = 'pt-BR')
     ),
      column(4,
      checkboxInput('rumor_checkdata', "Atualização data?", value = F),
      uiOutput('rumor_ui_data_atual')),
      column(4,
      uiOutput('rumor_se'))
     ), #endrow

     fluidRow(
       column(12,
        textAreaInput('rumor_infoadicional', 'Descrição:', width = '100%', height = '80px',
         resize = 'vertical') ),
       column(12,
        textInput('rumor_link', 'Link:', width = '100%')
       )  
      ), #endrow

       hr(),

      fluidRow(
       column(4,
        selectInput('rumor_doenca', 'Doença:', choices = doenca, selected = NA, multiple = F),
        selectInput('rumor_notif_imediata', 'Notificação imediata?', choices = c('Não', 'Sim'), selected = 'Não', multiple = F)
        ),
       column(4,
       selectInput('rumor_area_tecnica', 'Área técnica:', choices = area_tecnica, selected = NA, multiple = F),
       selectInput('rumor_fonte', 'Fonte:', choices = fonte, selected = 'CIEVS Nacional', multiple = F)
       ), 
       column(4,
       selectInput('rumor_pais', 'País:', choices = centroide[!is.na(centroide$pais),'pais'], selected = 'Brasil', multiple = F),
       uiOutput('rumor_estado_ui'),
       uiOutput('rumor_municipio_ui')
       )

      ), #endRow

    hr(),
   
    fluidRow(
     
     column(4,
      textInput('rumor_casos', 'Total de casos:'),
      textInput('rumor_casos_conf', 'Nº casos confirmados:')
      ), #end column
     column(4,
      textInput('rumor_suspeitos', 'Nº casos suspeitos:'),
      textInput('rumor_obitos', 'Nº óbitos:')
      ), #end column
     column(4,
      textInput('rumor_descartados', 'Nº casos descartados:')
      ) #end column
      
      ), #endrow
    hr(),
    br(),
    fluidRow(
    column(4, offset = 4,
    actionButton('rumor_enviar', 'Confirmar\npreenchimento', width  = '100%')
    )
     ), #endrow
     br()
     
     ) #end form
     )#end divcontainer
     ) #endtabPanel rumoruência
     