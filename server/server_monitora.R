 #server monitoração rumores (15-jan-2024, 13:31h)

  monitora_rumores <- reactiveVal() 
  #lista de rumores a serem monitorados
  observeEvent(input$navbar == 'Monitora' | input$monitora_enviar,{
                    lista_evento <- DBI::dbGetQuery(conn(), "SELECT id, data_noticia, doenca, area_tecnica FROM rumores_evento WHERE
                                            id IN (SELECT id FROM rumores_verifica WHERE risc_avalia IN ('Alto', 'Muito alto')) AND
                                            id IN (SELECT id FROM rumores_monitora WHERE monitora_encerra = FALSE) 
                                            ")
                    lista_verific <- DBI::dbGetQuery(conn(), "SELECT id, risc_avalia FROM rumores_verifica 
                                            WHERE risc_avalia IN ('Alto', 'Muito alto') AND
                                            id IN (SELECT id FROM rumores_monitora WHERE monitora_encerra = FALSE)")
                    lista_monitoradas <- dplyr::left_join(lista_evento, lista_verific, by = 'id')
                     monitora_rumores(lista_monitoradas)
  })

  #-------------------------------------------------------------------------
  #uiOutputs
  #-------------------------------------------------------------------------
  
  #------------------------------------------
  # output da lista rumores monitoração
  output$monitora_lista_rumor <- renderUI({
     
     if(nrow(monitora_rumores()) == 0){
        tagList(
        br(),    
        h4('Não há rumores a serem monitorados')
        )
     }else{
        reactableOutput('monitora_lista')
     }
  })
  
  #saída tabela
  output$monitora_lista <- renderReactable({
    dadoi <- monitora_rumores()
    names(dadoi) <- c('id', 'Data Notícia', 'Agravo', 'Área técnica', 'Risco')
    reactable(dadoi, selection = "single", onClick = "select")
  })
  
  #------------------------------------
  # opções monitoração
  monitora_selected <- reactive({dadoi <- monitora_rumores()
                               linha <- getReactableState("monitora_lista", "selected")
                               dadoi[linha,'id']
                               }) 
  
  # output monitoracao

  output$monitora_acoes <- renderUI({
        if(length(monitora_selected()) == 0){NULL}else{
        tagList(
            checkboxGroupInput("monitora_lista_acoes", label = h3("Ações Realizadas"), 
                choices = list('Nota Informativa', 'Nota técnica', 'Alerta epidemiológico', 'COES', "Sala de situação")),
            selectizeInput('monitora_area_tecnica', 'Área Técnica envolvida', choices = equipes_monitora, selected = NULL, options = list(
                      onInitialize = I('function() { this.setValue(""); }'))),
            checkboxInput('monitora_desfecho', label = 'Rumor já tem desfecho?',  value = F) 
        )
        }

  })



  #botão monitora_enviar
  observe({
  #preencher o vazio com os in´puts de "outras opções"
  vazio <- isTRUE(input$monitora_agravo == '')

  shinyjs::toggleState("monitora_enviar", 
                    if(!isTRUE(input$monitora_desfecho)) {length(monitora_selected()) != 0}else{
                    ( length(monitora_selected()) != 0 & !isTRUE(vazio)) 
                    })
 })
  #------------------------------------
  #output opcoes

  output$monitora_desfecho_opcoes <- renderUI({
        if(!isTRUE(input$monitora_desfecho)){NULL}else{
            tagList(
                div(style = 'text-align: center;',
                h3('Desfecho')),
                    br(),
                div(class = "card border-secondary mb-3",
                div(class = 'card-body',   
                fluidRow(
                    
                    column(4,
                      dateInput('monitora_datafim', 'Data de encerramento', value = Sys.Date(), format = 'dd/mm/yyyy'),
                    ), #end column

                    column(4,
                       numericInput('monitora_casos', 'Número de casos', value = 0, min = 0),
                       numericInput('monitora_obitos', 'Número de óbitos', value = 0, min = 0)
                    ), #end column

                    column(4,
                      selectizeInput('monitora_agravo', label = 'Qual doença ou agravo foi detectada?', 
                      choices = c(doenca, 'Outra'), selected = NULL, options = list(
                      onInitialize = I('function() { this.setValue(""); }')
                      ), width = '85%'),
                      shinyjs::hidden(
                        textInput("monitora_outroagravo", 'Outro agravo, qual?', value ='')
                        )

                    ) #end column
                )
                ) #end row
                ) #end card
            ) #enbd tagList  
        } #end if

  })

  observe({
       if(input$monitora_agravo == 'Outra'){shinyjs::show('monitora_agravo')}else{shinyjs::hide('monitora_agravo')} 
  }) #observe para ocultar o input de outro agravo
