 #server monitoração rumores (15-jan-2024, 13:31h)

  monitora_rumores <- reactiveVal() 
  #lista de rumores a serem monitorados
  observeEvent(input$navbar == 'Monitora' | input$monitora_enviar,{
                    lista_evento <- DBI::dbGetQuery(conn(), "SELECT id, data_noticia, doenca, area_tecnica FROM rumores_evento WHERE
                                            id IN (SELECT id FROM rumores_verifica WHERE risc_avalia IN ('Alto', 'Muito alto'))  AND
                                                  id NOT IN (SELECT id FROM rumores_monitora WHERE monitora_encerra = 'TRUE')
                                            ")
                    lista_verific <- DBI::dbGetQuery(conn(), "SELECT id, risc_avalia FROM rumores_verifica 
                                            WHERE risc_avalia IN ('Alto', 'Muito alto') AND
                                                  id NOT IN (SELECT id FROM rumores_monitora WHERE monitora_encerra = 'TRUE')
                                                  ")
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
                      onInitialize = I('function() { this.setValue(""); }')), multiple = T),
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

  observeEvent(input$monitora_agravo,{
       if(input$monitora_agravo == 'Outra'){shinyjs::show('monitora_outroagravo')}else{shinyjs::hide('monitora_outroagravo')} 
  }, ignoreNULL = T) #observe para ocultar o input de outro agravo

 #-------------------------------------------------------------------------
  #enviando dados
 #-------------------------------------------------------------------------

  #criando o df dos dados inseridos
  monitora_dados_empilhados <- reactive({
                                if(!isTRUE(input$monitora_rumor)){
                                    data.frame('id' = monitora_selected(),
                                           'send_usuario_monitora' = check_user()[,'usuario'],
                                           'send_time_monitora'   = Sys.time(),
                                           'monitora_acoes' = paste(input$monitora_lista_acoes, collapse = '|'),
                                           'monitora_equipes' = paste(input$monitora_area_tecnica, collapse = '|'),
                                           'monitora_encerra' = input$monitora_desfecho)

                                }else{
                                  data.frame('id' = monitora_selected(),
                                           'send_usuario_monitora' = check_user()[,'usuario'],
                                           'send_time_monitora'   = Sys.time(),
                                           'monitora_acoes' = paste(input$monitora_lista_acoes, collapse = '|'),
                                           'monitora_equipes' = paste(input$monitora_area_tecnica, collapse = '|'),
                                           'monitora_encerra' = input$monitora_desfecho,
                                           'monitora_dt_fim' = input$monitora_datafim,
                                           'monitora_casos' = input$monitora_casos,
                                           'monitora_obitos' = input$monitora_obitos,
                                           'monitora_agravo' = if(input$monitora_agravo == 'Outra'){input$monitora_outroagravo}else{input$monitora_agravo})
                                }
                                    })                   
   

 #--------------------------------------------------------------------------
 #modal confirmação
 #--------------------------------------------------------------------------
  
 #retorno de confirmação
 # aparecer o formulário inicial de senha
 observeEvent(input$monitora_enviar, {
  
  dadoi <-  monitora_dados_empilhados()
 #-----------------------------------------------------------------------------
 #reiniciando todos os inputs
  shinyjs::reset(id = 'monitora')
  
 #-----------------------------------------------------------------------------
   
 DBI::dbWriteTable(conn(),value = dadoi, name = "rumores_monitora", append = T)    
 
 on.exit(DBI::dbDisconnect(conn()))
 
  showModal(modalDialog(
      title = NULL,
       tagList(
        p("Notificação enviada com sucesso")),
        easyClose = TRUE,
        footer = NULL
      )
      )
  })  #end observe event

  #TODO >> atualização de status de monitoramento!!!
