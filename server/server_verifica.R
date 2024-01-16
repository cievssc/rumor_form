 #server verificação rumores (15-jan-2024, 13:31h)
 

  #lista de rumores a serem verificados
  verific_rumores <- eventReactive(c(input$navbar == 'Verificação de rumor',
                                   input$verific_enviar),{
                    lista_verificadas <- DBI::dbGetQuery(conn(), "SELECT id FROM rumores_verifica")
                     dadoi <- DBI::dbGetQuery(conn(), "SELECT id, data_noticia, descricao,
                                area_tecnica FROM rumores_evento WHERE verific_tecnica = TRUE")
                     dadoi <- dadoi[!(dadoi$id %in% lista_verificadas[,1]),] #mostrar somente rumores que não foram verificados ainda (16-jan-24, 16:34h)
                     dadoi
  })

  #-------------------------------------------------------------------------
  #uiOutputs
  #-------------------------------------------------------------------------
  
  #------------------------------------------
  # output da lista rumores verificação
  output$verific_lista_rumor <- renderUI({
     
     if(nrow(verific_rumores()) == 0){
        tagList(
        br(),    
        h4('Não há rumores a serem verificados')
        )
     }else{
        reactableOutput('verific_lista')
     }
  })
  
  #saída tabela
  output$verific_lista <- renderReactable({
    dadoi <- verific_rumores()
    names(dadoi) <- c('id', 'Data Notícia', 'Descrição', 'Área técnica')
    reactable(dadoi, selection = "single", onClick = "select")
  })
  
  #------------------------------------
  # opções verificação
  verific_selected <- reactive({dadoi <- verific_rumores()
                               linha <- getReactableState("verific_lista", "selected")
                               dadoi[linha,'id']
                               }) 
  
  # output verificacao

  output$verific_area_tecnica <- renderUI({
        if(length(verific_selected()) == 0){NULL}else{
        tagList(    
         dateInput('verific_data', 'Data retorno da área técnica', value = Sys.Date(), format = '%dd/%mm/%yyyy'),
                checkboxInput('verific_rumor', label = 'O rumor é verídico?',  value = F)  
               )
        }

  })

  #------------------------------------
  #output opcoes

  output$verific_opcoes <- renderUI({
        if(!isTRUE(input$verific_rumor)){NULL}else{
            tagList(
                div(style = 'text-align: center;',
                h3('Probabilidade')),
                    br(),
                div(class = "card border-secondary mb-3",
                div(class = 'card-body',   
                fluidRow(
                    
                    column(4,
                      selectInput('verific_prob_dissemina', label = 'Apresenta risco de disseminação nacional ou internacional?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0), 

                      selectInput('verific_prob_alerta', label = 'Evento em alerta internacional ou ESPII, evento no marco do RSI iminente ingresso no país?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0) 
                    ), #end column

                    column(4,
                      selectInput('verific_prob_inesperado', label = 'Trata-se de evento  inesperado ou desconhecido?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0), 

                      selectInput('verific_prob_reintroducao', label = 'Representa a reintrodução de doença erradicada?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0) 
                    ), #end column

                    column(4,
                      selectInput('verific_prob_manejo', label = 'A localidade não tem capacidade de manejo do evento?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0)
                    ) #end column
                )
                ) #end row
                ), #end card
                hr(),
              
               div(style = 'text-align: center;',
                h3('Impacto')),
                    br(),
                
                div(class = "card border-secondary mb-3",
                div( class = 'card-header','Impacto na Saúde Humana:'),
                div(class = 'card-body',
                fluidRow(                    
                    column(4, 
                    h5('Extensão Geográfica'),
                      selectInput('verific_geog_dissemina', label = 'O evento está disseminado em vários municípios ou países?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0),
                      selectInput('verific_geog_notific', label = 'O evento está notificado em mais de um estado ou região?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0),
                      selectInput('verific_geog_inst', label = 'O evento tem sido notificado em mais uma instituição?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0)), #end column

                    column(8,
                    h5('Característica do Evento'),
                     selectInput('verific_evento_surto', label = 'Evento está envolvido em suspeita ou confirmado de surto?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0),
                      selectInput('verific_evento_alerta', label = 'Trata-se de uma doença, agravo ou eventos de saúde pública com alterações
                       do perfil clínico epidemiológico (níveis de incidência, mortalidade, letalidade) ou em zona de alerta?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0),
                      selectInput('verific_evento_obito', label = 'Trata-se de evento de saúde pública com óbitos acima do esperados?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0),
                      selectInput('verific_evento_transmissi', label = 'Evento de alta patogenicidade, virulência e transmissibilidade?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0),
                      selectInput('verific_evento_pops', label = 'O evento afeta populações vulneráveis?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0)
                      )
                    ) #endRow
                ) #end card-body
                ),  #end card

            div(class = "card border-secondary mb-3",
                div( class = 'card-header','Impacto na Assistência:'),
                div(class = 'card-body', style = 'display: flex',
                
                    selectInput('verific_assist_hosp', label = 'Apresenta aspectos que demonstram aumento aos níveis atendimentos ou hospitalizações?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%'),
                      selectInput('verific_assist_medic', label = 'Evento envolve grave comprometimento assistencial?  Não existem tratamentos específicos 
                      ou requer uso de medicamentos controlados?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%'),
                      selectInput('verific_assist_profsaude', label = 'O evento afeta profissionais de saúde?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%')

                ) #end card-body
                ),  #end card

            div(class = "card border-secondary mb-3",
                div( class = 'card-header','Impacto Social:'),
                div(class = 'card-body', style = 'display: flex',
                
                    selectInput('verific_social_estigma', label = 'Trata-se de doença ou agravo ou evento de saúde pública com alta relevância social 
                    (que gere medo, estigmatização ou indignição social)', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%'),
                      selectInput('verific_social_economica', label = 'O evento afeta  localmente o turismo ou tem alta influência econômica?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%'),
                      selectInput('verific_social_convivencia', label = 'O evento afeta a convivência social?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%')
                    
                ) #end card-body
                ),  #end card

            div(class = "card border-secondary mb-3",
                div( class = 'card-header','Impacto na capacidade de resposta:'),
                div(class = 'card-body', style = 'display: flex',
                 
                    selectInput('verific_capac_atraso', label = 'Existem atrasos nas notificações ou análises de dados ou silêncio epidemiológico?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%'),
                      selectInput('verific_capac_sobrecarga', label = 'Existe sobrecarga na equipe de vigilância ou não tem equipe de pronta resposta?', 
                      choices = c('Não' = 0 ,'Talvez' = 1, 'Sim' = 2), selected = 0, width = '60%')
                ) #end card-body
                )  #end card

            ) #end tagList
        }

  })
  
 #-------------------------------------------------------------------------
  #enviando dados
 #-------------------------------------------------------------------------

    #criando o df dos dados inseridos
  verific_dados_empilhados <- reactive({
                                if(!isTRUE(input$verific_rumor)){
                                    data.frame('id' = verific_selected(),
                                           'send_usuario_verifica' = check_user()[,'usuario'],
                                           'send_time_verifica'   = Sys.time(),
                                           'data_retorno' = input$verific_data,
                                           'verifica' = input$verific_rumor)

                                }else{
                                 data.frame('id' = verific_selected(),
                                           'send_usuario_verifica' = check_user()[,'usuario'],
                                           'send_time_verifica'   = Sys.time(),
                                           'data_retorno' = input$verific_data,
                                           'verifica' = input$verific_rumor,
                                           'prob_dissemina' =  input$verific_prob_dissemina,
                                           'prob_alerta' =  input$verific_prob_alerta,
                                           'prob_inesp' = input$verific_prob_inesperado,
                                           'prob_reintroducao' = input$verific_prob_inesperado,
                                           'prob_manejo' = input$verific_prob_manejo,
                                           'geog_dissemina' = input$verific_geog_dissemina,
                                           'geog_notifica' = input$verific_geog_notific,
                                           'geog_inst' = input$verific_geog_inst,
                                           'evento_surto' = input$verific_evento_surto,
                                           'evento_alerta' = input$verific_evento_alerta,
                                           'evento_obito' = input$verific_evento_obito,
                                           'evento_transmissi' = input$verific_evento_transmissi,
                                           'evento_pops' = input$verific_evento_pops,
                                           'assist_hosp' = input$verific_assist_hosp,
                                           'assist_medic' = input$verific_assist_medic,
                                           'assist_profsaude' = input$verific_assist_profsaude,
                                           'social_estigma' = input$verific_social_estigma,
                                           'social_economica' = input$verific_social_economica,
                                           'social_convivencia' = input$verific_social_convivencia,
                                           'capac_atraso' = input$verific_capac_atraso,
                                           'capac_sobrecarga' = input$verific_capac_sobrecarga
                                           )
                                }
                                    })                   
   

 #--------------------------------------------------------------------------
 #modal confirmação
 #--------------------------------------------------------------------------
  
 #retorno de confirmação
 # aparecer o formulário inicial de senha
 observeEvent(input$verific_enviar, {
  
  dadoi <-  verific_dados_empilhados()
 #-----------------------------------------------------------------------------
 #reiniciando todos os inputs
  shinyjs::reset(id = 'verific')
  
 #-----------------------------------------------------------------------------
   
 DBI::dbWriteTable(conn(),value = dadoi, name = "rumores_verifica", append = T)    
 
 on.exit(DBI::dbDisconnect(conn()))
  
  showModal(modalDialog(
      title = NULL,
       tagList(
        p("Verificação enviada com sucesso")),
        easyClose = TRUE,
        footer = NULL
      )
      )
  })  #end observe event