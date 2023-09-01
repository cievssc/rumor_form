 #server para a aba formulário (30-ago-2023, 17:06h)

 #---------------------------------------------------------
 #-------------ui outputs----------------------------------

#data atualzada
 output$rumor_ui_data_atual <- renderUI({
    if(isTRUE(input$rumor_checkdata)){
      dateInput('rumor_data_atual', 'Data:', format = 'dd-mm-yyyy', language = 'pt-BR')   
    }else{
        NULL
    }
 }) #end renderui

#semana epidemiológica
semana_epid <- reactiveVal()
output$rumor_se <- renderUI({
    if(!isTRUE(input$rumor_checkdata)){
        dadoi <- lubridate::epiweek(input$rumor_data)
    }else{
        dadoi <- lubridate::epiweek(input$rumor_data_atual)
    }
    semana_epid(dadoi)
    tagList(
        h4(paste('Semana Epidemiológica:')),
        h5(dadoi)
    )
}) #end renderui

#semana epidemiológica
semana_epid <- reactiveVal()
output$rumor_se <- renderUI({
    if(!isTRUE(input$rumor_checkdata)){
        dadoi <- lubridate::epiweek(input$rumor_data)
    }else{
        dadoi <- lubridate::epiweek(input$rumor_data_atual)
    }
    semana_epid(dadoi)
    tagList(
        h4(paste('Semana Epidemiológica:')),
        h5(dadoi)
    )
}) #end renderui

#estados
estado <- reactiveVal(NA)
output$rumor_estado_ui <- renderUI({
    if(input$rumor_pais == 'Brasil'){
        selectInput('rumor_estado', 'Estado:', choices = c('Nacional', unique(municipios_br$uf)),
        selected = 'Nacional', multiple = F)
       #estado(input$rumor_estado)
    }else{
       NULL}
})

 observe(
   if(input$rumor_pais == 'Brasil'){
    estado(input$rumor_estado)
   }else{estado(NA)}
   
 )

#Municípios
municipio <- reactiveVal(NA)
output$rumor_municipio_ui <- renderUI({
    if(input$rumor_pais == 'Brasil' & input$rumor_estado != 'Nacional'){
        selectInput('rumor_municipio', 'Município:', choices = c('Todos',municipios_br[municipios_br$uf == 'Santa Catarina','municipio']),
        selected = 'Florianópolis', multiple = F)
       # municipio(input$rumor_municipio)
    }else{
        NULL
        #municipio(NA)
        }
})

observe(
    if(is.null(input$rumor_estado)){NULL}else {
       
 if(input$rumor_pais == 'Brasil' & input$rumor_estado != 'Nacional'){
   dadoi <- municipios_br[municipios_br$uf == input$rumor_estado,] 
  updateSelectInput(session, 'rumor_municipio', 'Município', choices = c('Todos',dadoi[,'municipio']), selected = "Todos")
  municipio(input$rumor_municipio)
 }
    }
)



   #criando o df dos dados inseridos
  rumor_dados_empilhados <- reactive({                                       
                                 data.frame('se' = semana_epid(),
                                            'data_noticia' = input$rumor_data,
                                            'data_atualizacao' = if(isTRUE(input$rumor_checkdata)){input$rumor_data_atual}else{NA},
                                            'descricao' = input$rumor_infoadicional,
                                            'link' = input$rumor_link,
                                            'doenca' = input$rumor_doenca,
                                            'notificacao_imed' = input$rumor_notif_imediata,
                                            'area_tecnica' = input$rumor_area_tecnica,
                                            'fonte' = input$rumor_fonte,
                                            'pais' = input$rumor_pais,
                                            'uf' = estado(),
                                            'municipio' = municipio(),
                                            'casos' = input$rumor_casos,
                                            'casos_confirmados' = input$rumor_casos_conf,
                                            'suspeito' = input$rumor_suspeitos,
                                            'descartado' = input$rumor_descartados,
                                            'obito' = input$rumor_obitos

                                           )
                                    })                   
   
  #--------------------------------------------------------------------------
 #modal confirmação
 #--------------------------------------------------------------------------
  
 #retorno de confirmação
 # aparecer o formulário inicial de senha
 observeEvent(input$rumor_enviar, {
  query <- DBI::sqlInterpolate(conn(), 
            paste0("SELECT id FROM rumores_evento")
            )
  lista_id <- DBI::dbGetQuery(conn(),query)
  
  if(nrow(lista_id) == 0){lista_id[1,1] <- 0}

  fields <- DBI::dbListFields(conn(), 'rumores_evento')
  dadoi <- rumor_dados_empilhados()
 if(id_indicador() == 0){
     dadoi$id <- max(lista_id, na.rm = T) +1
     dadoi$send_usuario <- check_user()[,'usuario']
     dadoi$send_time <- Sys.time()
    # dadoi <- dadoi[,fields]
     }else{
                                                          
     dadoi$id <- id_selecionado()
     dadoi$send_usuario <- check_user()[,'usuario']
     dadoi$send_time <- Sys.time()
     dadoi <- dadoi[,fields]                                                           
     }
   
 #-----------------------------------------------------------------------------
 #reiniciando todos os inputs
  shinyjs::reset(id = 'teste')
  lista_formulario(NULL) #ADD EM 19-jan-2023 (12:09h)
 #-----------------------------------------------------------------------------
   
 DBI::dbWriteTable(conn(),value = dadoi, name = "rumores_evento", append = T)    
 
 id_indicador(0)
 id_selecionado(NA)
 on.exit(DBI::dbDisconnect(conn()))
  
  showModal(modalDialog(
      title = NULL,
       tagList(
        p("Formulário enviado com sucesso")),
        easyClose = TRUE,
        footer = NULL
      )
      )
  })  #end observe event

