 #server para a aba formulário (30-ago-2023, 17:06h)
 
 #---------------------------------------------------------
 #-------------ui outputs----------------------------------

 output$teste <- renderPrint({input$navbar})
#semana epidemiológica
semana_epid <- reactiveVal()
output$rumor_se <- renderUI({
        dadoi <- lubridate::epiweek(input$rumor_data)
           
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
        }
})

observe(
    if(is.null(input$rumor_estado)){NULL}else {
       
   dadoi <- municipios_br[municipios_br$uf == input$rumor_estado,] 
   updateSelectInput(session, 'rumor_municipio', 'Município', choices = c('Todos',dadoi[,'municipio']), selected = "Todos")
  # municipio(input$rumor_municipio)
    }
)

observe(
  if(is.null(input$rumor_estado)){NULL}else {
    if(input$rumor_pais == 'Brasil' & input$rumor_estado != 'Nacional'){
    municipio(input$rumor_municipio)
  }else{municipio(NA)}
  }  
)

 #Campo para vinculação do rumor (add 11-jan-24, 14:20h)
 output$rumor_infopai <- renderUI({
            if(!isTRUE(input$rumor_adicional)){NULL}else{
             list(
           fluidRow(     
           column(6,
           dateRangeInput("rumor_periodo", label = "Período: ",  format = ' dd/mm/yyyy'),
           actionButton('rumor_consultar','Consultar')
           )
           ), #endrow
           hr(),
           fluidRow(
            column(12,
           h4('Relação de rumores'),
           br(), 
           DT::dataTableOutput("rumor_listaforms")
            ))
  ) }
 })

 #obtendo so dados
  rumor_data <- eventReactive(c(input$rumor_consultar), {
                                Sys.sleep(.3)
                                usuario = check_user()[,'usuario']
                               
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT * FROM rumores_evento WHERE id_pai is null") 
                                   ) #selecionando somente rumores que não estão ligados a nenhum outro
                                   formulario <- DBI::dbGetQuery(conn(), query)
                                  
                                 formulario$dia <- with(formulario,  data_noticia)
                                 formulario <- subset(formulario, 
                                                      dia %in% seq(as.Date(input$rumor_periodo[1]),as.Date(input$rumor_periodo[2]),'day'))
                                 formulario$dia <- NULL
                                 on.exit(DBI::dbDisconnect(conn()))
                                 formulario  
                                })
 
  
  rumor_formulario <- reactiveVal(NULL)

                      observeEvent(input$rumor_consultar,{
                                formulario <- rumor_data()
                                formulario <- formulario[,c(2,19,20,1,3,5:7,9)]
                                  names(formulario) <- c('id', 'Responsável','dt_envio','Semana\nEpidemiológica', 'Dt.Notícia','Descrição','Link',
                                  'Doença/Agravo','Área Técnica')
                                  
                             if(is.null(consult_data())) return() 
                                   
                             rumor_formulario(formulario)
                               })
                                        
  output$rumor_listaforms <-DT::renderDT(rumor_formulario(), selection = 'single', rownames = F)
  
  #selecionando o ip pai
  rumor_rumorpai_id <- reactive({
  if (is.null(input$rumor_listaforms_rows_selected)) return()
    rumor_formulario()[input$rumor_listaforms_rows_selected, 'id']
  })

 #ilustrando o ip pai
 output$rumor_idpai <- renderUI({
       if(!isTRUE(input$rumor_adicional)){NULL}else{   
    tagList(
        h4(paste('ID selecionado:')),
        h5(rumor_rumorpai_id())
    )}
}) #end renderui

 #----------------------------------------------------------------------------------
   #criando o df dos dados inseridos
  rumor_dados_empilhados <- reactive({                                       
                                 data.frame('se' = semana_epid(),
                                            'data_noticia' = input$rumor_data,
                                            'data_atualizacao' = NA, #Este campo está sem utilidade (11-jan-24, 14:34h)
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
                                            'obito' = input$rumor_obitos,
                                            'id_pai' = if(!isTRUE(input$rumor_adicional)){NA}else{rumor_rumorpai_id()}, #add em 11-jan-24 (16:26h)
                                            'verific_tecnica' = input$rumor_enviar_area 
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

