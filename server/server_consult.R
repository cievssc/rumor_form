 #server consultar formulários
  #-------------------------------------------------------------------------
  #lista formulários
  #-------------------------------------------------------------------------
  #obtendo so dados
  consult_data <- eventReactive(c(input$consult_consultar,input$consult_apagar), {
                                Sys.sleep(.3)
                                usuario = check_user()[,'usuario']
                               
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT * FROM rumores_evento")
                                   )
                                   formulario <- DBI::dbGetQuery(conn(), query)
                                  
                                 formulario$dia <- with(formulario, ifelse(!is.na(data_atualizacao), data_atualizacao, data_noticia))
                                 formulario <- subset(formulario, 
                                                      dia %in% seq(as.Date(input$consult_periodo[1]),as.Date(input$consult_periodo[2]),'day'))
                                 formulario$dia <- NULL
                                 on.exit(DBI::dbDisconnect(conn()))
                                 formulario  
                                })
 
  
  lista_formulario <- reactiveVal(NULL)

                      observeEvent(input$consult_consultar,{
                                formulario <- consult_data()
                                  names(formulario) <- c('id', 'Responsável','dt_envio','Semana\nEpidemiológica', 'Dt.Notícia','Dt.Atualização','Descrição','Link',
                                  'Doença/Agravo','Notif. Imediata?','Área Técnica','Fonte','País','UF','Município',
                                  'Total Casos','Casos confirmados','Suspeitos','Descartado','Óbito')
                                  
                             if(is.null(consult_data())) return() 
                                   
                             lista_formulario(formulario)     
                               })
                                        
  output$consult_listaforms <-DT::renderDT(lista_formulario(), selection = 'single', rownames = F)
  
  
  
  #apagando registro
  #atualizando os input de edição
  observeEvent(input$consult_apagar, {
    if (is.null(input$consult_listaforms_rows_selected)) return()
    
    selected <- lista_formulario()[input$consult_listaforms_rows_selected, 'id']
    
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("DELETE FROM rumores_evento  WHERE id = ?code1"), 
                                   code1 = selected[1]
                                   )
                                  DBI::dbSendQuery(conn(),query)
                                  
      on.exit(DBI::dbDisconnect(conn()))

      shinyjs::reset(id = 'consulta')
      
    showModal(modalDialog(
      title = NULL,
       tagList(
        p("Rumor Apagado!")),
        easyClose = TRUE,
        footer = NULL
      )
      )
    
   }) #end observe event apagar 
    
  #.!!!!importante!!!!!
  #se este valor for igual a zero, o programa semrpe criará novos ids.
  #se este valor for igual a um, o programa editará os ids existentes
  id_indicador <- reactiveVal(0)
  
  #reactive para marcar o id de edição  
  id_selecionado <- reactiveVal(NA) #add em 10-nov-2022 (1831h)
  
   
  
 #exportando...

 output$consult_expo <- downloadHandler(
  #if(nrow(lista_formulario()) == 0){return()},
  filename = function() {
      paste("rumores-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(lista_formulario(), file)
    }
)
  