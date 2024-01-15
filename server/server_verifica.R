 #server verificação rumores (15-jan-2024, 13:31h)
 

  #lista de rumores a serem verificados
  verific_rumores <- eventReactive(input$navbar == 'Verificação de rumor',{
                     dadoi <- DBI::dbGetQuery(conn(), "SELECT id, data_noticia, descricao,
                                area_tecnica FROM rumores_evento WHERE verific_tecnica = TRUE")
                     dadoi
  })

  #-------------------------------------------------------------------------
  #uiOutputs
  #-------------------------------------------------------------------------
  
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

  # opções verificação
  verific_selected <- reactive({dadoi <- verific_rumores()
                               linha <- getReactableState("verific_lista", "selected")
                               dadoi[linha,'id']
                               }) 
  


  #dateInput('verific_data', 'Data retorno da área técnica', value = Sys.Date(), format = '%dd/%mm/%yyyy'),
   #             selectInput('verific_rumor', 'O rumor é verídico?', choices = c('Sim','Não'), selected = 'Não')
  
