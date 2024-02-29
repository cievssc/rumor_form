 #server config (criado em 27-fev-2024, 16:36h)
 output$config_edit <- renderUI({
                            
                           

                            tagList(
                             if(input$config_opçoes == 'Adicionar'){textInput('config_addagravo',label = NULL,placeholder = 'Agravo/Doença')}else{
                                 reactableOutput('config_listatab')
                            },
                            br(),
                            actionButton('config_enviar', 'Enviar')
                            )
                        })



 output$config_listatab <- renderReactable({
                            doenca <- data.frame(doenca())
                            names(doenca) <- 'Agravo'
                            reactable(doenca, selection = "multiple", compact = T, pagination = F, highlight = T, style = 'font-size:12px;',
                            height = '15rem')})
                            

 #modal ao clicar no botão enviar
 
 observeEvent(input$config_enviar,{
         showModal(modalDialog(
      title = NULL,
       tagList(
        p("Tem certeza desta ação?")),
        easyClose = TRUE,
        footer = actionButton('config_confirm', "Confirmar")
      ))
        })

 observeEvent(input$config_confirm,{
        if(input$config_opçoes == 'Adicionar'){
            DBI::dbSendQuery(conn(), paste0('INSERT INTO lista_agravo VALUES (\'', input$config_addagravo,'\');'))
            shinyjs::reset('config_addagravo')
           }else{
            linha <- getReactableState("config_listatab", "selected")
            linha <- doenca()[linha]
            DBI::dbSendQuery(conn(), paste0('DELETE FROM  lista_agravo WHERE doenca = \'', linha,'\''))
           }
        doenca(DBI::dbGetQuery(conn(), 'SELECT * FROM lista_agravo') %>% .[,1] )
        DBI::dbDisconnect(conn())
        updateSelectInput(session,'rumor_doenca', 'Doença:', choices = doenca(), selected = NA)
        doenca <- data.frame(doenca())
        names(doenca) <- 'Agravo'
        updateReactable('config_listatab', data = doenca)
        removeModal()
        })

