 #server para a aba de infrequência (20-jan-2022, 22:36h)
 
 # update input de escolas (mostrar somente as escolas do município)
 escolas <- reactiveVal()
 observe({if(id_indicador() == 0){
         escolas(escola_programa[escola_programa$municipio == input$infreq_municipio,'nome_ue'])
         updateSelectInput(session, 'infreq_escola', "Unidade escolar:",
                           choices = escolas())}
         
         })
 
 #add 04-abr-22 (19:34h)
 observe({
          escolhas <- escolhas_categoria_vetor[escolhas_categoria_vetor != input$infreq_principal] 
         updateSelectInput(session,'infreq_ativa', "Causas Secundárias: ", choices = escolhas)
         
         })
 
 
 #.renderizando os UI de preenchimento de infrequência
 #endereço da escola
 output$infreq_uiescola <- renderUI({
                       dadoi <- escola_programa[which(escola_programa$nome_ue == input$infreq_escola &
                                                escola_programa$municipio == input$infreq_municipio),]
                       endereco <- with(dadoi, paste0(Endereço,', nª ', Número ))
                       complemento <- with(dadoi, paste0(Complemento))
                       bairro <- with(dadoi, paste0(Bairro))
                       
                     tagList(h4('Endereço: ', style = 'color: black'),
                          h5(endereco, style = 'color: black'),
                          h5(paste('Complemento:', complemento), style = 'color: black'),
                          h5(paste('Bairro:', bairro), style = 'color: black')
                          )
                     }) #END ui endereço escola
                     
 
 
 #idade
 idade_infreq <- reactiveVal(0)
 
 #alterado em 09-nv-2022 (16:36h)
 observe({
     dadoi <- as.numeric(as.Date(input$infreq_falta7) - as.Date(input$infreq_nascimento))
     idade_infreq(dadoi)
     })
     
 output$infreq_idade <- renderUI({
                     dadoi <- idade_infreq()
                     
                     anos <- dadoi/365.25
                     dadoi <- paste0(floor(anos),' anos e ',floor((anos - floor(anos))*12), ' meses.')
                     list(h3('Idade:'),
                     h4(dadoi))
                     
                     })
 


   #criando o df dos dados inseridos
  infreq_dados_empilhados <- reactive({
                                       
                                 data.frame('municipio'   = input$infreq_municipio,
                                            'escola'      = input$infreq_escola,
                                           'infreq_adicional' = input$infreq_infoadicional,
                                            'periodo_atipico' = as.numeric(input$infreq_periodoatipico),
                                           'aluno'       = input$infreq_nome,
                                           'aluno_cpf'   = as.character(input$infreq_cpf),
                                           'dat_nascimento'  = as.Date(input$infreq_nascimento),
                                           'idade'           = idade_infreq()/365.25,
                                           'municipio_residencia' = input$infreq_munic_resid,
                                           'endereco_residencia'    = input$infreq_endereco_resid,
                                           'bairro_residencia'    = input$infreq_bairro_resid,
                                            'zona_residencia' = as.numeric(input$infreq_zonaresid),
                                           'serie'         = input$infreq_serie,
                                           'turma'         = input$infreq_turma,
                                           'infreq_matricula' = input$infreq_matricula,
                                           'pessoa_responsavel'   = as.numeric(input$infreq_pessoaresid),
                                           'nome_responsavel' = input$infreq_responsavel,
                                           #'cpf_responsavel' =as.character(input$infreq_responsavelcpf),#retirado na versão 12
                                           'nome_responsavel_2' = input$infreq_responsavel_2,
                                            'info_adicional_resp' = input$infreq_residinfoadicional,
                                            'falta_1' = as.Date(input$infreq_falta1),
                                            'falta_2' = as.Date(input$infreq_falta2),
                                            'falta_3' = as.Date(input$infreq_falta3),
                                            'falta_4' = as.Date(input$infreq_falta4),
                                            'falta_5' = as.Date(input$infreq_falta5),
                                            'falta_6' = as.Date(input$infreq_falta6),
                                            'falta_7' = as.Date(input$infreq_falta7),
                                            'possiveis_causas' = paste(input$infreq_ativa, collapse = ' / '),
                                            'possiveis_causas_principal' = input$infreq_principal,
                                            'situacao_apoia' = input$infreq_situacao,
                                            'sexo' = as.numeric(input$infreq_sexo)
                                           )
                                    })                   
   
  #--------------------------------------------------------------------------
 #modal confirmação
 #--------------------------------------------------------------------------
  
 #retorno de confirmação
 # aparecer o formulário inicial de senha
 observeEvent(input$infreq_enviar, {
  query <- DBI::sqlInterpolate(conn(), 
            paste0("SELECT id FROM ",vetor_b[[3]])
            )
  request <- DBI::dbSendQuery(conn(),query)
  lista_id <- DBI::dbFetch(request)
  DBI::dbClearResult(request)
  
  if(nrow(lista_id) == 0){lista_id[1,1] <- 0}
  fields <- DBI::dbListFields(conn(), vetor_b[[3]])
  dadoi <- infreq_dados_empilhados()
 if(id_indicador() == 0){
     dadoi$id <- max(lista_id, na.rm = T) +1
     dadoi$send_usuario <- usuario_log()
     dadoi$send_time <- Sys.time()
     dadoi$active <- T
    # dadoi <- dadoi[,fields]
     }else{
    query <- DBI::sqlInterpolate(conn(), 
            paste0("UPDATE ",vetor_b[[3]],
                   " SET active = ?code 
                   WHERE id = ?code1"),
              code = 'f',
              code1 = id_selecionado()
             )
    DBI::dbSendQuery(conn(), query)
                                                          
     dadoi$id <- id_selecionado()
     dadoi$send_usuario <- usuario_log()
     dadoi$send_time <- Sys.time()
     dadoi$active <- T
     dadoi <- dadoi[,fields]                                                           
     }
   
 #-----------------------------------------------------------------------------
 #reiniciando todos os inputs
  shinyjs::reset(id = 'teste')
  lista_formulario(NULL) #ADD EM 19-jan-2023 (12:09h)
 #-----------------------------------------------------------------------------
   
 DBI::dbWriteTable(conn(),value = dadoi, name = "infreq_dados_empilhados", append = T)    
 
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
