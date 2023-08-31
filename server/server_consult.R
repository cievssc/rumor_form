 #server consultar formulários
  #-------------------------------------------------------------------------
  #lista formulários
  #-------------------------------------------------------------------------
  #obtendo so dados
  consult_data <- eventReactive(c(input$consult_consultar,input$consult_apagar), {
                                Sys.sleep(.3)
                                usuario = usuario_log()
                                if(input$consult_formulario == 1){
                                   query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT * FROM ",vetor_b[[2]], " WHERE active = ?code1"), 
                                   code1 = 't'
                                   )
                                   request <- DBI::dbSendQuery(conn(),query)
                                   formulario <- DBI::dbFetch(request)
                                   DBI::dbClearResult(request)
                                  
                                  }
                                if(input$consult_formulario == 2){
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT * FROM ",vetor_b[[1]], " WHERE active = ?code1"), 
                                   code1 = 't'
                                   )
                                   request <- DBI::dbSendQuery(conn(),query)
                                   formulario <- DBI::dbFetch(request)
                                   DBI::dbClearResult(request)
                                  }
                                if(input$consult_formulario == 3){
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("SELECT * FROM ",vetor_b[[3]], " WHERE active = ?code1"), 
                                   code1 = 't'
                                   )
                                   request <- DBI::dbSendQuery(conn(),query)
                                   formulario <- DBI::dbFetch(request)
                                   DBI::dbClearResult(request)
                                  }
                                  
                                 formulario$dia <- as.Date(substr(formulario$send_time,1,10), origin = '1970-01-01')
                                 formulario <- subset(formulario, 
                                                      dia %in% seq(as.Date(input$consult_periodo[1]),as.Date(input$consult_periodo[2]),'day'))
                                 formulario$dia <- NULL
                                 on.exit(DBI::dbDisconnect(conn()))
                                 formulario  
                                })
 
  
  lista_formulario <- reactiveVal(NULL)

                      observeEvent(input$consult_consultar,{
                               if(input$consult_formulario == 1){
                                  formulario <- consult_data()[,c('id','send_usuario','paciente','dat_obito',
                                                             'dec_obito','municipio_obito')]
                                  names(formulario) <- c('id', 'Responsável','Paciente','Data Óbito',
                                                        'Número DO', 'Município da Ocorrência')
                                  }
                                if(input$consult_formulario == 2){
                                  formulario <- consult_data()[,c('id','send_usuario','paciente','dat_obito',
                                                             'dec_obito','municipio', 'nome_responsavel')]
                                  names(formulario) <- c('id', 'Responsável','Paciente','Data\nÓbito',
                                                        'Número DO', 'Município\nda Ocorrência', 'Nome\nResponsável')
                                  }
                                if(input$consult_formulario == 3){
                                  formulario <- consult_data()[,c('id','send_usuario','aluno','falta_7',
                                                             'infreq_matricula','situacao_apoia', 'municipio')]
                                  names(formulario) <- c('id', 'Responsável','Aluno','Data\nÚltima Falta',
                                                        'Matrícula', 'Situação APOIA', 'Município')
                                  }
                             if(is.null(consult_data())) return() 
                                   
                             lista_formulario(formulario)     
                               })
                                        
  output$consult_listaforms <-DT::renderDT(lista_formulario(), selection = 'single', rownames = F)
  
  
  
  #apagando registro
  #atualizando os input de edição
  observeEvent(input$consult_apagar, {
    if (is.null(input$consult_listaforms_rows_selected)) return()
    
    selected <- lista_formulario()[input$consult_listaforms_rows_selected, 'id']
    if(input$consult_formulario == 1){
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("UPDATE ",vetor_b[[2]], 
                                          " SET active = ?code 
                                           WHERE id = ?code1"), 
                                   code = 'f',
                                   code1 = selected[1]
                                   )
                                  DBI::dbSendQuery(conn(),query)
                                
                                  }
    if(input$consult_formulario == 2){
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("UPDATE ",vetor_b[[1]], 
                                          " SET active = ?code 
                                          WHERE id = ?code1"), 
                                   code = 'f',
                                   code1 = selected[1]
                                   )
                                  DBI::dbSendQuery(conn(),query)
                                  }
    if(input$consult_formulario == 3){
                                  query <- DBI::sqlInterpolate(conn(), 
                                   paste0("UPDATE ",vetor_b[[3]], 
                                          " SET active = ?code  WHERE id = ?code1"), 
                                   code = 'f',
                                   code1 = selected[1]
                                   )
                                  DBI::dbSendQuery(conn(),query)
                                  }
    
      on.exit(DBI::dbDisconnect(conn()))
      
    showModal(modalDialog(
      title = NULL,
       tagList(
        p("Formulário Apagado!")),
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
  
   observeEvent(input$consult_editar, {
    id_indicador(1)
    if (is.null(input$consult_listaforms_rows_selected)) return()
    
    selected <- consult_data()[input$consult_listaforms_rows_selected, ]
    id_selecionado(selected[1,'id']) 
    
    
    if(input$consult_formulario == 1){
                                  updateNavbarPage(session, 'navbar', selected = "Saúde")
                                  Sys.sleep(.1)
                                  updateTabsetPanel(session, 'panel_obito', selected = 'Longevidade')
                                  
                                  #add em 16-mar-2022 (15:44h)
                     #vetor de município (para casos anteriores a não preencimento da UF - 17-mar-22 14:58h)
                     if(is.na(selected$long_uf_obito)){selected$long_uf_obito <- 'Santa Catarina'}             
                    if(is.na(selected$estado_residencia)){selected$estado_residencia <- 'Santa Catarina'}         
                                  
                     updateSelectInput(session,'long_uf_obito', 'Estado', choices = lapply(split(municipios_br, municipios_br$regiao), function(x){unique(x$uf)}),
      selected = selected$long_uf_obito)
                                  
                     updateDateInput(session, 'long_dataobito', 'Data do óbito:', value = selected$dat_obito)
                                  
                                  updateSelectInput(session, 'long_municipio', 'Município: ', choices = municipios_br[municipios_br$uf == selected$long_uf_obito,'municipio'],
                     selected = selected$municipio_obito)
                     updateTextInput(session,'long_endereco', 'Endereço:', value = selected$endereco_obito)
                     updateTextInput(session,'long_bairro', 'Bairro: ', value = selected$bairro_obito)
                    
                     updatePickerInput(session, 'long_localobito', 'Local do óbito:', 
         choices = c('Hospital', 'Via pública', 'Domicílio', 'Outro serviço de saúde','Aldeia indígena'),
         selected = selected$local_obito)
                   
                    updateTextAreaInput(session, 'long_infoadicional', 'Informações adicionais:',value = selected$long_adicional)
                    updateRadioButtons(session,'long_periodoatipico', 'Contexto do período:',
        choices = c('Normal' = 1,'Atípico' = 2), selected = selected$periodo_atipico) 
                     
                    updateTextInput(session, 'long_nome', 'Nome:', value = selected$paciente)
                    updateRadioButtons(session, 'long_sexo', 'Sexo:', choices = c('Masculino' = 1, 'Feminino' = 2),  selected = selected$sexo)
                    updateTextInput(session, 'long_cpf', 'CPF:', value =selected$paciente_cpf)
                    updateDateInput(session, 'long_nascimento', 'Data de nascimento:', value = selected$dat_nascimento)
                                  
                    updateSelectInput(session,'long_uf_resid', 'Estado', choices = lapply(split(municipios_br, municipios_br$regiao), function(x){unique(x$uf)}),
      selected = selected$estado_residencia)
       
                    updateSelectInput(session,'long_munic_resid', 'Município', choices = municipios_br[municipios_br$uf == selected$estado_residencia,'municipio'],
      selected = selected$municipio_residencia)
                    
                    updateTextInput(session, 'long_endereco_resid', 'Endereço:', value = selected$endereco_residencia)
                    updateTextInput(session, 'long_bairro_resid', 'Bairro: ', value = selected$bairro_residencia)
                    
                   updateSelectInput(session,'long_escolaridade', 'Escolaridade:', 
        choices = c('Sem escolaridade' = 1,'Fundamental I'= 2,'Fundamental II' =3, 'Ensino médio' =4 ,'Superior Incompleto' = 5, 'Superior Completo' = 6, 'Ignorado' = 7), selected = selected$escolaridade)
                   updateSelectInput(session, 'long_cor', 'Cor da pele:', choices = c('Branca' = 1, 'Preta' = 2, 'Amarela' = 3, 'Parda' = 4, 'Indígena' = 5,  'Ignorado' = 6),
      selected =  selected$cor)
                   updateSelectInput(session, 'long_conjug', 'Situação conjugal:', 
      choices = c('Casado' = 1, 'Solteiro' = 2, 'Viúvo' = 3, 'Separado/divorciado' = 4, 'União estável' = 5, 'Ignorado' = 6),
      selected = selected$situacoo_conjugal)
                  updateSelectizeInput(session, 'long_ocupacao', 'Ocupação:', choices = cbo_list[,2], selected = selected$ocupacao_exercida)
      
                    updateRadioButtons(session, 'long_pessoaresid', 'Com quem reside?',
      choices = c('Sozinho' = 1, 'Pais' = 2, 'Representante legal' = 3, 'Família' = 4, 'Outros' = 5),
      selected = selected$pessoa_responsavel)
                  updateTextInput(session, 'long_responsavel', 'Nome do declarante:', value = selected$nome_responsavel)
                  #updateTextInput(session,'long_responsavelcpf', 'CPF:',value = selected$cpf_responsavel) 
                  updateTextInput(session,'long_responsavel_parentesco', 'Grau de parentesco:',value = selected$responsavel_parentesco) 
                  updateTextAreaInput(session,'long_residinfoadicional', 'Informações adicionais:' , value = selected$info_adicional_resp)
                  
                  updateRadioButtons(session,'long_caracterizacao', 'Caracterização do Evento:',
       choices = c('Natural' = 1,'Incidental' = 2), selected = selected$caracterizacao_evento) 
                  updateSelectInput(session,'long_cidbase', 'Causa básica:', 
     choices = cid10[,1], selected = selected$cid_base)
    
                     
                  updateSelectizeInput(session, 'long_cidsec', 'Causa secundária:', 
     choices = cid10[,1], selected = selected$cidsec,
         options = list(
          placeholder = 'Selecione',
          onInitialize = I('function() { this.setValue(""); }')))
                  
                   updateRadioButtons(session, 'long_tabaco', 'O falecido era fumante?',
       choices = c('Sim' = 1, 'Não' = 2, 'Ignorado' = 3), selected = selected$fumante)
                   updateRadioButtons(session, 'long_obeso', 'Tinha excesso de peso, ou obeso?',
       choices = c('Sim' = 1, 'Não' = 2, 'Ignorado' = 3), selected = selected$obeso)
                   updateRadioButtons(session, 'long_ativfisica', 'O indivíduo não praticava nenhum tipo de atividade regular:',
       choices = c('Verdadeiro' = 1, 'Falso' = 2), selected = selected$ativfisica)
                   updateRadioButtons(session, 'long_bebida', 'O indivíduo consumia abusivamente bebida alcoólica?*',
       choices = c('Sim' = 1, 'Não' = 2, 'Ignorado' = 3), selected = selected$bebida)
                   updateRadioButtons(session, 'long_hipertensao', 'O falecido tinha conhecimento de diagnósticos de hipertensão?',
       choices = c('Sim' = 1, 'Não' = 2, 'Ignorado' = 3), selected = selected$hipertensao)
                   updateRadioButtons(session, 'long_diabetes', 'O falecido tinha conhecimento de diagnósticos de diabetes?',
       choices = c('Sim' = 1, 'Não' = 2, 'Ignorado' = 3), selected = selected$diabetes)
                   updateRadioButtons(session, 'long_cancer', 'O falecido tinha conhecimento de diagnósticos de câncer?',
       choices = c('Sim' = 1, 'Não' = 2, 'Ignorado' = 3), selected = selected$cancer)
                                          
                #add em 21-out-2022
                   updateTextInput(session, 'long_ubsvinc', 'UBS de Vinculação:', value = selected$ubs_vinculo)
                   updateTextInput(session, 'long_equipe', 'Equipe responsável:', value = selected$equipe_responsavel)
                   updateRadioButtons(session, 'long_acs_ativo', 'Possui ACS ativo? ', choices = c('Sim' = T, 'Não' = F), selected = selected$acs_ativo)               
                   updateNumericInput(session, 'long_consultas', 'Quantas consultas foram realizadas pela APS nos 3 meses anteriores ao óbito?',
                   value = selected$consultas_aps)
                   
                   
                 municipio <- municipios_br[(municipios_br[,4] == selected$municipio_obito) & (municipios_br[,2] == selected$long_uf_obito),'codigo']
                 municipio <- floor(municipio/10)
                 hospital <- dado_estab[dado_estab$CO_MUNICIPIO_GESTOR %in% municipio,'NO_FANTASIA']
                   updateSelectizeInput(session, 'long_hospital',"Estabelecimento de Saúde:" , 
                     choices = hospital,
                     selected = selected$nome_hospital[1])
                     
                 #add em 11-nov-2022
                 updateTextInput(session,'long_do', 'Número DO*:',value = selected$dec_obito)                 
                                  } #end if 1
                                  
    if(input$consult_formulario == 2){
                                 updateNavbarPage(session, 'navbar', selected = "Saúde")
                                  Sys.sleep(.1)
                                  updateTabsetPanel(session, 'panel_obito', selected = 'Mortalidade\nInfantil')
                                  
                              #vetor de município (para casos anteriores a não preencimento da UF - 17-mar-22 14:58h)
                              if(is.na(selected$inf_uf_obito)){selected$inf_uf_obito <- 'Santa Catarina'}                 
                                  
                              #add em 06-mar-22
                             updateSelectInput(session, 'inf_uf_obito', 'Estado', choices = lapply(split(municipios_br, municipios_br$regiao), function(x){unique(x$uf)}),
                   selected = selected$inf_uf_obito)
                             
                             updateSelectInput(session, 'inf_municobito', 'Município: ', choices = municipios_br[municipios_br$uf == selected$inf_uf_obito,'municipio'],
                     selected = selected$municipio)
                     
                     
                     updateSelectInput(session, 'inf_uf_obito_resp', 'Estado', choices = lapply(split(municipios_br, municipios_br$regiao), function(x){unique(x$uf)}),
                   selected = selected$uf_responsavel)
                             
                             updateSelectInput(session, 'inf_municobito_resp', 'Município: ', choices = municipios_br[municipios_br$uf == selected$inf_uf_obito,'municipio'],
                     selected = selected$municipio_responsavel)
       
                     updatePickerInput(session, 'inf_localobito', 'Local do óbito:', 
         choices = c('Hospital', 'Via pública', 'Domicílio', 'Outro serviço de saúde','Aldeia indígena'),
         selected = selected$local_obito)
                   
                    updateTextAreaInput(session, 'inf_infoadicional', 'Informações adicionais:',value = selected$info_adicional)
                          
                    updateTextInput(session, 'inf_nome', 'Nome:', value = selected$paciente)
                    
                    updateTextInput(session, 'inf_declaracao_vivo', 'Número da declaração de Nascido Vivo:', value = selected$dec_nascidovivo) 
                                  
                    updateTextInput(session, 'inf_do', 'Número da Declaração de Óbito (DO):', value = selected$dec_obito)
       
                    updateDateInput(session, 'inf_nascimento', 'Data de nascimento:', value = selected$dat_nascimento)
                    updateDateInput(session, 'inf_obito', 'Data de óbito:', value = selected$dat_obito)
                    updateRadioButtons(session, 'inf_sexo', 'Sexo:', choices = c('Masculino' = 1, 'Feminino' = 2),  selected = selected$sexo)
                    updateNumericInput(session, 'inf_peso', 'Peso ao nascer:', value = selected$idade)
                    
                    updateRadioButtons(session, 'inf_tipoidade', 'Tipo idade: ',
                     choices = c('Horas' = 1,'Minuto' = 2, 'NA' = 3), selected = selected$tipo_idade)
                    updateNumericInput(session, 'inf_idade_horamin', 'Idade:', value = selected$idade_horamin)
                    
                    if(selected$responsavel != 'mãe'){  
                    updateRadioButtons(session, 'inf_responsavel', 'Responsável: ', 
                                       choices = c('Pais' = 1, 'Responsável' = 2), selected =2)
                   updateTextInput(session,'inf_grau_parentesco', 'Informar grau de parentesco:', value = selected$responsavel)                    
                                       }
                    updateTextInput(session, 'inf_nomeresp', 'Nome:', value = selected$nome_responsavel)
                    updateTextInput(session, 'inf_nomerespII', 'Nome:', value = selected$nome_responsavel2)
                    updateNumericInput(session, 'inf_idade_resp', 'Idade:', value = selected$idade_responsavel)
                    updateSelectInput(session,'inf_escolaridade', 'Escolaridade:', 
        choices = c('Sem escolaridade' = 1,'Fundamental I'= 2,'Fundamental II' =3, 'Ensino médio' =4 ,'Superior Incompleto' = 5, 'Superior Completo' = 6), selected = selected$escolaridade)
                   updateSelectInput(session, 'inf_cor', 'Cor da pele:', choices = c('Branca' = 1, 'Preta' = 2, 'Amarela' = 3, 'Parda' = 4, 'Indígena' = 5),
      selected =  selected$cor_pele)
                   updateSelectInput(session, 'inf_conjug', 'Situação conjugal:', 
      choices = c('Casado' = 1, 'Solteiro' = 2, 'Viúvo' = 3, 'Separado/divorciado' = 4, 'União estável' = 5, 'Ignorado' = 6),
      selected = selected$sit_conjugal)
                   updateNumericInput(session, 'inf_idadegest', 'Idade gestacional:', 
      value = selected$idade_gest)
                   updateNumericInput(session, 'inf_qtdegest', 'Quantidade de gestações:', value = selected$qtde_gestacao)
                   
                   updateSelectInput(session, 'inf_parto', 'Tipo de parto:', choices = c('Vaginal' = 1, 'Cesáreo' = 2, 'Não se aplica' = 3),
                    selected = selected$tipo_parto)
                  
                   updateNumericInput(session, 'inf_qtdefilho', 'Número de filhos vivos:', 
                   value = selected$qtde_filhos)
                    
                   updateRadioButtons(session, 'inf_evitavel', 'Morte evitável? ', choices = c('Sim' = 1, 'Não' = 2), selected = selected$obt_evitavel)
                   updateSelectizeInput(session, 'inf_lista_cidevitavel', 'CID-10:', choices = sort(cid_evitavel[,2]), 
                     selected = selected$cid_evitavel)
                   
                   #add em 20-out-2022
                   updateTextInput(session, 'inf_ubsvinc', 'UBS de Vinculação:', value = selected$ubs_vinculo)
                   updateTextInput(session, 'inf_equipe', 'Equipe responsável:', value = selected$equipe_responsavel)
                   updateRadioButtons(session, 'inf_acs_ativo', 'Possui ACS ativo? ', choices = c('Sim' = T, 'Não' = F), selected = selected$acs_ativo)               
                   updateNumericInput(session, 'inf_consultas', 'Quantas consultas foram realizadas pela APS nos 3 meses anteriores ao óbito?',
                   value = selected$consultas_aps)
                   updateTextInput(session, 'inf_bairrolocalobito', 'Bairro da ocorrência:', value = selected$bairro)
                   
                   #ajeitar aqui (23-out-22, 2304h)
                   municipio <- municipios_br[(municipios_br[,'municipio'] == selected$municipio) & 
                                              ( municipios_br$uf == selected$inf_uf_obito),'codigo']
           municipio <- floor(municipio/10)
         hospital <- dado_estab[dado_estab$CO_MUNICIPIO_GESTOR %in% municipio,'NO_FANTASIA']
         updateSelectizeInput(session, 'inf_hospital', "Estabelecimento de Saúde:",
                           choices = hospital, selected = selected$hospital)
                  
                  updateSelectizeInput(session, 'inf_ocupacao', 'Ocupação:', choices = cbo_list[,2], selected = selected$ocupacao_exercida) 
                                  }
    if(input$consult_formulario == 3){
                                 updateNavbarPage(session, 'navbar', selected = "Educação")
                                  Sys.sleep(.1)
                                  updateTabsetPanel(session, 'panel_infreq', selected = 'Infrequência escolar')
                                  
                                       
                            #add em 24-mar-22 (09:13h)
                          
                             updateSelectInput(session, 'infreq_municipio', 'Municípío:', choices = c(municipio_prog$municipio),
                             selected = selected$municipio)
                            
                   
                             updateRadioButtons(session, 'infreq_periodoatipico', 'Contexto do período:',
        choices = c('Normal' = 1,'Atípico' = 2), selected = selected$periodo_atipico)
                   
                    updateTextAreaInput(session, 'infreq_infoadicional', 'Informações adicionais:',value = selected$infreq_adicional)
                          
                    updateTextInput(session, 'infreq_nome', 'Nome:', value = selected$aluno)
                    updateTextInput(session, 'infreq_cpf', 'CPF:', value = selected$aluno_cpf)
                    
                    updateDateInput(session, 'infreq_nascimento', 'Data de nascimento:', value = selected$dat_nascimento)
                    #continuar daqui (24-mar-22)
                    
                   
                    updateRadioButtons(session, 'infreq_pessoaresid','Com quem reside?',
                    choices = c('Pais' = 2, 'Representante legal' = 3,  'Outros' = 5),
                    selected = selected$pessoa_responsavel)
      
                    updateTextInput(session, 'infreq_responsavel', 'Nome:', value = selected$nome_responsavel)
                    updateTextInput(session, 'infreq_responsavel_2', 'Nome:', value = selected$nome_responsavel_2) #add em 20-jul-22
                    updateTextAreaInput(session, 'infreq_residinfoadicional', 'Informações adicionais:', value = selected$info_adicional_resp)
                    
                    updateSelectInput(session, 'infreq_munic_resid', 'Município:', 
                                      choices = c(municipiopoly$Municipio, 'Outro estado'), selected = selected$municipio_residencia)
                    
                    updateTextInput(session,'infreq_endereco_resid', 'Endereço:', value = selected$endereco_residencia)
                    updateTextInput(session,'infreq_bairro_resid', 'Bairro: ', value = selected$bairro_residencia)
                    updateRadioButtons(session, 'infreq_zonaresid','Zona de residência:',
                    choices = c('Urbano' = 1, 'Rural' = 2, 'Sem informação' = 3),
                    selected = selected$zona_residencia)
                    
                    
                    updateSelectInput(session, 'infreq_serie', 'Ano:', choices = educ_series, 
                    selected = selected$serie)
                    updateTextInput(session,'infreq_turma', 'Turno de matrícula:', value = selected$turma)
                    
                    #updateNumericInput(session, 'infreq_responsavelcpf', 'CPF:',value = selected$cpf_responsavel)
                    
                    updateDateInput(session,'infreq_falta1', 'Data falta 1:', value = selected$falta_1)
                    updateDateInput(session,'infreq_falta2', 'Data falta 2:', value = selected$falta_2)
    		            updateDateInput(session,'infreq_falta3', 'Data falta 3:', value = selected$falta_3)
                    updateDateInput(session,'infreq_falta4', 'Data falta 4:', value = selected$falta_4)
     		            updateDateInput(session,'infreq_falta5', 'Data falta 5:', value = selected$falta_5)
                    updateDateInput(session,'infreq_falta6', 'Data falta 6:', value = selected$falta_6)
		                updateDateInput(session,'infreq_falta7', 'Data falta 7:', value = selected$falta_7)    
                  
                    updateSelectInput(session, 'infreq_ativa', "Causas secundárias: ", choices = escolhas_categoria,
                        selected = strsplit(selected$possiveis_causas, ' / ')[[1]]) 
                    
                    updateSelectInput(session, 'infreq_principal', "Causa principal: ", choices = escolhas_categoria,
                        selected = selected$possiveis_causas_principal)
                        
                    #add em 31-out-2022
                    updateSelectInput(session, 'infreq_situacao', 'Situação no APOIA:', choices = c('Em andamento - UE', 'Em andamento - CT',
     'Em andamento - MP', 'Êxito - UE', 'Êxito - CT', 'Êxito - MP', 'Suspenso - UE', 'Suspenso - CT', 'Suspenso - MP', 'Cancelamento - UE',
     'Maioridade - UE', 'Maioridade - CT', 'Maioridade - MP', 'Arquivado - MP', 'Arquivado - Maioridade MP', 'Arquivado - Não localizado MP',
     'Arquivado - Ação Ajuizada', 'Arquivado - Redirecionado Rede-MP', 'MP-PA individual Instaurado'), 
       selected = selected$situacao_apoia)
                                     
                   updateRadioButtons(session, 'infreq_sexo','Sexo:',
                    choices = c('Masculino' = 1, 'Feminino' = 2, 'Não se aplica' = 3),
                    selected = selected$sexo)                  
                                  }#end if3
     #add 11-nov-2022
    updateTextInput(session, 'infreq_matricula', 'Matrícula:', value = selected$infreq_matricula)
    
     #Sys.sleep(.8)

                             updateSelectInput(session, 'infreq_escola', 'Unidade escolar:',
                             choices = escola_programa[escola_programa$municipio == selected$municipio[1],'nome_ue'],
                             selected = selected$escola[1])
    
    })
    
    
    
 #--------------------------------------------------------------------------
 #pdf gerado
 #--------------------------------------------------------------------------
 #elaborado em 29-mar-22 (15:33h)

 output$print_form <- downloadHandler(
  
  # For PDF output, change this to "report.pdf"
  filename = function(){
    paste0("formulario_id", consult_data()[input$consult_listaforms_rows_selected,'id'],'_',Sys.Date() ,".pdf")
  },
  content = function(file) {
    #removeModal()

    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    tempReport <- tempdir()#file.path(tempdir())#file.path(tempdir(), "evento_pdf.Rmd")
        if(input$consult_formulario == 1){
        file.copy(c("markdown/longevidade_pdf.Rmd"), tempReport, overwrite = TRUE)}
        if(input$consult_formulario == 2){
        file.copy(c("markdown/mortalidade_infantil.Rmd"), tempReport, overwrite = TRUE)}
        if(input$consult_formulario == 3){
        file.copy(c("markdown/infrequencia_pdf.Rmd"), tempReport, overwrite = TRUE)}
        file.copy(c("markdown/mystyles.sty"), tempReport, overwrite = TRUE)
        file.copy(c("markdown/LOGO_GCAT-01.png"), tempReport, overwrite = TRUE)
    on.exit(tempReport)
    # Set up parameters to pass to Rmd document
    params_ <- list(
      data = consult_data()[input$consult_listaforms_rows_selected, ]#,
      #val_date = ymd(data_preenchimento)
    )

    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    withProgress(
    rmarkdown::render(
        if(input$consult_formulario == 1){
        paste0(tempReport,'/longevidade_pdf.Rmd')}else{
        if(input$consult_formulario == 2){
        paste0(tempReport,'/mortalidade_infantil.Rmd')}else{
        paste0(tempReport,'/infrequencia_pdf.Rmd')}},#"markdown/evento_pdf.Rmd",
        output_file = file,
        params = params_
      ),
      message = "Em processamento..."
    )

    #sendSweetAlert(session = session,
    #               title = "Success!",
    #               text = paste0("Report for ", member_(), " successfully generated!"),
    #               type = "success")
  }
)
  
  
  