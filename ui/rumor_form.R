 tabPanel(title = 'Educação',
  tabsetPanel(id = 'panel_infreq' ,
  
   tabPanel('Infrequência escolar',
    
    tags$div(class = 'container',
    tags$form(
    h4('Local da unidade escolar'),
    fluidRow(
     
      column(4,
      selectInput('infreq_municipio', 'Município:', choices = c(municipio_prog$municipio),
      selected = municipio_prog$municipio[1]),
      selectInput('infreq_escola', 'Unidade escolar:', choices = escola_programa$nome_ue,
      selected = escola_programa$nome_ue[1], multiple = F),
     ),
      column(4,
             uiOutput('infreq_uiescola')),
       column(4,
        textAreaInput('infreq_infoadicional', 'Informações adicionais:', width = '100%', height = '110px',
         resize = 'vertical') )
      ), #endrow
      fluidRow(
       column(5,
        radioButtons('infreq_periodoatipico', 'Contexto do período:',
        choices = c('Normal' = 1,'Atípico' = 2), selected = 1, inline = T)
        ),
      
      ), #endRow
    hr(),
    h4('Aluno'),
    fluidRow(
     column(4,
      textInput('infreq_nome', 'Nome:'),
      textInput('infreq_cpf', 'CPF:'),
      dateInput('infreq_nascimento', 'Data de nascimento:', format = ' dd/mm/yyyy'),
      uiOutput('infreq_idade')
      ), #end column
     column(4,
      selectInput('infreq_munic_resid', 'Município:', choices = c(municipiopoly$Municipio, 'Outro estado'),
      selected = 'São Joaquim'), 
      textInput('infreq_endereco_resid', 'Endereço:'),
      textInput('infreq_bairro_resid', 'Bairro: '),
      radioButtons('infreq_zonaresid', 'Zona de residência: ',
      choices = c('Urbano' = 1, 'Rural' = 2, 'Sem informação' = 3),
      selected = 1)
      ), 
      column(4,
      radioButtons('infreq_sexo', 'Sexo: ',
      choices = c('Masculino' = 1, 'Feminino' = 2, 'Não se aplica' = 3),
      selected = 1),
      textInput('infreq_matricula', 'Matrícula:'),
      selectInput('infreq_serie', 'Ano:', choices = educ_series, 
       selected = educ_series[1], multiple = F),
      selectInput('infreq_turma', 'Turno de matrícula:', 
                  choices = c('Matutino', 'Vespertino', 'Noturno', 'Integral'),
                  selected = 'Matutino', multiple = F)
      )
      ), #endrow
    hr(),
     fluidRow(
     column(2,
      radioButtons('infreq_pessoaresid', 'Com quem reside?',
      choices = c('Pais' = 2, 'Representante legal' = 3,  'Outros' = 5),
      selected = 2, inline = T)),
      
      column(6,
      #uiOutput('infreq_comquemreside')),
      textInput('infreq_responsavel', 'Filiação 1:'),
      textInput('infreq_responsavel_2', 'Filiação 2:')
      
      ),
      column(4,
      textAreaInput('infreq_residinfoadicional', 'Informações adicionais:', width = '100%', height = '100px',
         resize = 'horizontal')
         )
      ), #endrow
     
     hr(),
     h4('Registro no APOIA*:'),
     br(),
     selectInput('infreq_situacao', 'Situação no APOIA:', choices = c('Em andamento - UE', 'Em andamento - CT',
     'Em andamento - MP', 'Êxito - UE', 'Êxito - CT', 'Êxito - MP', 'Suspenso - UE', 'Suspenso - CT', 'Suspenso - MP', 'Cancelamento - UE',
     'Maioridade - UE', 'Maioridade - CT', 'Maioridade - MP', 'Arquivado - MP', 'Arquivado - Maioridade MP', 'Arquivado - Não localizado MP',
     'Arquivado - Ação Ajuizada', 'Arquivado - Redirecionado Rede-MP', 'MP-PA individual Instaurado'), 
       selected = 'Em andamento - UE', multiple = F),
     
     fluidRow(
     column(3,
            dateInput('infreq_falta1', 'Data falta 1:', format = ' dd/mm/yyyy', language = 'br'),
            dateInput('infreq_falta2', 'Data falta 2:', format = ' dd/mm/yyyy', language = 'br')),
     column(3,
            dateInput('infreq_falta3', 'Data falta 3:', format = ' dd/mm/yyyy', language = 'br'),
            dateInput('infreq_falta4', 'Data falta 4:', format = ' dd/mm/yyyy', language = 'br')),
     column(3,
            dateInput('infreq_falta5', 'Data falta 5:', format = ' dd/mm/yyyy', language = 'br'),
            dateInput('infreq_falta6', 'Data falta 6:', format = ' dd/mm/yyyy', language = 'br')),       
     column(3,
            dateInput('infreq_falta7', 'Data falta 7:', format = ' dd/mm/yyyy', language = 'br'))       
            
            ),
     tags$small('*Em caso de 5 faltas consecutivas, preencher as datas de falta 6 e 7 igual a falta 5'), 
     hr(),
     h4('Possíveis causas:'), 
     fluidRow(
     column(3, 
            selectInput('infreq_principal', "Causa Primária: ", choices = escolhas_categoria,  multiple = F)),
     column(3, 
            selectInput('infreq_ativa', "Causas Secundárias: ", choices = escolhas_categoria_vetor, multiple = T)
            )
            ), #end row 
    br(),
    fluidRow(
    column(4, offset = 4,
    actionButton('infreq_enviar', 'Confirmar\npreenchimento', width  = '100%')
    
    )
     ), #endrow
     br()
     ) #end form
     )#end div
     ) #endtabPanel infrequência
     
     
    )#end tabsetPanel
    
    
    )#end tabPanel saude
  