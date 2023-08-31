 tabPanel(title = 'Consultar formulário',
 
 fluidRow(
          column(3,
           selectInput('consult_formulario', 'Selecionar formulário:',
           choices = list('Saúde' = c('Longevidade' = 1, 'Mortalidade Infantil' = 2), 'Educação' = c('Infrequência Escolar' = 3)),
           selected = 1, multiple = F),
           actionButton('consult_consultar','Consultar')
           ),
           column(3,
           dateRangeInput("consult_periodo", label = "Período: ", min = (Sys.Date()-180), format = ' dd/mm/yyyy')
           )
           ), #endrow
           br(),
        
        h3('Relação dos formulários'),
        br(), 
        DT::dataTableOutput("consult_listaforms"),
        
        br(),
        actionButton('consult_editar','Editar'),
        br(),
        actionButton('consult_apagar','Apagar'),
        br()
        #,
        #downloadButton('print_form','Exportar')
     ) #endtabPanel consult 
     
  