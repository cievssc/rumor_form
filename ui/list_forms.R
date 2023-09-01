 tabPanel(title = 'Consultar rumor',
 
 tags$div(id = 'consulta', style = 'padding = 20px;',
 fluidRow(
          column(3,
           dateRangeInput("consult_periodo", label = "Período: ",  format = ' dd/mm/yyyy'),
           actionButton('consult_consultar','Consultar')
           )
           ), #endrow
           hr(),
        
        h3('Relação de rumores'),
        br(), 
        DT::dataTableOutput("consult_listaforms"),
        
        br(),
        #actionButton('consult_editar','Editar'),
        br(),
        actionButton('consult_apagar','Apagar'),
        br(),
        downloadButton('consult_expo','Exportar')
        )
     ) #endtabPanel consult 
     
  