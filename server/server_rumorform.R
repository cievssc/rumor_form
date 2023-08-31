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
       # estado(input$rumor_estado)
    }else{
       NULL}
})


#Municípios
municipio <- reactiveVal(NA)
output$rumor_municipio_ui <- renderUI({
    if(input$rumor_pais == 'Brasil' & input$rumor_estado != 'Nacional'){
        selectInput('rumor_municipio', 'Município:', choices = municipios_br[municipios_br$uf == 'Santa Catarina','municipio'],
        selected = 'Florianópolis', multiple = F)
       # municipio(input$rumor_municipio)
    }else{
        NULL
        #municipio(NA)
        }
})

observeEvent(input$rumor_municipio,{
 if(input$rumor_pais == 'Brasil' & input$rumor_estado != 'Nacional'){
   dadoi <- municipios_br[municipios_br$uf == input$rumor_estado,] 
  updateSelectInput(session, 'rumor_municipio', 'Município', choices = dadoi[,'municipio'])
 }else{NULL}}, ignoreInit = T
)

