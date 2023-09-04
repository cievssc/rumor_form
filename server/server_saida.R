 #04-set-23 (13:37h)
 #botão saída

 observeEvent(input$navbar,{
   if(input$navbar == 'Sair'){
   showModal(modalDialog(
        title = NULL,
        "Você realmente deseja sair?",
        br(),
        tags$div(
            actionButton('sair_sim','Sim'),
            actionButton('sair_nao','Não')
        ),
        easyClose = TRUE,
        footer = NULL
      ))

   }

 #não sair
  observeEvent(input$sair_nao,{
    removeModal()
    updateNavbarPage(session, 'navbar', selected = "Formulário")
  }) 

 #sim sair
  observeEvent(input$sair_sim, {
   shinyjs::reset("form")
   shinyjs::show("form")
   
   check_user(0) 
   removeModal()

   shinyjs::reset("teste")
  })
 


 })
   
