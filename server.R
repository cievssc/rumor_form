 #app gente catarina (28-dez-21, 22:38h)
 #auth0_server(function(input, output, session) {
 function(input, output, session) {
    # aparecer o formulário inicial de senha
    check_user <- NULL
 observeEvent(input$botao_acesso, {
  #saveData(formData())
  if(nrow(check_user()) == 1){
  #shinyjs::reset("form")
  shinyjs::hide("form")
  shinyjs::show("teste")}else{
  
  showModal(modalDialog(
        title = NULL,
        "Usuário ou senha incorreto",
        easyClose = TRUE,
        footer = NULL
      ))
  }
 })
 }
 #}, info = a0_1) #end server function
