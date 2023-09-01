 #app rumor form (31-ago-2023, 14:16h)
 #auth0_server(function(input, output, session) {
 function(input, output, session) {

 check_user <- reactiveVal(0)

  # aparecer o formulário inicial de senha
 observeEvent(input$botao_acesso, {
  query <- DBI::sqlInterpolate(conn(), 
            paste0("SELECT * FROM lista_usuario 
            WHERE login = ?code1 AND senha = ?code2"),
            code1 = input$login,
            code2 = digest::digest(as.character(input$email))
                        )
  dadoi <- DBI::dbGetQuery(conn(),query)
 
  
  if(nrow(dadoi) == 1){
  shinyjs::reset("form")
  shinyjs::hide("form")
  shinyjs::show("teste")
  check_user(dadoi)
  }else{
  
  shinyjs::reset("form")
  showModal(modalDialog(
        title = NULL,
        "Usuário ou senha incorreto",
        easyClose = TRUE,
        footer = NULL
      ))
  }
 }) #end observeEvent


 #---------------------------------------------------------------------#
 #rumor_form
 source('./server/server_rumorform.R', local = T, encoding = 'UTF-8')

 #rumor_consult
 source('./server/server_consult.R', local = T, encoding = 'UTF-8')

 } #end server function
 #}, info = a0_1) #end server function
