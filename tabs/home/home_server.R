
observeEvent(input$btn_candidatos, {
  updateTabsetPanel(session = session, inputId = "navbar", selected = "candidatos")
})

observeEvent(input$btn_bairros, {
  updateTabsetPanel(session = session, inputId = "navbar", selected = "bairros")
})
