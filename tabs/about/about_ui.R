sobre <-
  tabPanel(
    title = "Sobre",
    value = "about",
    column(
      width = 2
    ),
    column(
      width = 8,
      HTML(
        "<h4><center>Esse projeto foi desenvolvido com o objetivo de explorar os dados disponibilizados pelo TSE sobre as eleições municipais
      para o município de Porto Alegre, em 2020. Qualquer dúvida, sugestão ou correção, favor referenciar o projeto no GitHub
      e abrir uma issue lá.</center></h4>"
      ),
      br(), br(),
      column(
        width = 12,
        HTML(
          '<h4><strong>Sobre o desenvolvedor: </strong></h4>'
        ),  
        h2(
          a(
            href = "https://paeselhz.github.io/",
            icon("globe"),
            target = "_blank"
          ),
          a(
            href = "https://linkedin.com/in/lhzpaese",
            icon("linkedin"),
            target = "_blank"
          ),
          a(
            href = "https://github.com/paeselhz",
            icon("github"),
            target = "_blank"
          ),
          a(
            href = "https://twitter.com/paeselhz",
            icon("twitter"),
            target = "_blank"
          )
        )
      )
    )
  )