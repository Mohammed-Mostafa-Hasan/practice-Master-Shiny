library(ggplot2)
library(shiny)
# ui <- fluidPage(
#   theme = bslib::bs_theme(bg = "#0b3d91",
#                           fg = "white",base_font = "Source Sans Pro"),
#   sidebarLayout(
#     sidebarPanel(
#       textInput("txt", "Text input:", "text here"),
#       sliderInput("slider", "Slider input:", 1, 100, 30)
#     ),
#     mainPanel(
#       h1(paste0("Theme: darkly")),
#       h2("Header 2"),
#       p("Some text")
#     )
#   )
#   
# )
# server <- function(input,output,session){
# }
library(ggplot2)
# ui <- fluidPage(
#   theme = bslib::bs_theme(bg = "#0b3d91",
#                           fg = "white",base_font = "Source Sans Pro"),
#   titlePanel("A themed plot"),
#   plotOutput("plot"),
# )
# ui <- fluidPage(
#   HTML(r"(
#        <h1>This is a heading</h1>
#        <p class="my-class">This is some text!</p>
#        <ul>
#        <li>First bullet</li>
#        <li>Second bullet</li>
#        </ul>
#   )")
# )

ui <- fluidPage(
  h1("This is a heading"),
  p("This is some text", class = "my-class"),
  tags$ul(
    tags$li("First bullet"),
    tags$li("Second bullet")
  ),tags$p(
    "You made ",
    tags$b("$", textOutput("amount", inline = TRUE)),
    " in the last ",
    textOutput("days", inline = TRUE),
    " days "
  )
)

server <- function(input, output, session) {
  thematic::thematic_shiny()
  output$plot <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      geom_smooth()
  }, res = 96)
}


shinyApp(ui,server)