library(shiny)
library(ggplot2)
library(vroom)
library(tidyverse)
# library(neiss)
# library(devtools)
# ui <- fluidPage(
#   selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
#   verbatimTextOutput("summary"),
#   tableOutput("table")
# )
# server <- function(input, output, session) {
#   # Create a reactive expression
#   dataset <- reactive({
#     get(input$dataset, "package:datasets")
#   })
#   output$summary <- renderPrint({
#     # Use a reactive expression by calling it like a function
#     summary(dataset())
#   })
#   output$table <- renderTable({
#     dataset()
#   })
# }
# shinyApp(ui,server)

# ui <- fluidPage(
#   titlePanel("User greets"),
#   numericInput("age", "How old are you?", value = NA),
#   textInput("name", "What's your name?"),
#   textOutput("greeting")
#   
# )
# 
# server <- function(input, output, session){
#   output$greeting <- renderText({
#     paste0("Hello ", input$name)
#   })
#--------------------------------------------------------------------------------
#   ui <- fluidPage(
#     sliderInput("x", label = "If x is", min = 1, max = 50, value = 30),
#     sliderInput("multiplier",label = "mulipiler value",min = 1,max = 10,value = 3),
#     "then x times y is",
#     
#     textOutput("product")
#     
#   )
#   server <- function(input, output, session) {
#     output$product <- renderText({
#       input$x * input$multiplier
#     })
#   }
#   
# 
# 
# shinyApp(ui,server)

# ui <- fluidPage(
#   sliderInput("x", "If x is", min = 1, max = 50, value = 30),
#   sliderInput("y", "and y is", min = 1, max = 50, value = 5),
#   "then, (x * y) is", textOutput("product"),
#   "and, (x * y) + 5 is", textOutput("product_plus5"),
#   "and (x * y) + 10 is", textOutput("product_plus10")
# )
# server <- function(input, output, session) {
#   product <- reactive({input$x*input$y})
#   
#   output$product <- renderText({
#    product()
#   })
#   output$product_plus5 <- renderText({
#   product()+5
#   })
#   output$product_plus10 <- renderText({
#   product()+10
#   })
# }
# shinyApp(ui, server)

#-----------------------------------------------------------------

# library(shiny)
# library(ggplot2)
# datasets <- c("economics", "faithfuld", "seals")
# ui <- fluidPage(
#   selectInput("dataset", "Dataset", choices = datasets),
#   verbatimTextOutput("summary"),
#   plotOutput("plot")
# )
# server <- function(input, output, session) {
#   dataset <- reactive({
#     get(input$dataset, "package:ggplot2")
#   })
#   output$summary <- renderPrint({
#     summary(dataset())
#   })
#   output$plot <- renderPlot({
#     plot(dataset())
#   }, res = 96)
# }
# shinyApp(ui, server)

#-------------------------Exercise ch2-------------------------
 # ui = fluidPage(
 #   titlePanel("Exercise ch2"),
 #   
 #   textInput("name","User Name",value = "Your name"),
 #   sliderInput("date","When should we deliver",min = as.Date('2020-09-01','%Y-%m-%d'),max = as.Date('2020-09-30','%Y-%m-%d'),value = as.Date('2020-09-02',"%Y-%m-%d"),step = 1),
 #   sliderInput("animate","slider wity animation",min = 1,max = 100,value = 5,animate = TRUE),
 #   animationOptions()
 # 
 #   )
 # 
 # server <- function(input,output,session){}
 # 
 # shinyApp(ui,server)


#----------------------------CHAPTER 3 Basic Reactivity------------------------------------------------

# ui <- fluidPage(
#   textInput("name", "What's your name?"),
#   textOutput("greeting")
# )
# 
# server1 <- function(input, output, server) {
#   output$greeting <- renderText(paste0("Hello ", input$name))
# }
# server2 <- function(input, output, server) {
#   string <- reactive( paste0("Hello ", input$name))
#   output$greeting <- renderText(string())
# }
# server3 <- function(input, output, server) {
#   String <-reactive( paste0("Hello"," ", input$name))
#   output$greeting <- renderText(String())
# }
# 
# shinyApp(ui,server3)

# freqpoly <- function(x1,x2,binwidth = 0.1, xlim = c(-3,3)){
#   df <- data.frame(x = c(x1,x2),
#   g <- c(rep("x1",length(x1)),rep("x2",length(x2)))
#   )
#   ggplot(df,aes(x,colour = g))+
#     geom_freqpoly(binwidth = binwidth,size = 1.5)+
#     coord_cartesian(xlim = xlim)
# }
# t_test <-function(x1,x2) {
#   test <- t.test(x1,x2)
#   sprintf("p_value: %.3f\n confidant interval[%0.2f,%0.2f]",test$p.value, test$conf.int[1], test$conf.int[2])
# }

# ui <- fluidPage(
#   fluidRow(
#     column(4,
#            "Distribution 1",
#            numericInput("n1", label = "n", value = 1000, min = 1),
#            numericInput("mean1", label = "μ", value = 0, step = 0.1),
#            numericInput("sd1", label = "σ", value = 0.5, min = 0.1, step = 0.1)
#     ),
#     column(4,
#            "Distribution 2",
#            numericInput("n2", label = "n", value = 1000, min = 1),
#            numericInput("mean2", label = "μ", value = 0, step = 0.1),
#            numericInput("sd2", label = "σ", value = 0.5, min = 0.1, step = 0.1)
#     ),
#     column(4,
#            "Frequency polygon",
#            numericInput("binwidth", label = "Bin width", value = 0.1, step = 0.1),
#            sliderInput("range", label = "range", value = c(-3, 3), min = -5, max = 5)
#     )
#   ),
#   fluidRow(
#     column(9, plotOutput("hist")),
#     column(3, verbatimTextOutput("ttest"))
#   )
# )
#back end of the app without reactivety
# server <- function(input, output, session) {
#   output$hist <- renderPlot({
#     x1 <- rnorm(input$n1, input$mean1, input$sd1)
#     x2 <- rnorm(input$n2, input$mean2, input$sd2)
#     freqpoly(x1, x2, binwidth = input$binwidth, xlim = input$range)
#   }, res = 96)
#   output$ttest <- renderText({
#     x1 <- rnorm(input$n1, input$mean1, input$sd1)
#     x2 <- rnorm(input$n2, input$mean2, input$sd2)
#     t_test(x1, x2)
#   })
# }

#back end of the app with reactivety
# server <- function(input, output, session) {
#   x1 <-reactive( rnorm(input$n1, input$mean1, input$sd1))
#   x2 <-reactive( rnorm(input$n2, input$mean2, input$sd2))
#   output$hist <- renderPlot({
# 
#     freqpoly(x1(), x2(), binwidth = input$binwidth, xlim = input$range)
#   }, res = 96)
#   output$ttest <- renderText({
# 
#     t_test(x1(), x2())
#   })
# }

#simplfing the app and use poisson distribution (describe event per time period)
# ui <- fluidPage(
#   fluidRow(
#     column(3,
#            numericInput("lambda1", label = "lambda1", value = 3),
#            numericInput("lambda2", label = "lambda2", value = 5),
#            numericInput("n", label = "n", value = 1e4, min = 0)
#     ),
#     column(9, plotOutput("hist"))
#   )
# )
# # server <- function(input, output, session) {
# #   x1 <- reactive(rpois(input$n, input$lambda1))
# #   x2 <- reactive(rpois(input$n, input$lambda2))
# #   output$hist <- renderPlot({
# #     freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
# #   }, res = 96)
# # }
# server <- function(input, output, session) {
#   timer <- reactiveTimer(500)
#   x1 <- reactive({
#     timer()
#     rpois(input$n, input$lambda1)
#   })
#   x2 <- reactive({
#     timer()
#     rpois(input$n, input$lambda2)
#   })
#   output$hist <- renderPlot({
#     freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
#   }, res = 96)
# }
#------------------------------------------------------------------------------
# ui <- fluidPage(
#   fluidRow(
#     column(3,
#            numericInput("lambda1", label = "lambda1", value = 3),
#            numericInput("lambda2", label = "lambda2", value = 5),
#            numericInput("n", label = "n", value = 1e4, min = 0),
#            actionButton("simulate", "Simulate!")
#     ),
#     column(9, plotOutput("hist"))
#   )
# )
# 
# server <- function(input, output, session) {
#   x1 <- reactive({
#     input$simulate
#     rpois(input$n, input$lambda1)
#   })
#   x2 <- reactive({
#     input$simulate
#     rpois(input$n, input$lambda2)
#   })
#   output$hist <- renderPlot({
#     freqpoly(x1(), x2(), binwidth = 1, xlim = c(0, 40))
#   }, res = 96)
# }
#---------------------------------Observers----------------------------


# ui <- fluidPage(
#   textInput("name", "What's your name?"),
#   textOutput("greeting")
# )
# server <- function(input, output, session) {
#   string <- reactive(paste0("Hello ", input$name, "!"))
#   output$greeting <- renderText(string())
#   observeEvent(input$name, {
#     message("Greeting performed")
#   })
# }
# 
# shinyApp(ui,server)


#----------------------------------------------------------------------CH_4----------

# install.packages("devtools")
# install_github("hadley/neiss")
#--------------TRY to read data in different ways
# top_prod <- injuries %>%
#   filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
#   count(prod1, sort = TRUE) %>%
#   filter(n > 5 * 365)

#dir.create("neiss")

#> Warning in dir.create("neiss"): 'neiss' already exists
# download <- function(name) {
#   url <- "https://github.com/hadley/mastering-shiny/raw/master/neiss/"
#   download.file(paste0(url, name), paste0("neiss/", name), quiet = TRUE)
# }
# 
# download("injuries.tsv.gz")
# download("population.tsv")
# download("products.tsv")

# injuries <- vroom::vroom("neiss/injuries.tsv.gz")
# colnames(injuries)
# head(injuries$narrative,10)
# 
# products <- vroom::vroom("neiss/products.tsv")
# head(products)
# 
# population <- vroom::vroom("neiss/population.tsv")
# population

# ------------------------Exploration-----------------------------------------
# selected <- injuries %>% filter(prod_code == 649)
# nrow(selected)
# head(selected)
# #Note that I weight by the weight variable so that
# #the counts can be interpreted as estimated total injuries across the whole US
# selected %>% count(location, wt = weight, sort = TRUE)
# selected %>% count(body_part, wt = weight, sort = TRUE)
# selected %>% count(diag, wt = weight, sort = TRUE)

#explore the pattern across age and sex
# summary <- selected %>%
#   count(age, sex, wt = weight)
# summary
# 
# ggplot(summary,aes(age, n, colour = sex)) +
#   geom_line(size = 1) +
#   labs(y = "Estimated number of injuries")
# One problem with interpreting this pattern is that we know that there are fewer older
# people than younger people, so the population available to be injured is smaller. We
# can control for this by comparing the number of people injured with the total population
# and calculating an injury rate. Here I use a rate per 10,000
# summary <- selected %>%
#   count(age, sex, wt = weight) %>%
#   left_join(population, by = c("age", "sex")) %>%
#   mutate(rate = n / population * 1e4)
# summary  
# 
# #plot the result
# summary %>%
#   ggplot(aes(age, rate, colour = sex)) +
#   geom_line(size = 0.9,na.rm = TRUE) +
#   labs(y = "Injuries per 10,000 people")
# 
# # we can look at some of the narratives. Browsing through these is an informal
# # way to check our hypotheses and generate new ideas for further exploration
# selected %>%
#   sample_n(10) %>%     #pull out a random sample of 10
#   pull(narrative)
# #-------------------Prototype---------------------------------------------
# # prod_codes <- setNames(products$prod_code, products$title)
# 
# ui <- fluidPage(
#   # fluidRow(
#   #   column(10,
#   #          selectInput("code", "Product", choices = prod_codes)
#   #   )
#   # )
#   fluidRow(
#     column(8,
#            selectInput("code", "Product",
#                        choices = setNames(products$prod_code, products$title),
#                        width = "100%")
#     ),
#     column(2, selectInput("y", "Y axis", c("rate", "count")))
#   ),
#   
#   fluidRow(
#     column(4, tableOutput("diag")),
#     column(4, tableOutput("body_part")),
#     column(4, tableOutput("location"))
#   ),
#   fluidRow(
#     column(12, plotOutput("age_sex"))
#   ),
#   fluidRow(
#     column(2, actionButton("story", "Tell me a story")),
#     column(10, textOutput("narrative"))
#   )
# )
# server <- function(input, output, session) {
#   selected <- reactive(injuries %>% filter(prod_code == input$code))
#   output$diag <- renderTable(count_top(selected(), diag), width = "100%")
#   output$body_part <- renderTable(count_top(selected(), body_part), width = "100%")
#   output$location <- renderTable(count_top(selected(), location), width = "100%")
#   summary <- reactive({
#     selected() %>%
#       count(age, sex, wt = weight) %>%
#       left_join(population, by = c("age", "sex")) %>%
#       mutate(rate = n / population * 1e4)
#   })
#   #Polish Tables
#   
#   # output$age_sex <- renderPlot({
#   #   summary() %>%
#   #     ggplot(aes(age, n, colour = sex)) +
#   #     geom_line(size = 1) +
#   #     labs(y = "Estimated number of injuries")
#   # }, res = 96)
#   
#   #Rate Versus Count
#   output$age_sex <- renderPlot({
#     if (input$y == "count") {
#       summary() %>%
#         ggplot(aes(age, n, colour = sex)) +
#         geom_line() +
#         labs(y = "Estimated number of injuries")
#     } else {
#       summary() %>%
#         ggplot(aes(age, rate, colour = sex)) +
#         geom_line(na.rm = TRUE) +
#         labs(y = "Injuries per 10,000 people")
#     }
#   }, res = 96)
#   #<< narrative-server
#   narrative_sample <- eventReactive(
#     list(input$story, selected()),
#     selected() %>% pull(narrative) %>% sample(1)
#   )
#   output$narrative <- renderText(narrative_sample())
#   #>>
# }
# 
# injuries %>%
#   mutate(diag = fct_lump(fct_infreq(diag), n = 5)) %>%
#   group_by(diag) %>%
#   summarise(n = as.integer(sum(weight)))
# count_top <- function(df, var, n = 5) {
#   df %>%
#     mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
#     group_by({{ var }}) %>%
#     summarise(n = as.integer(sum(weight)))
# }
shinyApp(ui,server)






































