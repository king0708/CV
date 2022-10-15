#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
     
     # Application title
     titlePanel("Nitrogen uptake rates and root traits"),
     
     # Sidebar with a slider input for number of bins 
     sidebarLayout(
          sidebarPanel(
               selectInput("trait", label = h3("Root Trait"), 
                           choices = list("Specific Root Length" = "srl", 
                                          "Root Dry Matter Content" = "rdmc", 
                                          "Root Tissue Density" = "rtd"), 
                           selected = "srl"), 
               checkboxGroupInput("Ntype", label = h3("Nitrogen form"), 
                                  choices = list("Glycine" = "Glycine", 
                                                 "Ammonium" = "NH4", 
                                                 "Nitrate" = "NO3"),
                                  selected = "Glycine"),
               checkboxGroupInput("species", label = h3("Species"), 
                                  choices = list("Pinus strobus" = "PIST", 
                                                 "Picea glauca" = "PIGL" 
                                  ),
                                  selected = "PIST"),
               radioButtons("transform", label = h3("Transformation (sqrt)"),
                            choices = list("None" = 0, "X-axis" = 1,
                                           "Y-axis" = 2, "Both" = 3), 
                            selected = 0)
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
               plotOutput("distPlot")
          )
     )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
     require(tidyverse)
     theme_N <- theme_bw()+
          theme(axis.text = element_text(size = 12), 
                axis.title = element_text(size =12))
     
     output$distPlot <- renderPlot({
          # generate bins based on input$bins from ui.R
          colors <- data.frame(n_treatment = c("Glycine", "NH4", "NO3"), 
                               color = c("green3", "royalblue3", "purple3"))
          long.dat <- read.csv("C:/Rachel/Working_oldcomp/Rachels_documents/Graduate_school/Projects/11_root_traits_N_uptake/11_data_files/old_or_duplicate_datafiles/Root_Trait_Nitrogen_Uptake_data_aug18.csv")
          x <- subset(long.dat, n_treatment %in% input$Ntype & species %in% input$species)
          colors <- filter(colors, n_treatment %in% input$Ntype)
          if(input$transform == "1" | input$transform == "3") {
               t <- input$trait
               x %>% 
                    select(t) %>%
                    mutate_all(funs(sqrt(.))) -> x$trait
               x$trait <- as_vector(x$trait)
          }
          if(input$transform == "2" | input$transform == "3"){
               x %>%
                    mutate(uptake_mg_g_hr = sqrt(uptake_mg_g_hr)) -> x
          } 
          if(input$transform == "0"| input$transform == "2"){
               x %>% select(input$trait) -> x$trait
               x$trait <- as_vector(x$trait)
          }
          
          g <- ggplot(x, aes(y = uptake_mg_g_hr, x = trait, color = n_treatment)) +
               geom_point()+
               geom_smooth(method = "lm")+
               theme_N+facet_wrap(~species)+
               scale_color_manual(values = as.character(colors$color))
          
          return(g)     
          
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

