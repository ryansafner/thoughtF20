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
    titlePanel("ECON 326 Course Grade Calculator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            h4("Input Actual or Hypothetical Grades For Each Assignment"),
            numericInput("disc",
                        "Discussion Average (20%):",
                        min = 0,
                        max = 5,
                        step=0.1,
                        value = 4),
            helpText("Discussion average out of 5"),
            numericInput("hw",
                         "Homework Average (30%):",
                         min = 0,
                         max = 100,
                         value = 95),
            helpText("Homework average out of 100"),
            numericInput("midterm",
                         "Midterm Grade (20%):",
                         min = 0,
                         max = 100,
                         value = 95),
            numericInput("final",
                         "Final Exam Grade (30%):",
                         min = 0,
                         max = 100,
                         value = 95)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h2("Your final course grade will be: "),
           h2(textOutput("printgrades"), align="center", style="color:green"),
           p("See the", a("syllabus", href="http://ios20.classes.ryansafner.com/syllabus"), "for details. Note that I reserve the right to boost the grades of those that I believe have consistently contributed to classroom discussion, by up to 2.5%."),
           p("Use this tool to estimate what grades on future assignments you would need to earn in order to target a specific course grade."),
           br(),
           br(),
           p("Made with",
             a("R and Shiny",
             href="https://shiny.rstudio.com/"),
             "for",
             a("Ryan Safner",
               href="http://ryansafner.com"),
             "'s", 
             a("Industrial Organization",
               href="http://ios20.classes.ryansafner.com"),
             "course at Hood College.")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$printgrades <- renderText({
        coursegrade<-round(((input$disc*4)+(input$hw*0.30)+(input$midterm*0.20)+(input$final*0.30)),4)
        lettergrade<-ifelse(coursegrade>=92.5, "A",
                            ifelse(coursegrade>=89.5, "A-",
                                   ifelse(coursegrade>=86.5, "B+",
                                          ifelse(coursegrade>=82.5, "B",
                                                 ifelse(coursegrade>=79.5, "B-",
                                                        ifelse(coursegrade>=76.5, "C+",
                                                               ifelse(coursegrade>=72.5, "C",
                                                                      ifelse(coursegrade>=69.5, "C-",
                                                                             ifelse(coursegrade>=66.5, "D+",
                                                                                    ifelse(coursegrade>=62.5, "D",
                                                                                           ifelse(coursegrade>=59.5, "D-", "F")))))))))))
        printgrades<-paste(coursegrade, ": ", lettergrade)
    
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
