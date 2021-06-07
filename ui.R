#
# NEXT WORD IS ... Application U.I.
#
# This application takes a user input string/phrase/sentence and "suggests"
# the next word to complete the thought
#


shinyUI(fluidPage(
    setBackgroundImage(
        src="SearchWordPlot_Shaded.png"),

    
    navbarPage(title="Next Word Predictor",

        tabPanel(title="Next Word Is ...",
            fluidRow(
                column(2,
                       div(img(src="ThinkingIcon.png",
                               align="center")),
                ),
                column(7, offset = 1,
                       tags$h2("And your next word is ...") 
                ),
            ),
            fluidRow(
                column(7, offset=2,
                       textInput(inputId = "userText", 
                                 label="Start typing ...", 
                                 value="the ",
                                 width="auto"),
                ),
            ),
            fluidRow(
                    column(7, offset=2,
                            formattableOutput("nextWordList")
                    )
            ),

        ),
        
        navbarMenu("About and Help",

            tabPanel(title="Help",
                    tags$p("Using the ", 
                            tags$em(tags$strong("Next Word Predictor ")),
                           "is easy.  Simply start typing into the text input box 
                            and the suggested next words will appear in blue below."),
                    tags$hr(),
                    imageOutput(outputId="howToUse", 
                                width="auto",height="auto")
                 
            ),
            tabPanel(title="about Next Word Predictor",
                     tags$p("Click page to go to presentation on Next Word Prediction"),
                     tags$a(imageOutput(outputId="aboutNextWord"),
                            href = "https://rpubs.com/jlranaliticas/nextWordIs")
                     
                     )
        )
)
)
)
