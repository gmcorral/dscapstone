library(shiny)
library(markdown)
library(shinyBS)

shinyUI
(
    fluidPage
    (

        tags$head(
            tags$title("Next word predictor"),
            tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');")),
            tags$style(type="text/css",
                    "input {
                        width: 400px;
                    }
                    select[size], select[multiple] {
                        width: 400px;
                        height: 210px;
                        color: #333333;
                    }
                    label {
                       font-size:15px;
                       display:inline-block;
                       padding-top: 5px;
                       padding-bottom: 5px;
                    }
                    h6 {
                       font-size:15px;
                       padding-top: 5px;
                       padding-bottom: 5px;
                    }
                    h4 {
                       font-size:20px;
                    }")
        ),

        theme = "bootstrap.min.3.css",

        fluidRow(
            column
            (
                12,
                headerPanel(h1("Next word predictor",
                            style = "font-family: 'Lobster';
                                     font-weight: 500;
                                     font-size:50px;
                                     line-height: 1.1;
                                     color: #ffffff;"))
            )
        ),
        fluidRow(
            column
            (
                10, offset = 1,

                helpText(h4("I'll predict the next word in your text!")),

                sliderInput('nwords', label = "How many words do you want me to predict?",
                                value = 3, min = 1, max = 10, step = 1),

                helpText(h6("Please type your text here:")),
                tags$textarea(id="phrase", rows=3, cols=35),

                uiOutput("suggestions")
            )
        ),
        fluidRow(
            column
            (width = 1, offset = 1,
                bsButton("help", "( help! )", style = "default", type = "action", block = FALSE),
                bsPopover("help", "This app tries to predict the next word based on what you have typed so far and the analysis of a large corpora of text, stored as n-grams. Choose how many words you want the app to predict, start typing a sentence in the upper box and the predicted words will show up in the box below the sentence. If you want to append any of the predicted words to your text, just click on it. Have fun!",
                        trigger = "hover", placement = "top", options = list(container = "body"))
            ),
            column
            (width = 1,
                bsButton("about", "( about.. )", style = "default", type = "action", block = FALSE),
                bsPopover("about", "Coursera Data Scientist certification capstone, by Guillermo Menéndez",
                          trigger = "hover", placement = "top", options = list(container = "body"))
            )
        )
    )
)
