library(shiny)
library(markdown)
shinyUI
(
    fluidPage
    (

        tags$head(
            tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    #overflow: visible;
                    #visibility: visible;
                    #display: block;
                    #.selectize-dropdown-content {
                    #    height: 600 px;
                    #    width: 700 px;
                    #    size: 10;
                    #    max-height: 600px;
                    #    background-color: #43f43b;}

                    input {
                        width: 400px;
                    }

                    select[size], select[multiple] {
                        width: 400px;
                        height: 200px;
                        color: #333333;
                    }
                    "))
        ),

        theme = "bootstrap.min.3.css",

        fluidRow(
            column
            (
                12,
                headerPanel(h1("Next word predictor",
                               style = "font-family: 'Lobster';
                                font-weight: 500; line-height: 1.1;
                                color: #ffffff;"))
            )
        ),
        fluidRow(
            column
            (
                6, offset = 1,

                helpText(h4("I'll predict your next word!")),

                sliderInput('nwords', 'How many words do you want to be suggested?',
                                value = 3, min = 1, max = 10, step = 1),

                textInput("phrase", "Type your text here:"),

                #verbatimTextOutput("predlist"),

                #selectizeInput('sugg', label="Type your text here:", choices = NULL,
                #              options = list(placeholder = 'Type a phrase'))

                uiOutput("suggestions")
            )
        )
    )
)
