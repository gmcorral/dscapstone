library(stringr)

#attach("capstone.RData")
load("capstone.RData")

# predict
predict.words <- function(phrase, nwords)
{
    if(is.null(phrase) | length(phrase) == 0 | nchar(str_trim(phrase)) == 0)
        as.character(head(all.1gram.df$w1, nwords))

    phrase <- str_trim(gsub("[^[:alpha:]]", " ", phrase))
    words <- unlist(strsplit(phrase, "( )+"))
    size <- length(words)

    if(size >= 3)
        predict.3(words[size-2], words[size-1], words[size], nwords)
    else if(size == 2)
        predict.2(words[1], words[2], nwords)
    else
        predict.1(words[1], nwords)
}

predict.3 <- function(word1, word2, word3, nwords)
{
    candidates <- as.character(all.4gram.df[all.4gram.df$w1 == word1 &
                                                all.4gram.df$w2 == word2 &
                                                all.4gram.df$w3 == word3, 4])
    nres <- length(candidates)
    if(nres >= nwords)
        candidates[1:nwords]
    else
        unique(c(candidates, predict.2(word2, word3, nwords - nres)))
}

predict.2 <- function(word1, word2, nwords)
{
    candidates <- as.character(all.3gram.df[all.3gram.df$w1 == word1 &
                                                all.3gram.df$w2 == word2, 3])
    nres <- length(candidates)
    if(nres >= nwords)
        candidates[1:nwords]
    else
        unique(c(candidates, predict.1(word2, nwords - nres)))
}

predict.1 <- function(word1, nwords)
{
    candidates <- as.character(all.2gram.df[all.2gram.df$w1 == word1, 2])
    nres <- length(candidates)
    if(nres >= nwords)
        candidates[1:nwords]
    else
        unique(c(candidates, as.character(head(all.1gram.df$w1, nwords - nres))))
}


shinyServer(

    function(input, output, session)
    {
        #suggested.words <- reactiveValues(suggestions = c("the", "a"))

        pred <- reactive({
            phrase <- input$phrase
            words <- list()
            if(!is.null(phrase) & length(phrase) > 0)
            {
                words <- predict.words(phrase, input$nwords)
                #suggested.words <- reactiveValues(suggestions = words[!is.na(words)])
                words <- words[!is.na(words)]
            }
print(words)
            words
        });

        observe ({
            sugg <- input$suggestions
            if(!is.null(sugg))
            {
                isolate ({ val <- paste(input$phrase, sugg, sep = " ") })
                updateTextInput(session, "phrase", value = val)
            }
        })
        #output$prediction <- renderPrint({
        #    pred()
        #});

        #output$predlist <- renderPrint({
        #    pred()
            #isolate({
            #    updateSelectizeInput(session, "suggestions", choices = pred(), selected = NULL)
            #})
        #})

        #output$predlist <- renderUI({
        #    selectizeInput('suggestions', 'Suggestions', pred())
        #})

        #reactive({ updateSelectizeInput(session, 'foo', choices = pred(), server = TRUE) })
        #updateSelectizeInput(session, 'foo', choices = predict(input$foo, input$nwords), server = TRUE)

        output$suggestions <- renderUI({
                #words <- pred()
                selectInput("suggestions", label="Suggestions:", choices = NULL,
                          #options = list(placeholder = 'Type a phrase', create = FALSE,
                          #               openOnFocus = TRUE, hideSelected = FALSE,
                          #               closeAfterSelect = FALSE,
                          #               dropdownParent = 'body', create = TRUE),
                          selectize = FALSE,
                          selected = FALSE, multiple = TRUE)

        })

        observe({
            #input$suggestions
        #    alist <- list(text = input$suggestions, suggestions = pred())
            words <- pred()
        #    print(alist)
        #    if (is.null(input$suggestions) | length(input$suggestions) == 0)
        #        return()

        #    print(input$suggestions)

        #    session$words <- predict(input$suggestions, input$nwords)
        #    session$words <- session$words[!is.na(session$words)]
        #    print(session$words)

            isolate({
                print("4")
                updateSelectInput(session, "suggestions", choices = words, selected = NULL)
            })#iso
        })#obs
    }
)