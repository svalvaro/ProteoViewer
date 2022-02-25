demo <- shiny::reactiveValues(start = FALSE)

observeEvent(input$Demo, {
    shiny::req(input$Demo)

    demo$start <-  TRUE

    message('Demo data being loaded')

    shinyalert::shinyalert("Demo Data Loaded",
                           "Now you can visualize some proteins!",
                           type="success",
                           closeOnClickOutside = TRUE,
                           closeOnEsc = TRUE,
                           timer = 6000)
})

observeEvent(input$saveExpDesign, {

    message('Experiment Design updated')


    shinyalert::shinyalert("Experiment Design saved",
                           "You can go back to the Results Tab",
                           type="success",
                           closeOnClickOutside = TRUE,
                           closeOnEsc = TRUE,
                           timer = 6000)

})
