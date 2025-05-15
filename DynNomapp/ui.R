ui = bootstrapPage(fluidPage(
    titlePanel('Neurodevelopmental Disorders in Children'),
    sidebarLayout(sidebarPanel(uiOutput('manySliders'),
                               uiOutput('setlimits'),
                               actionButton('add', 'Predict'),
                               br(), br(),
                               helpText('Press Quit to exit the application'),
                               actionButton('quit', 'Quit')
    ),
    mainPanel(tabsetPanel(id = 'tabs',
                          tabPanel('Graphical Summary', plotlyOutput('plot')),
                          tabPanel('Numerical Summary', verbatimTextOutput('data.pred')),
                          tabPanel('Model Summary', verbatimTextOutput('summary'))
    )
    )
    )))
