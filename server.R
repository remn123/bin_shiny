## server.R ##

server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    theme_set(theme_bw())
    perf <- input$perf_rbtn
    
    if(perf=='perf_fis'){
      perf_if <- 'Bad Rate Física (%)'
      
    }
    else if(perf=='perf_fin'){
      perf_if <- 'Bad Rate Financeira (%)'
    }
    else if(perf=='cnt_qtd_cnpj'){
      perf_if <- 'Demografia'
    }
    
    # plot
    plot_obj <- ggplot(df_plot, aes(x=factor(pk_dt),
                                    y=df_plot[[perf]],
                                    colour=factor(score_bins))) + 
                  geom_point() +
                  geom_line(aes(group=factor(score_bins)), size=1) +
                  labs(title="Time Series of Bins", 
                       caption="Source: Itaú Unibanco", 
                       y=perf_if,
                       x='Referência',
                       color=NULL) +  # title and caption  
                  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
                        panel.grid.minor = element_blank())  # turn off minor grid
    
    plot_obj
  })

  output$tbl_train <- renderDataTable({ 
    refs <- c(201602, 201603, 201604, 201605, 201606, 201607)
    
    dfT_train <- generateDT(get_DT(df, refs, 1, input$var_inpt) %>% 
                                select(score_bins, !! input$var_inpt, !! input$perf2_rbtn),
                            input$var_inpt,
                            input$perf2_rbtn)
    bins <- dfT_train$Bins
    dfT_train<-dfT_train[!names(dfT_train)=='Bins']
    
    
    datatable(dfT_train,  
              selection = list(target = 'cell'),
              rownames = bins,
              options = list(dom = 't', columnDefs = list(list(className = 'dt-center', targets = 0:length(names(dfT_train))-1)))) %>% 
      formatStyle(names(dfT_train), textAlign = 'center') %>%
      formatPercentage(names(dfT_train), 1) %>%
      formatStyle(names(dfT_train), backgroundColor = styleInterval(c(1/100, 5/100, 10/100), c('red','orange','yellow','green')))

  })
  
  output$tbl_test <- renderDataTable({ 
      refs <- c(201602, 201603, 201604, 201605, 201606, 201607)
      
       
      
      dfT_test  <- generateDT(get_DT(df, refs, 0, input$var_inpt) %>% 
                                select(score_bins, !! input$var_inpt, !! input$perf2_rbtn),
                              input$var_inpt,
                              input$perf2_rbtn)
      datatable(dfT_test, 
                selection = list(target = 'cell'),
                options = list(dom = 'tp')) %>% 
          formatStyle(names(dfT_test), backgroundColor = styleInterval(c(1/100, 5/100, 10/100), c('red','orange','yellow','green')))
    
  })

  
  
  # output$text1 <- renderPrint({ # changed to renderPrint
  #   tree
  # })

  # output$plot <- renderPlot({
  #   rpart.plot(tree, 
  #              extra=104, box.palette="GnBu",
  #              branch.lty=3, shadow.col="gray", nn=TRUE)
  # })

  
  output$str <- renderPrint(str(input$node))
  
  output$tbl = renderDataTable(
    datatable(test, options =list(
      pageLength = 5)
  ))
  
  # Download the filtered data
  # output$downloadData = downloadHandler('test-filtered.csv', content = function(file) {
  #   write.csv('test', file)
  # })
  
  ## Modal Dialog
  # observeEvent(input$insertBtn, {
  #   showModal(modalDialog(
  #     title = "Important message",
  #     "This is an important message!",
  #     easyClose = TRUE
  #   ))
  # })
  
  
  
  
  ## 
  
  
  # filter <- character(0)
  # 
  # makeReactiveBinding("vars")
  # # makeReactiveBinding("cur_id")
  # # makeReactiveBinding("rmv_qtd")
  # vars <- list()
  # cur_id <- 0
  # rmv_qtd <- 0
  # # A notification ID
  # id <- NULL
  # # Add deveria ser uma fila ao inves de fixo ou lista
  # # add  <- queue()
  # 
  # dataSet <- df_init[ , -which(names(df_init) %in% c("CNPJ","MOB","DOB","bad_rate","SEGMENTO","NUM_MES_SAFRA","OVER_10","VC"))]
  # 
  # observeEvent(input$addFilter, {
  #   
  #   add <- input$addFilter - rmv_qtd
  #   cur_id <<- cur_id + 1
  #   
  #   filterId <- paste0('Filter_', add)
  #   xvarId <- paste0('xCol_Filter_', add)
  #   yvarId <- paste0('yCol_Filter_', add)
  #   confirmId <- paste0('Conf_Filter_', add)
  #   rowfilterId <- paste0('Row_Filter_', add)
  #   removeFilterId <- paste0('Remove_Filter_', add)
  #   tableId <- paste0('Table_', add)
  #   stepId <- paste0('Recorte # ', add)
  #   placeholdertableId <- paste0('placeholderTable_', add)
  #   tblId <- paste0('tbl_',add)
  #   headers <- names(dataSet)
  #   insertUI(
  #     selector = '#placeholderFilter',
  #     ui = tags$div(id = filterId,
  #                   
  #                   
  #                   box(title = stepId,solidHeader = TRUE,collapsible = TRUE, collapsed = TRUE,width = 12,background = cores[add],
  #                       hr(),
  #                       
  #                       fluidRow(
  #                         column(width=6,
  #                                selectInput(xvarId, label = "Variavel em x:", choices = as.list(headers), width = "50%", selected = 1)
  #                         ),
  #                         column(width=6,
  #                                selectInput(yvarId, label = "Variavel em y:", choices = as.list(headers), width = "50%", selected = 1)
  #                                
  #                         )
  #                       ),
  #                       hr(),
  #                       
  #                       
  #                       actionButton(confirmId, label = "Gerar Tabela"),
  #                       tags$div(id = placeholdertableId)
  #                   ),actionButton(removeFilterId, label = "x",style="float: right")
  #                   
  #                   
  #     )
  #   )
  #   
  #   
  #   observeEvent(input[[confirmId]], {
  #     
  #     xvars <- input[[xvarId]]
  #     yvars <- input[[yvarId]]
  #     xvalues <- as.list(unique(dataSet[xvars]))[[1]]
  #     yvalues <- as.list(unique(dataSet[yvars]))[[1]]
  #     
  #     
  #     # updateCheckboxGroupInput(session, rowfilterId , label = "Select variable    values", 
  #     #                          choices = values, selected = values, inline = TRUE)
  #     
  #     vars[[filterId]]$col  <<- xvars
  #     vars[[filterId]]$rows <<- xvalues
  #     
  #     
  #     
  #     # invisible(lapply(vars, function(filter){
  #     #   
  #     #   dataSet <<- dataSet[which((dataSet[[filter$col]] %in% filter$rows)), ]
  #     #   
  #     # }))
  #     
  #     vars[[filterId]]$col  <<- yvars
  #     vars[[filterId]]$rows <<- yvalues
  #     
  #     # invisible(lapply(vars, function(filter){
  #     #   
  #     #   dataSet <<- dataSet[which((dataSet[[filter$col]] %in% filter$rows)), ]
  #     #   dataSet <<- dataSet[,which((dataSet[[filter$col]] %in% filter$col))]
  #     #   
  #     # }))
  #     # 
  #     
  #     tmp <- dataSet[ , which(names(dataSet) %in% c(xvars,yvars)) ]
  #     
  #     assign(tableId, tmp)
  #     
  #     insertUI(
  #       selector = paste0('#',placeholdertableId),
  #       ui = tags$div(id = tableId,
  #                     DTOutput(tblId)
  #       )
  #     )
  #     output[[tblId]] <- renderDataTable({ 
  #       datatable(get(tableId), selection = list(target = 'cell'),
  #                 options = list(dom = 'tp'))
  #       
  #     })
  #     
  #     # aggregFilterObserver[[filterId]]$col <<- col
  #     # aggregFilterObserver[[filterId]]$rows <<- NULL
  #   })
  #   
  #   
  #   # observeEvent(input[[rowfilterId]], {
  #   #   
  #   #   rows <- input[[rowfilterId]]
  #   #   
  #   #   aggregFilterObserver[[filterId]]$rows <<- rows
  #   #   
  #   # })
  #   
  #   observeEvent(input[[removeFilterId]], {
  #     
  #     if(cur_id==as.numeric(substr(filterId,8,nchar(filterId))))
  #     {
  #       removeUI(selector = paste0('#', filterId),multiple = TRUE, immediate = TRUE)
  #       
  #       vars[[filterId]] <<- NULL   
  #       
  #       rmv_qtd <<- rmv_qtd + 1
  #       cur_id  <<- cur_id - 1
  #     }
  #     else
  #     {
  #       # If there's currently a notification, don't add another
  #       # if (!is.null(id))
  #       #   return()
  #       # Save the ID for removal later
  #       id <<- showNotification(paste("Erro: Não é possível remover tabelas de grau menor do que a última gerada."), duration = 3,type = "error")
  #       # showModal(modalDialog(
  #       #   title = "Important message",
  #       #   "Não é possível remover uma tabela de grau menor do que a ultima gerada!",
  #       #   easyClose = TRUE
  #       # ))
  #     }
  #     
  #     # if (!is.null(id))
  #     #   removeNotification(id)
  #     # id <<- NULL
  #     
  #   })
  # })
  # 
  # # output$data <- renderTable({
  # #   
  # #   dataSet <- mtcars
  # #   
  # #   invisible(lapply(aggregFilterObserver, function(filter){
  # #     
  # #     dataSet <<- dataSet[which(!(dataSet[[filter$col]] %in% filter$rows)), ]
  # #     
  # #   }))
  # #   
  # #   dataSet
  # # })
  
}



# pdf("c:/scatterplot.pdf") 
# x <- rnorm(1000)
# y <- rnorm(1000) 
# plot(test,y, main="PDF Scatterplot Example", col=rgb(0,100,0,50,maxColorValue=255), pch=16)
# dev.off()