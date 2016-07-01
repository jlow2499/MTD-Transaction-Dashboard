library(dplyr)
library(scales)
library(ggvis)

shinyServer(function(input, output) {
  
  output$table1 <- DT::renderDataTable({
    sub <- subset(TRAN,Month %in% input$Month1)
    
    Direct <- subset(sub,FundsType%in%c("RGVO","RVVO","SB"))
    AWG <- subset(sub,FundsType%in%c("RGWG","RVWG"))
    RHB <- subset(sub,FundsType%in%c("RGRH","RVRH","WORH"))
    
    Summary1 <- Direct %>%
      group_by(PostedClientCode,add=T) %>%
      summarise(DC = round(sum(TransAmt)*.152,2))
    
    Summary2 <- AWG %>%
      group_by(PostedClientCode,add=T) %>%
      summarise(AWG = round(sum(TransAmt)*.152,2))
    
    Summary3 <- RHB %>%
      group_by(PostedClientCode,add=T) %>%
      summarise(RHB = round(sum(CommAmt),2))
    
    
    
    Summarya <- full_join(Summary1,Summary2,by=c("PostedClientCode"))
    Summary <- full_join(Summarya,Summary3,by=c("PostedClientCode"))
   
   
    Summary[is.na(Summary)]<-0

    datatable(Summary,extensions = 'TableTools', options = list(
      pageLength=50,
      "sDom" = 'T<"clear">lfrtip',
      "oTableTools" = list(
        "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
        "aButtons" = list(
          "copy",
          "print",
          list("sExtends" = "collection",
               "sButtonText" = "Save",
               "aButtons" = c("csv","xls"))))))%>%
      formatCurrency(c("DC","AWG","RHB"),"$")
    
    
  
    
    })
  
  output$table2 <- DT::renderDataTable({
  
      sub <- subset(TRAN,Month %in% c(input$Month2))
      sub2 <- subset(sub,PostedClientCode %in% c(input$Office1))
      sub2$EffDt <- as.character(sub2$EffDt)
      
      Direct <- subset(sub2,FundsType%in%c("RGVO","RVVO","SB"))
      AWG <- subset(sub2,FundsType%in%c("RGWG","RVWG"))
      RHB <- subset(sub2,FundsType%in%c("RGRH","RVRH","WORH"))
      
      Daily_Summary1 <- Direct %>%
        group_by(EffDt,PostTms,add=T) %>%
        summarise(DC = round(sum(TransAmt)*.152,2))
      
      Daily_Summary2 <- AWG %>%
        group_by(EffDt,PostTms,add=T) %>%
        summarise(AWG = round(sum(TransAmt)*.152,2))
      
      Daily_Summary3 <- RHB %>%
        group_by(EffDt,PostTms,add=T) %>%
        summarise(RHB = round(sum(CommAmt),2))
      
     
      library(plyr)
      
      Daily_Summarya <- join(Daily_Summary1,Daily_Summary2,by=c("PostTms","EffDt"),type="full")
      Daily_Summary <- join(Daily_Summarya,Daily_Summary3,by=c("PostTms","EffDt"),type="full")
      detach("package:plyr", unload=TRUE)
      

      Daily_Summary[is.na(Daily_Summary)]<-0

      
      datatable(Daily_Summary,extensions = 'TableTools', rownames=FALSE,class = 'cell-border stripe',filter="top",
                options = list(
                  searching=TRUE,
                  autoWidth=TRUE,
                  paging=FALSE,
                  
                  "sDom" = 'T<"clear">lfrtip',
                  "oTableTools" = list(
                    "sSwfPath" = "//cdnjs.cloudflare.com/ajax/libs/datatables-tabletools/2.1.5/swf/copy_csv_xls.swf",
                    "aButtons" = list(
                      "copy",
                      "print",
                      list("sExtends" = "collection",
                           "sButtonText" = "Save",
                           "aButtons" = c("csv","xls"))))))%>%
        formatCurrency(c("DC","AWG","RHB"),"$")
      
      
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(TRAN, file)
    }
  )
  
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) 
        counter <- -1
      else
        load(file="counter.Rdata")
      counter  <- counter + 1
      save(counter, file="counter.Rdata")     
      paste("You are visitor# ", counter)
    })
  
  bar_info <- function(x) {
    if(is.null(x)) return(NULL) 
    
    paste(
      names(x)[1],
      x[1,1],
      names(x)[2],
      x[1,2],
      names(x)[3],
      x[1,3],
      names(x)[4],
      x[1,4],
      sep=" : ",
      collapse="<br />")
  }

  plot1 <-reactive({
    googly <-subset(TRAN,PostedClientCode%in%c(input$officina))
    Direct <- subset(googly,FundsType%in%c("RGVO","RVVO","SB"))
    AWG <- subset(googly,FundsType%in%c("RGWG","RVWG"))
    RHB <- subset(googly,FundsType%in%c("RGRH","RVRH","WORH"))
    
    Summary1 <- Direct %>%
      group_by(Month,add=T) %>%
      summarise(DC = round(sum(TransAmt)*.152,2))
    
    Summary2 <- AWG %>%
      group_by(Month,add=T) %>%
      summarise(AWG = round(sum(TransAmt)*.152,2))
    
    Summary3 <- RHB %>%
      group_by(Month,add=T) %>%
      summarise(RHB = round(sum(CommAmt),2))
    
    detach("package:dplyr", unload=TRUE)
    library(plyr)
    
    Summarya <- join(Summary1,Summary2,by=c("Month"))
    Summary <- join(Summarya,Summary3,by=c("Month"))
    row.names(Summary) <- NULL
    
    detach("package:plyr", unload=TRUE)
    library(dplyr)
    
    Summary[is.na(Summary)]<-0
    
    library(reshape2)
    melt <- melt(Summary,measure.vars=c("DC","AWG","RHB"))
    melt
})

    plot1 %>% ggvis(~Month,~value) %>%
      filter(variable == eval(input_select(choices=c('DC','AWG','RHB')))) %>%
      layer_bars(fill="") %>%
      layer_text(text := ~dollar(value),fontSize :=16,baseline:="top") %>%
      scale_ordinal("font", range = c("Helvetica Neue", "Times New Roman"))%>%
      add_axis("y", title = "",orient="left") %>%
      add_axis("y",title="",orient="right") %>%
      add_tooltip(bar_info,"hover") %>%
      bind_shiny("theplot",controls_id="plotcontrol")

  })
  
