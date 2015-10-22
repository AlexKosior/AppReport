setwd(dir = "~/Desktop/")

##function bydevicesessions renders tables for product report
## you can CHANGE the number of weeks shown in the final table when calling
## the function in the console "bydevicesessions(X)" where X is the number
## of weeks you want to show, the default value is 5, which means it will
## show 6 weeks - 5 prior and the current one
bydevicesessions<-function(week=5){

    ##LIBRARIES FOR SHINY

    library("shiny")
    ## CURRENT WEEK - counts how many weeks are shown in final tables

    currentweek<-function(x=Sys.Date()){
        cw<-as.numeric(strftime(x,format="%W"))
        aw<-cw-week
        print(aw)
    }

    ## SUBSETTING AND PREPARING FOR SHINY - DATA IS READ FROM CSV (in this case),
    ## then we have changeable part - year
    ## needs to be changed every year
    dat<-read.csv("dev.csv", header=T)
    dat1<-dat[which(dat$year=='2015' & dat$week>currentweek()),]
    dat2<-dat1[c(-2,-4)]

    ##reshape2 LIBRARY FOR CREATING PIVOT TABLES
    library(reshape2)
    ##MAIN DATASET - we melt the dataset aql by devicecategory, region and week
    ## then we create pivot tables with dcast function -> for everything and for
    ##every region respectively HERE YOU CAN ADD NEW REGION IF NECCESSARY
    ##TOTAL
    aql<-melt(dat2, id.vars=c("devicecategory","region_jovago","week"))
    aqw<-dcast(aql, devicecategory~week,sum,value.var = "value")
    ##East Africa
    ea<-aql[which(aql$region_jovago=="East Africa"),]
    regionea<-dcast(ea, devicecategory~week,sum,value.var = "value")
    ##Nigeria
    nig<-aql[which(aql$region_jovago=="Nigeria"),]
    regionnig<-dcast(nig, devicecategory~week,sum,value.var = "value")
    ##West Africa
    we<-aql[which(aql$region_jovago=="West Africa"),]
    regionwe<-dcast(we, devicecategory~week,sum,value.var = "value")
    ##Pakistan
    pak<-aql[which(aql$region_jovago=="Pakistan"),]
    regionpak<-dcast(pak, devicecategory~week,sum,value.var = "value")
    ##other - meaning black in database
    o<-aql[which(aql$region_jovago=="(blank)"),]
    regiono<-dcast(o, devicecategory~week,sum,value.var = "value")

    ##DONOT TOUCH - SCRIPT FOR CONDITIONAL COLORING


    script <- "
var cols_in_use = [3,4,5,6,7];

for (i = 0; i < cols_in_use.length; i++) {

  var str = 'tbody tr td:nth-child('+ cols_in_use[i] + ')';
  var sumValue = 0;

  $(str).each(function() {
    var cellValue = Number($(this).text());
    sumValue = sumValue + Number(cellValue);
    });

  console.log(sumValue);

  $(str).each(function() {
    var cellValue = Number($(this).text()
    );

    console.log(cellValue/sumValue);

    if (cellValue/sumValue < 0.2) {

      $(this).css('background-color', '#FF0000');
    }
    
    else if (cellValue/sumValue >= 0.1) {

      $(this).css('background-color', '#00CC00');
    }

  });
}
"



    runApp(list(server = function(input, output, session) {
        session$onFlushed(function() {
            session$sendCustomMessage(type='jsCode', list(value = script))
        }, once = FALSE)

        ## TABLE RENDERING - if you add region add one more
        ## output$viewX <-renderTable({print(XYZ)})
        ## where X is a view number (1,...,n) and XYZ is dataset's name
        ## see MAIN DATASET (above)
        output$view <- renderTable({
            aqw
        })
        output$Test1 <- renderUI({
            list(
                tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode", function(message) { eval(message.value); });')))
                , tableOutput("view")
            )
        })
        output$view1 <- renderTable({
            print(regionea)
        })
        output$view2 <- renderTable({
            print(regionnig)
        })
        output$view3 <- renderTable({
            print(regionwe)
        })
        output$view4 <- renderTable({
            print(regionpak)
        })
        output$view5 <- renderTable({
            print(regiono)
        })
    }
    ##here is USER INTERFACE, if adding new table, you need to write:
    ## tabPanel ("ABC",tabelOutput("viewX"))
    ##where ABC is the tab name and viewX is the table name
    ## see TABLE RENDERING (above)
    , ui = fluidPage(

        titlePanel("byDevice"),
        mainPanel(
            tabsetPanel(
                tabPanel("total", uiOutput("Test1")),
                tabPanel("East Africa",tableOutput("view1")),
                tabPanel("Nigeria",tableOutput("view2")),
                tabPanel("West Africa",tableOutput("view3")),
                tabPanel("Pakistan",tableOutput("view4")),
                tabPanel("other",tableOutput("view5"))
            )
        )
    ))
    )}

bydevicesessions()
