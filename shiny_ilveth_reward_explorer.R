#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

dataset <- diamonds


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("ILV/ETH Staking Explorer Alpha Version"),
    
    p("Disclaimer: This is - of course - no financial advice. I can not guarantee that the calculations are correct. I'm just a dude who wanted to create a shiny app. This is not an official tool created by the Illuvium team! Also I don't know yet if I'll have time to implement feedback (Baby incoming)."),
    p("With all this out of the way I hope that a) the calculations are correct, b) the tool functions as intended and c) the tool helps people to explore how the frequency of claiming (and thereby restaking) rewards from the ILV/ETH staking pool and gas prices influence their overall rewards."),
    p("The lines for staking rewards are flattening because the APY will decrease by 3% every two weeks. APY will probably decrease even more as more people stake which is NOT included here. Further ILV price and gas prices are not static in real-life. Keep that in mind ;)."),
    p("Into the ILV amount in LP input field please put in the amount of ILV that you put into the ILV/ETH pool. It will be multiplied by 2 automatically."),
    p("Days until withdrawal of SLP is basically just the length of the x-axis."),

    title = "Staking Explorer",
    
    fluidRow(
        column(4,
               numericInput("stake","ILV amount in LP",value=2),
               numericInput("ilvprice", "ILV price in USD",value=390)
        ),
        column(4, 
               numericInput("slpapy","APY% of ILV/ETH pool",value=600),
               numericInput("ilvapy", "APY% of ILV pool",value=100)
        ),
        column(4,
               numericInput("claimdays","Days until withdrawal of SLP",value=365),
               numericInput("gas", "Gas price in USD for reward claiming",value=30),
               numericInput("rewaclaim", "Days between each reward claim",value=30)
        )
    ),
    hr(),
    plotOutput('plot'),
    hr(),
    p("Further reading:", br(), "https://medium.com/illuvium/9-tokenomics-launchpad-and-reward-details-5894c3b356be" ,br() ,"https://medium.com/illuvium/28-everything-you-need-to-know-about-staking-ilv-6669594b2fac", br(), "https://medium.com/illuvium/29-governance-yield-farming-and-vault-distribution-faq-7327ae7eb507")
    #textOutput("test")
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #x <- reactive({paste(input$stake,input$ilvprice)})
    #y <- reactive({input$ilvprice})
    #z <- x+y
    #df <- data.frame(x=x,y=y)
    output$plot <- renderPlot({
    #x = input$stake
    #y = input$ilvprice
    #z = data.frame(x=x, y=y)
        
        # Initial data
        claimdays <- input$claimdays
        slpapy <- input$slpapy
        ilvapy <- input$ilvapy
        ilvprice <- input$ilvprice
        gas <- input$gas
        stake <- input$stake * 2
        rewaclaim <- input$rewaclaim
        
        
        # Dataframe setup and APY-decrease calculation
        rewadf <- tibble(day = 0:claimdays, slprewards = as.numeric(0), ilvrewards = as.numeric(0), slpstake = stake, ilvstake = 0,claim=0,slprewa_acc = 0,gas = 0, gas_accu = 0)
        rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>% 
            mutate(slpapy_day = (slpapy / (100*365))*0.97^(week-1)) %>% 
            mutate(slpapy_day = case_when(week %% 2 == 0 ~ lag(slpapy_day,n=7),
                                          week %% 2 == 1 ~ slpapy_day)) %>% 
            mutate(ilvapy_day = (ilvapy / (100*365))*0.97^(week-1)) %>% 
            mutate(ilvapy_day = case_when(week %% 2 == 0 ~ lag(ilvapy_day,n=7),
                                          week %% 2 == 1 ~ ilvapy_day))
        
        # Simulation
        rewadf_w <- rewadf %>% mutate(claim = ifelse(0:claimdays %% rewaclaim == 0, 1,0))
        
        #rewadf_w[1,"slprewards"] <- rewadf_w[1,"slpstake"]* rewadf_w[1,"slpapy_day"]
        rewadf_w[2,"slprewards"] <- rewadf_w[2,"slpstake"]* rewadf_w[2,"slpapy_day"]
        rewadf_w[2,"slprewa_acc"] <- rewadf_w[2,"slpstake"]* rewadf_w[2,"slpapy_day"]
        
        for(i in 3:nrow(rewadf_w))
        {
            # SLP rewards
            rewadf_w[i,"slprewards"] <- rewadf_w[i-1,"slprewards"] + rewadf_w[i,"slpstake"]* rewadf_w[i,"slpapy_day"]
            rewadf_w[i,"slprewa_acc"] <- rewadf_w[i-1,"slprewa_acc"] + rewadf_w[i,"slpstake"]* rewadf_w[i,"slpapy_day"]
            
            # ILV rewards
            rewadf_w[i,"ilvrewards"] <- rewadf_w[i-1,"ilvrewards"] + rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"]
            
            # Claim?
            if(rewadf_w[i,"claim"]==1){ 
                rewadf_w[i:nrow(rewadf_w),"ilvstake"] <- rewadf_w[i-1,"ilvstake"] + rewadf_w[i,"slprewards"]
                rewadf_w[i,"slprewards"] <- 0
                rewadf_w[i,"gas"] <- -50
            }
            
            # Gas accu
            rewadf_w[i, "gas_accu"] <- rewadf_w[i-1, "gas_accu"] + rewadf_w[i-1, "gas"]
            
        } 
        
        
        
        rewadf_w %>% ggplot(aes(x = day, y = slprewa_acc*ilvprice,color="#377eb8",size=2)) +
            geom_path() +
            geom_path(aes(y = ilvrewards*ilvprice,colour="#984ea3",size=2)) +
            geom_path(aes(y = ilvrewards*ilvprice + slprewa_acc*ilvprice,colour="#ff7f00",size=2)) +
            geom_step(aes(y = gas_accu,colour="#e41a1c",size=2)) +
            geom_step(aes(y=slprewa_acc*ilvprice+ilvrewards*ilvprice+gas_accu,color="#4daf4a",size=2)) +
            #geom_segment(x=0, y=stake*ilvprice, xend=claimdays, yend= stake*ilvprice,linetype="dashed",size=1,color="black") +
            geom_hline(yintercept=0,size=1,color="black") +
            xlab("Days") + ylab("Yield [$]")  + theme_classic() +
            theme(legend.position="bottom",
                  legend.text = element_text(size=16)) +
            scale_x_continuous(expand = c(0, 0)) +
            scale_color_identity(guide = "legend", name="", breaks=c("#ff7f00", "#377eb8", "#4daf4a", "#984ea3", "#e41a1c"),
                                 labels = c("Rewards from both pools","SLP pool rewards","Rewards - Gas fees", "ILV pool rewards", "Gas fees")) +
            guides(color = guide_legend(override.aes = list(size=5)))+
            scale_size_identity(guide="none") +
            annotate(geom="segment",xend=0,x=claimdays/100, y=stake*ilvprice,yend=stake*ilvprice,size=1,arrow=arrow(length = unit(claimdays/200,"mm"))) +
            annotate(geom="text",x=claimdays/100, y=stake*ilvprice, label = "Initial investment",hjust=0) +
            scale_y_continuous(breaks = c(pretty(c(rewadf_w$slprewa_acc*ilvprice,rewadf_w$gas_accu)), stake*ilvprice), labels = c(pretty(c(rewadf_w$slprewa_acc*ilvprice,rewadf_w$gas_accu)), stake*ilvprice))+
            geom_label(show.legend=F, aes(x=tail(rewadf_w,1)$day, y=tail(rewadf_w,1)$slprewa_acc*ilvprice+tail(rewadf_w,1)$ilvrewards*ilvprice+tail(rewadf_w,1)$gas_accu),label=round(tail(rewadf_w,1)$slprewa_acc*ilvprice+tail(rewadf_w,1)$ilvrewards*ilvprice+tail(rewadf_w,1)$gas_accu),color="#4daf4a", hjust=1, vjust=0,size=3)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
