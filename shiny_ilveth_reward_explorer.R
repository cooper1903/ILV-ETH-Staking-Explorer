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



# Define UI for application that draws a histogram
staketypeui <- tabsetPanel(
  id = "stake_ui",
  type = "hidden",
  tabPanel("ILV staking only",
           column(4,
                    numericInput("ILVstake","ILV amount",value=5),
                    numericInput("ilvprice", "ILV price in USD",value=500)),
           column(4,
                    numericInput("ilvapy", "APY% of staked ILV",value=50),
                    numericInput("ilvrewapy", "APY% for ILV pool at token weight 2",value=90)),
           
           column(4,
                    numericInput("claimdays","Days until withdrawal of ILV",value=365),
                    numericInput("gas", "Gas price in USD for reward claiming",value=30),
                    numericInput("rewaclaim", "Days between each reward claim",value=30))
             ),
  tabPanel("SLP staking only",
         column(4,
                    numericInput("stake2","ILV amount in SLP",value=2),
                    numericInput("ilvprice2", "ILV price in USD",value=500)),
         column(4,
                    numericInput("slpapy2","APY% of staked ILV/ETH pool",value=720),
                    #numericInput("ilvapy", "APY% of staked ILV pool",value=100),
                    numericInput("ilvrewapy2", "APY% for ILV pool at token weight 2",value=90)),
         
         column(4,
                    numericInput("claimdays2","Days until withdrawal of SLP",value=365),
                    numericInput("gas2", "Gas price in USD for reward claiming",value=30),
                    numericInput("rewaclaim2", "Days between each reward claim",value=30))
             ),
  tabPanel("ILV + SLP staking",
             column(4,
                    numericInput("stake3","ILV amount in SLP",value=2),
                    numericInput("ILVstake3", "ILV amount", value=5),
                    numericInput("ilvprice3", "ILV price in USD",value=500)
                    
             ),
             column(4,
                    numericInput("slpapy3","APY% of staked ILV/ETH pool",value=720),
                    numericInput("ilvapy3", "APY% of staked ILV pool",value=50),
                    numericInput("ilvrewapy3", "APY% for ILV pool at token weight 2",value=90)),
             column(4,
                    numericInput("claimdays3","Days until withdrawal of ILV",value=365),
                    numericInput("gas3", "Gas price in USD for reward claiming",value=30),
                    numericInput("rewaclaim3", "Days between each reward claim",value=30)
             ))
)

ui <- fluidPage(
    
    titlePanel("ILV/ETH Staking Explorer Alpha Version2"),
    
    h3("Disclaimer:"),
    p("This is - of course - no financial advice. I can not guarantee that the calculations are correct. I'm just a dude who wanted to create a shiny app. This is not an official tool created by the Illuvium team! "),
    p("Please find some info on how to use this tool and some limitations underneath the graphic."),
    hr(),
    
    title = "Staking Explorer",
    selectInput("initialinput","In which staking pool are your ILV?",
                choices = c("ILV staking only", "SLP staking only", "ILV + SLP staking")),#, "ILV + SLP staking")),

    
    hr(),
    staketypeui,
        
    # fluidRow(
    #     column(4,
    #            numericInput("stake","ILV amount in LP",value=2),
    #            numericInput("ilvprice", "ILV price in USD",value=390)
    #     ),
    #     column(4, 
    #            numericInput("slpapy","APY% of ILV/ETH pool",value=600),
    #            numericInput("ilvapy", "APY% of ILV pool",value=100)
    #     ),
    #     column(4,
    #            numericInput("claimdays","Days until withdrawal of SLP",value=365),
    #            numericInput("gas", "Gas price in USD for reward claiming",value=30),
    #            numericInput("rewaclaim", "Days between each reward claim",value=30)
    #     )
    # ),
    hr(),
    plotOutput('plot',height=600),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    br(),
    hr(),
    p(strong("Limitations"), br(), "Two of the main limitations of this simulation are that the APY% is static and not a function of the total value locked in the pools. In real life your rewards decrease if your share in the pool decreases. Further the ILV price is static in this simulation, which will of course fluctuate in real life. Please keep this in mind. I can't stress enough that this tool is only able to give you a very rough guesstimate how gas fees and claiming frequency influence your overall yield!"),
    p(strong("Explanations:")),
    p("The lines for staking rewards are flattening because the APY will decrease by 3% every two weeks. APY will probably decrease even more as more people stake"),
    p("Days until withdrawal of SLP is basically just the length of the x-axis."),
    p("ILV amount in LP is the amount of ILV that you put into the ILV/ETH pool. It will be multiplied by 2 automatically. So if you paired 5 ILV with 0.5 ETH put in 5."),
    p("Days until withdrawal of SLP is basically just the length of the x-axis."),
    p("APY% of staked ILV/ETH pool and APY% of staked ILV pool are the APYs for the staked ILV. This APY depends on the token weight, which should be used for the calculations. So right now for flexible staking in the ILV pool its 45%´, for 6 month locked its 68%"),
    p("APY% for ILV pool at token weight 2 is used to calculate the compounding interest of claimed ILV that is locked for 1 year. As it is locked it always has a token weight of 2"),
    p("Initial investment is calculated as: ILV amount * ILV price + ILV amount in SLP * 2 * ILV price"),
   
    p(strong("Source code:"),br(), "https://github.com/cooper1903/ILV-ETH-Staking-Explorer/blob/main/shiny_ilveth_reward_explorer.R"),
    p(strong("Further reading on the ILV tokenomics:"), br(), "https://medium.com/illuvium/9-tokenomics-launchpad-and-reward-details-5894c3b356be" ,br() ,"https://medium.com/illuvium/28-everything-you-need-to-know-about-staking-ilv-6669594b2fac", br(), "https://medium.com/illuvium/29-governance-yield-farming-and-vault-distribution-faq-7327ae7eb507"),
    p(strong("New in Version2:"),br(), "One can now provide a separate APY% for staked ILV and claimed ILV (as token weight and therefore APY of claimed ILV is always two, but can vary for staked ILV)"),
    p("It is now possible to select between ILV staking, ILV/ETH staking and ILV/ETH + ILV staking"),
    p("Graphic looks a bit nicer"),
    p("More Explanations and shortcomings"),
    hr(),
    p("If you have questions or feedback (remember this is my first shiny app) you can find me on discord: cooper#6432" )
    
    
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$initialinput, {
    updateTabsetPanel(inputId =  "stake_ui", selected = input$initialinput, session)
  })
  
    output$plot <- renderPlot({
      
      switch(input$initialinput,
             "ILV staking only" = {
               
               claimdays <- input$claimdays
               #slpapy <- input$slpapy
               ilvapy <- input$ilvapy
               ilvprice <- input$ilvprice
               gas <- input$gas
               ilvstake <- input$ILVstake
               rewaclaim <- input$rewaclaim
               ilvrewaapy<-input$ilvrewapy
               
               
               # Dataframe setup and APY-decrease calculation
               rewadf <- tibble(day = 0:claimdays, ilvrewards = as.numeric(0), ilvstake = ilvstake,claim=0, gas = 0, gas_accu = 0,ilvrewa_acc=0,ilvrewa_claimed=0,ilvclaimcomp=0,ilvsingle=0)
               # APY dilution of ilvapy (staked token weight)
               rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
                 mutate(ilvapy_day = (ilvapy / (100*365))*0.97^(week-1)) %>%
                 mutate(ilvapy_day = case_when(week %% 2 == 0 ~ lag(ilvapy_day,n=7),
                                               week %% 2 == 1 ~ ilvapy_day))
               # APY dilution of ilvrewaapy (token weight of 2 because of 1 year locked in vesting)
               rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
                 mutate(ilvrewaapy_day = (ilvrewaapy / (100*365))*0.97^(week-1)) %>%
                 mutate(ilvrewaapy_day = case_when(week %% 2 == 0 ~ lag(ilvrewaapy_day,n=7),
                                                   week %% 2 == 1 ~ ilvrewaapy_day))
               
               # Simulation
               rewadf_w <- rewadf %>% mutate(claim = ifelse(0:claimdays %% rewaclaim == 0, 1,0))
               
               #rewadf_w[1,"slprewards"] <- rewadf_w[1,"slpstake"]* rewadf_w[1,"slpapy_day"]
               rewadf_w[2,"ilvrewards"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
               rewadf_w[2,"ilvrewa_acc"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
               rewadf_w[2,"ilvsingle"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
               for(i in 3:nrow(rewadf_w))
               {
                 # # SLP rewards
                 # rewadf_w[i,"slprewards"] <- rewadf_w[i-1,"slprewards"] + rewadf_w[i,"slpstake"]* rewadf_w[i,"slpapy_day"]
                 # rewadf_w[i,"slprewa_acc"] <- rewadf_w[i-1,"slprewa_acc"] + rewadf_w[i,"slpstake"]* rewadf_w[i,"slpapy_day"]
                 
                 # ILV rewards
                 # daily rewards from normal stake and claimed-stake
                 rewadf_w[i,"ilvrewards"] <- rewadf_w[i-1,"ilvrewards"] + rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
                 # accumulated ilvrewards
                 rewadf_w[i,"ilvrewa_acc"] <- rewadf_w[i-1,"ilvrewa_acc"] + rewadf_w[i,"ilvrewards"] - rewadf_w[i-1,"ilvrewards"] 
                 # accumulated rewards from ILVs that were claimed
                 rewadf_w[i, "ilvclaimcomp"] <-rewadf_w[i-1, "ilvclaimcomp"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
                 # accumulated rewards from ILV staking
                 rewadf_w[i, "ilvsingle"] <- rewadf_w[i-1, "ilvsingle"] +rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"]
                 
                 
                 # ilv_rewa_claimed
                 rewadf_w[i, "ilvrewa_claimed"] <- rewadf_w[i-1,"ilvrewa_claimed"]
                 
                 # Claim?
                 if(rewadf_w[i,"claim"]==1){
                   rewadf_w[i:nrow(rewadf_w),"ilvrewa_claimed"] <-rewadf_w[i, "ilvrewa_claimed"] + rewadf_w[i-1,"ilvrewards"] 
                   rewadf_w[i,"ilvrewards"] <- rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
                   rewadf_w[i,"gas"] <- -gas
                 }
                 
                 # Gas accu
                 rewadf_w[i, "gas_accu"] <- rewadf_w[i-1, "gas_accu"] + rewadf_w[i, "gas"]
               }
               
             
               
               rewadf_w %>% ggplot(aes(x = day, y =ilvstake*ilvprice+ ilvrewa_acc*ilvprice,color="#fc8d59",size=2)) +
                 geom_path() +
                 geom_path(aes(y = ilvstake*ilvprice + ilvclaimcomp*ilvprice,colour="#99d594",size=2)) +
                 geom_path(aes(y = ilvstake*ilvprice + ilvsingle*ilvprice,colour="#e6f598",size=2)) +
                 geom_step(aes(y = ilvstake*ilvprice + gas_accu,colour="#d53e4f",size=2)) +
                 geom_step(aes(y=ilvstake*ilvprice + ilvrewa_acc*ilvprice+gas_accu,color="#3288bd",size=2)) +
                 #geom_segment(x=0, y=stake*ilvprice, xend=claimdays, yend= stake*ilvprice,linetype="dashed",size=1,color="black") +
                 geom_hline(size=1,aes(yintercept=ilvstake*ilvprice,color="black")) +
                 xlab("Days") + ylab("Yield [$]")  + theme_classic() +
                 theme(legend.position="bottom",
                       legend.text = element_text(size=16, face="bold"),
                       axis.text = element_text(size=16, face="bold"),
                       axis.title = element_text(size=16, face="bold"))+#,
                 #axis.text.y = element_text(size=16, face="bold")) +
                 scale_x_continuous(expand = c(0, 0)) +
                 scale_color_identity(guide = "legend", name="", breaks=c("#fc8d59", "#99d594", "#e6f598", "#d53e4f", "#3288bd", "black"),
                                      labels = c("Total rewards","Claimed ILV rewards","ILV pool rewards", "Gas fees", "Total rewards - Gas fees", "Initial investment [$]")) +
                 guides(color = guide_legend(override.aes = list(size=5)))+
                 scale_size_identity(guide="none") #+
               },
             
             
             
             "SLP staking only" = {
               # Initial data
               claimdays <- input$claimdays2
               slpapy <- input$slpapy2
               #ilvapy <- 50
               ilvprice <- input$ilvprice2
               gas <- input$gas2
               slpstake <- input$stake2
               rewaclaim <- rewaclaim <- input$rewaclaim2
               ilvrewaapy<-input$ilvrewapy2
               
               
               # Dataframe setup and APY-decrease calculation
               rewadf <- tibble(day = 0:claimdays, claim=0, slp_stake = slpstake*2, slprewards = 0,  slprewa_acc = 0, gas = 0, gas_accu = 0, ilvclaimed = 0, ilvclaimrewards=0, ilvclaimrewards_acc=0)
               # APY dilution of ilvapy (staked token weight)
               rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
                 mutate(slpapy_day = (slpapy / (100*365))*0.97^(week-1)) %>%
                 mutate(slpapy_day = case_when(week %% 2 == 0 ~ lag(slpapy_day,n=7),
                                               week %% 2 == 1 ~ slpapy_day))
               # APY dilution of ilvrewaapy (token weight of 2 because of 1 year locked in vesting)
               rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
                 mutate(ilvrewaapy_day = (ilvrewaapy / (100*365))*0.97^(week-1)) %>%
                 mutate(ilvrewaapy_day = case_when(week %% 2 == 0 ~ lag(ilvrewaapy_day,n=7),
                                                   week %% 2 == 1 ~ ilvrewaapy_day))
               
               # Simulation
               rewadf_w <- rewadf %>% mutate(claim = ifelse(0:claimdays %% rewaclaim == 0, 1,0))
               
               #rewadf_w[1,"slprewards"] <- rewadf_w[1,"slpstake"]* rewadf_w[1,"slpapy_day"]
               rewadf_w[2,"slprewards"] <- rewadf_w[2,"slp_stake"]* rewadf_w[2,"slpapy_day"]
               rewadf_w[2,"slprewa_acc"] <- rewadf_w[2,"slp_stake"]* rewadf_w[2,"slpapy_day"]
               #rewadf_w[2,"ilvsingle"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
               for(i in 3:nrow(rewadf_w))
               {
                 # # SLP rewards
                 rewadf_w[i,"slprewards"] <- rewadf_w[i-1,"slprewards"] + rewadf_w[i,"slp_stake"]* rewadf_w[i,"slpapy_day"]
                 rewadf_w[i,"slprewa_acc"] <- rewadf_w[i-1,"slprewa_acc"] + rewadf_w[i,"slprewards"] - rewadf_w[i-1,"slprewards"]
                 
                 # ILV rewards
                 # ilv_rewa_claimed
                 rewadf_w[i, "ilvclaimed"] <- rewadf_w[i-1,"ilvclaimed"]
                 # daily rewards from claimed-stake
                 rewadf_w[i,"ilvclaimrewards"] <- rewadf_w[i-1,"ilvclaimrewards"] + rewadf_w[i, "ilvclaimed"] * rewadf_w[i,"ilvrewaapy_day"] #rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
                 
                 # accumulated rewards from ILVs that were claimed
                 #rewadf_w[i, "ilvclaimcomp"] <-rewadf_w[i-1, "ilvclaimcomp"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
                 # accumulated rewards from ILV staking
                 #rewadf_w[i, "ilvsingle"] <- rewadf_w[i-1, "ilvsingle"] +rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"]
                 
                 
                 
                 
                 # Claim?
                 if(rewadf_w[i,"claim"]==1){
                   #rewadf_w[i:nrow(rewadf_w),"ilvrewa_claimed"] <-rewadf_w[i, "ilvrewa_claimed"] + rewadf_w[i-1,"slprewards"]     
                   rewadf_w[i:nrow(rewadf_w),"ilvclaimed"] <-rewadf_w[i, "ilvclaimed"] +rewadf_w[i-1,"ilvclaimrewards"]+ rewadf_w[i-1,"slprewards"] 
                   
                   rewadf_w[i,"ilvclaimrewards"] <-  rewadf_w[i, "ilvclaimed"] * rewadf_w[i,"ilvrewaapy_day"]
                   rewadf_w[i,"slprewards"] <- rewadf_w[i,"slp_stake"]* rewadf_w[i,"slpapy_day"]
                   rewadf_w[i,"gas"] <- -gas
                 }
                 # accumulated ilvrewards
                 rewadf_w[i,"ilvclaimrewards_acc"] <- rewadf_w[i-1,"ilvclaimrewards_acc"] + rewadf_w[i, "ilvclaimed"] * rewadf_w[i,"ilvrewaapy_day"]
                 
                 # Gas accu
                 rewadf_w[i, "gas_accu"] <- rewadf_w[i-1, "gas_accu"] + rewadf_w[i, "gas"]
               }
               
               
               
               rewadf_w %>% ggplot(aes(x = day, y = slp_stake*ilvprice+ slprewa_acc*ilvprice+ ilvclaimrewards_acc*ilvprice,color="#fc8d59",size=2)) +
                 geom_path() +
                 geom_path(aes(y = slp_stake*ilvprice + slprewa_acc*ilvprice,colour="#fee08b",size=2)) + # just SLP
                 geom_path(aes(y = slp_stake*ilvprice + ilvclaimrewards_acc*ilvprice,colour="#99d594",size=2)) +
                 geom_step(aes(y = slp_stake*ilvprice + gas_accu,colour="#d53e4f",size=2)) +
                 geom_step(aes(y=slp_stake*ilvprice + slprewa_acc*ilvprice+ ilvclaimrewards_acc*ilvprice+gas_accu,color="#3288bd",size=2)) +
                 #geom_segment(x=0, y=stake*ilvprice, xend=claimdays, yend= stake*ilvprice,linetype="dashed",size=1,color="black") +
                 geom_hline(size=1,aes(yintercept=slp_stake*ilvprice,color="black")) +
                 xlab("Days") + ylab("Yield [$]")  + theme_classic() +
                 theme(legend.position="bottom",
                       legend.text = element_text(size=16, face="bold"),
                       axis.text = element_text(size=16, face="bold"),
                       axis.title = element_text(size=16, face="bold"))+#,
                 #axis.text.y = element_text(size=16, face="bold")) +
                 scale_x_continuous(expand = c(0, 0)) +
                 scale_color_identity(guide = "legend", name="", breaks=c("#fc8d59", "#fee08b", "#99d594", "#d53e4f", "#3288bd", "black"),
                                      labels = c("Total rewards","SLP pool rewards","Claimed ILV rewards", "Gas fees", "Total rewards - Gas fees", "Initial investment [$]")) +
                 guides(color = guide_legend(override.aes = list(size=5)))+
                 scale_size_identity(guide="none") #+
               
             },
             
             
             
             "ILV + SLP staking" = 
               {

    claimdays <- input$claimdays3
    slpapy <- input$slpapy3
    ilvapy <- input$ilvapy3
    ilvprice <- input$ilvprice3
    gas <- input$gas3
    slpstake <- input$stake3
    ilvstake <- input$ILVstake3
    rewaclaim <- input$rewaclaim3
    ilvrewaapy<-input$ilvrewapy3
    
    
    # Dataframe setup and APY-decrease calculation
    rewadf <- tibble(day = 0:claimdays, claim=0, slp_stake = slpstake*2, slprewards = 0,  slprewa_acc = 0, gas = 0, gas_accu = 0, ilvstake = ilvstake, ilvreward=0, ilvrewa_acc=0 ,ilvclaimed = 0, ilvclaimrewards=0, ilvclaimrewards_acc=0)
    # APY dilution of ilvapy (staked token weight)
    rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
      mutate(slpapy_day = (slpapy / (100*365))*0.97^(week-1)) %>%
      mutate(slpapy_day = case_when(week %% 2 == 0 ~ lag(slpapy_day,n=7),
                                    week %% 2 == 1 ~ slpapy_day))
    # APY dilution of ilvrewaapy (token weight of 2 because of 1 year locked in vesting)
    rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
      mutate(ilvrewaapy_day = (ilvrewaapy / (100*365))*0.97^(week-1)) %>%
      mutate(ilvrewaapy_day = case_when(week %% 2 == 0 ~ lag(ilvrewaapy_day,n=7),
                                        week %% 2 == 1 ~ ilvrewaapy_day))
    # APY dilution of ilv
    rewadf <- rewadf %>% mutate(week = rewadf$day %/% 7 + 1) %>%
      mutate(ilvapy_day = (ilvapy / (100*365))*0.97^(week-1)) %>%
      mutate(ilvapy_day = case_when(week %% 2 == 0 ~ lag(ilvapy_day,n=7),
                                    week %% 2 == 1 ~ ilvapy_day))
    
    # Simulation
    rewadf_w <- rewadf %>% mutate(claim = ifelse(0:claimdays %% rewaclaim == 0, 1,0))
    
    #rewadf_w[1,"slprewards"] <- rewadf_w[1,"slpstake"]* rewadf_w[1,"slpapy_day"]
    rewadf_w[2,"slprewards"] <- rewadf_w[2,"slp_stake"]* rewadf_w[2,"slpapy_day"]
    rewadf_w[2,"slprewa_acc"] <- rewadf_w[2,"slp_stake"]* rewadf_w[2,"slpapy_day"]
    rewadf_w[2,"ilvreward"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
    rewadf_w[2,"ilvrewa_acc"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
    #rewadf_w[2,"ilvsingle"] <- rewadf_w[2,"ilvstake"]* rewadf_w[2,"ilvapy_day"]
    for(i in 3:nrow(rewadf_w))
    {
      # # SLP rewards
      rewadf_w[i,"slprewards"] <- rewadf_w[i-1,"slprewards"] + rewadf_w[i,"slp_stake"]* rewadf_w[i,"slpapy_day"]
      rewadf_w[i,"slprewa_acc"] <- rewadf_w[i-1,"slprewa_acc"] + rewadf_w[i,"slprewards"] - rewadf_w[i-1,"slprewards"]
      
      # ILV-pool rewards
      # ILV rewards
      # daily rewards from normal stake and claimed-stake
      rewadf_w[i,"ilvreward"] <- rewadf_w[i-1,"ilvreward"] + rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"] 
      # accumulated ilvrewards
      rewadf_w[i,"ilvrewa_acc"] <- rewadf_w[i-1,"ilvrewa_acc"] + rewadf_w[i,"ilvreward"] - rewadf_w[i-1,"ilvreward"] 
      
      # ILV reward rewards
      # ilv_rewa_claimed
      rewadf_w[i, "ilvclaimed"] <- rewadf_w[i-1,"ilvclaimed"]
      # daily rewards from claimed-stake
      rewadf_w[i,"ilvclaimrewards"] <- rewadf_w[i-1,"ilvclaimrewards"] + rewadf_w[i, "ilvclaimed"] * rewadf_w[i,"ilvrewaapy_day"] #rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
      
      # accumulated rewards from ILVs that were claimed
      #rewadf_w[i, "ilvclaimcomp"] <-rewadf_w[i-1, "ilvclaimcomp"] + rewadf_w[i-1,"ilvrewa_claimed"]*rewadf_w[i,"ilvrewaapy_day"]
      # accumulated rewards from ILV staking
      #rewadf_w[i, "ilvsingle"] <- rewadf_w[i-1, "ilvsingle"] +rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"]
      
      
      
      
      # Claim?
      if(rewadf_w[i,"claim"]==1){
        #rewadf_w[i:nrow(rewadf_w),"ilvrewa_claimed"] <-rewadf_w[i, "ilvrewa_claimed"] + rewadf_w[i-1,"slprewards"]     
        rewadf_w[i:nrow(rewadf_w),"ilvclaimed"] <-rewadf_w[i, "ilvclaimed"] + rewadf_w[i-1,"ilvreward"] +rewadf_w[i-1,"ilvclaimrewards"]+ rewadf_w[i-1,"slprewards"] 
        
        rewadf_w[i,"ilvreward"] <- rewadf_w[i,"ilvstake"]* rewadf_w[i,"ilvapy_day"] 
        rewadf_w[i,"ilvclaimrewards"] <-  rewadf_w[i, "ilvclaimed"] * rewadf_w[i,"ilvrewaapy_day"]
        rewadf_w[i,"slprewards"] <- rewadf_w[i,"slp_stake"]* rewadf_w[i,"slpapy_day"]
        rewadf_w[i,"gas"] <- -gas
      }
      # accumulated ilvrewards
      rewadf_w[i,"ilvclaimrewards_acc"] <- rewadf_w[i-1,"ilvclaimrewards_acc"] + rewadf_w[i, "ilvclaimed"] * rewadf_w[i,"ilvrewaapy_day"]
      
      # Gas accu
      rewadf_w[i, "gas_accu"] <- rewadf_w[i-1, "gas_accu"] + rewadf_w[i, "gas"]
    }
    
    
    
    rewadf_w %>% ggplot(aes(x = day, y = slp_stake*ilvprice + ilvstake*ilvprice + ilvrewa_acc*ilvprice + slprewa_acc*ilvprice+ ilvclaimrewards_acc*ilvprice,color="#fc8d59",size=2)) + #overall
      geom_path() +
      geom_path(aes(y = slp_stake*ilvprice + ilvstake*ilvprice + slprewa_acc*ilvprice,colour="#fee08b",size=2)) + # just SLP
      geom_path(aes(y = slp_stake*ilvprice + ilvstake*ilvprice + ilvrewa_acc*ilvprice,colour="#e6f598",size=2)) + # just ilv
      geom_path(aes(y = slp_stake*ilvprice + ilvstake*ilvprice + ilvclaimrewards_acc*ilvprice,colour="#99d594",size=2)) + # just claimed ILV
      geom_step(aes(y = slp_stake*ilvprice + ilvstake*ilvprice + gas_accu,colour="#d53e4f",size=2)) + # Gas
      geom_step(aes(y=slp_stake*ilvprice + ilvstake*ilvprice + ilvrewa_acc*ilvprice + slprewa_acc*ilvprice+ ilvclaimrewards_acc*ilvprice+gas_accu,color="#3288bd",size=2)) + # overall - Gas
      #geom_segment(x=0, y=stake*ilvprice, xend=claimdays, yend= stake*ilvprice,linetype="dashed",size=1,color="black") +
      geom_hline(size=1,aes(yintercept=slp_stake*ilvprice + ilvstake*ilvprice,color="black")) +
      xlab("Days") + ylab("Yield [$]")  + theme_classic() +
      theme(legend.position="bottom",
            legend.text = element_text(size=16, face="bold"),
            axis.text = element_text(size=16, face="bold"),
            axis.title = element_text(size=16, face="bold"))+#,
      #axis.text.y = element_text(size=16, face="bold")) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_identity(guide = "legend", name="", breaks=c("#fc8d59", "#fee08b","#e6f598", "#99d594", "#d53e4f", "#3288bd", "black"),
                           labels = c("Total rewards","SLP rewards","ILV pool rewards","Claimed ILV rewards", "Gas fees", "Total rewards - Gas fees", "Initial investment [$]")) +
      guides(color = guide_legend(override.aes = list(size=5)))+
      scale_size_identity(guide="none") #+
               }
      )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
