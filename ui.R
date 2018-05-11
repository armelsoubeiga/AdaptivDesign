
library(shinythemes)
library(dygraphs)
# non-standard plot dimensions
verName<-'EAGLE.1' 
width <- "90%"          
height <- "500px" 


#Plot function
my_plotOutput <- function(...)
  plotOutput(..., width=width, height=height)

#NOTE: header element defaults to h3 instead of h1
my_headerPanel <- function (title, windowTitle = title, h=h3){
  tagList(tags$head(tags$title(windowTitle)), div(class = "span12", 
  style = "padding: 10px 0px;", h(title)))
}

#Compile html
pbreak<-HTML('<P CLASS=breakhere>')
readHelpTabHTML<- paste0(readLines('help_tab.html'),collapse='') 
animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton = NULL)
readHelpTabHTML.1<- paste0(readLines('help_tab_1.html'),collapse='') 
animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton = NULL)

###########################################united############################################ navbarPage
shinyUI(navbarPage(theme = shinytheme("cerulean"), HTML('<rigth><img src="BD.PNG" width="150" height="75"></rigth>'),
  tabPanel(h4(strong("Fixed Design"),style = "color: #fcfcfc"),
        sidebarLayout(
          sidebarPanel(
           conditionalPanel(
              condition="input.conditionedPanels==1 | input.conditionedPanels==3",
              sliderInput("Nrange", label = h6("Range of sample size"),
              min=20, max=4000,step=100, value=c(1000,2000), sep = ""),
              sliderInput("RRrange", label=h6("Range of risk ratio (RR)"),
              min=0.1, max=1.0, value=c(0.6,0.8), step=0.1),
              numericInput("nratio", label = h6("Exposure prevalence ratio = n2/n1"), value=0.1, min=0.1, max=20, step=0.1),
              helpText(h6('n1= sample size un-exposed and n2=sample size exposed')),
              numericInput("p1", label = h6("Proportion with outcome in un-exposed (p1)"), value=0.2, min=0.1, max =0.9, step=0.1)),
              conditionalPanel(
              condition="input.conditionedPanels==2 | input.conditionedPanels==4",
              sliderInput("p1range", label=h6("Range of proportion with outcome in un-exposed (p1)"),
              min=0.1, max=0.9, value=c(0.1,0.9), step=.1),
              sliderInput("nratiorange", label=h6("Range of Exposure Prevalence Ratio (nratio)"),
              min=0.1, max=20, value=c(0.1,0.3), step=.1),
              helpText(h6('Exposure Prevalence Ratio (nratio) = n2/n1 where n1=size of un-exposed group and n2=size of exposed group')),
              numericInput("RR", label = h6("Risk Ratio"), value=0.8, min=0.1, max =0.9, step=0.1),
              numericInput("power", label=h6("Power"), value=0.8, min=0.1, max =0.9, step=0.1)),
              conditionalPanel(
              condition="input.conditionedPanels==1 | input.conditionedPanels==2 | input.conditionedPanels==3 | 
                                  input.conditionedPanels==4",
              numericInput("alpha", "Significance level (0.05, 0.01, 0.001)", value=0.05, min=0.001, max =0.05))),
          
           mainPanel(
              tabsetPanel(
              tabPanel("Description", value=1, h4("Power and Sample Size for Two-Sample Proportions Test"),
                                           p('The goal of this analysis is to determine the sample size required or evaluate the power for a test intended to
                                             determine if there is an association between the probability of an outcome of interest and the level of the 
                                             exposure factor. The computations are based on the ',strong('Pearson-s chi-squared test') ,' for two independent 
                                             binomial populations and are performed for a two-sided hypothesis test.'),
                                           br(),
                                           h4('Parameters used in the computations'),
                                           p('These parameters are all inter-related such that if you know some you can estimate the others so need not provide 
                                             all to get power/sample size.'),
                                           strong('Risk-ratio :'), 'corresponds to the smallest reduction in the risk (effect size) that one would like to be able to
                                             detect, e.g. effect size of 20% => RR of 80% .',
                                           strong('alpha'), ': Confidence level desired .',
                                           'Level of ', strong('power'), 'desired .',
                                           strong('n1'), ' and/or', strong('n2'),': sample sizes in the un-exposed and exposed groups respectivelly, 
                                             corresponding to prevalence of
                                             the exposure .',
                                           strong('p1'),' and/or ', strong('p2'), ': proportion developing the outcome in the unexposed group and exposed group respectively (usually estimated from previous
                                             research .',
                                           strong('nratio'),': ratio of un-exposed subjects to exposed subjects (n2/n1) .',
                                           br(),
                       HTML('<Center><img src="bd.1.PNG" width="450" height="400"></Center>'),
                       br()),
                tabPanel("Power calculator Plot", value=3, h4("Power calculator Plot"),
                                           p('Evaluate the ', 
                      strong('power'), ' for a range of sample sizes and effect sizes given a fixed value of other study 
                      parameters.'),
                      plotOutput("powPlotP", width=900)),
                                  
                tabPanel("Sample Size Calculator Plot", value=4, h4("Sample Size Calculator Plot"),
                                           p('Compute the ', 
                      strong('sample size'), ' for a range of control-group (unexposed) proportions developing the 
                      outcome and a range of exposure prevalence ratios (n2/n1), given a fixed value of other study parameters.'),
                      plotOutput("powPlotN", width=900)),
               tabPanel("Power calculator Table", value=1, h4("Power calculator table"),
                      p('Evaluate the ',
                      strong('power'), ' for a range of sample sizes and effect sizes given a fixed value of other study 
                      parameters.'), 
                      dataTableOutput("powTableP")),
              tabPanel("Sample Size Calculator Table", value=2, h4("Sample Size Calculator table"),
                      p('Compute the ', 
                      strong('sample size'), ' for a range of control-group (unexposed) proportions developing the 
                      outcome and a range of exposure prevalence ratios (n2/n1), given a fixed value of other study parameters.'), 
                      dataTableOutput("powTableN")),
                                  id="conditionedPanels")))), 
    
##############################################################################################

tabPanel(h4(strong("Group Sequential Design"),style = "color: #fcfcfc"),
         sidebarLayout(
           sidebarPanel(
             selectInput("params", "", c("Computed power" = "1",
                                         "Computed sample size" = "2",
                                         "Sample size conditional treatment effect and power"="3"),selected = 2),
             br(),br(),
             sliderInput("delta","traitement effect", min = -0.5, max = 0.5, value = 0.07, step = 0.1),
            numericInput("alpha","alpha", min = 0.0001, max = 1, value = 0.025, step = 0.0001),
             numericInput("power","Power", min = 0.1, max = 1, value = 0.8, step = 0.1),
             numericInput("k","Number of Interim analyse", min = 1, max = 10, value = 3, step = 1),
             numericInput("p1","Prob. outcome = 1 under control", min =0, max =1, value = 0.10, step = 0.1),
             numericInput("p2","Prob. outcome = 1 under treatment ", min = 0, max = 1, value = 0.15, step = 0.1),
             br()), 
           
           ###Main
           mainPanel(
             
             #OUTPUT
             radioButtons("OutputSelect", em(strong("Output select")),
                          c("About" = "1", "Designs" = "2"), selected="2"),
             br(), pbreak,
               conditionalPanel(condition = "input.OutputSelect == '1'",
                              HTML(paste(readHelpTabHTML.1,collapse='')),
                              HTML('<Center><img src="bd.1.PNG" width="450" height="400"></Center>')),
             
               conditionalPanel(condition = "input.OutputSelect == '2'",
                              em(strong("Designs")),
                              tabsetPanel(
                                tabPanel("Design of pocok method",
                                         plotOutput("pocok"),
                                         br(),pbreak),
                                         #tableOutput("pocok_table")),
                                tabPanel("Design of O’Brien and Flemming  method",
                                         plotOutput("of"),
                                         br(),pbreak),
                                        # tableOutput("of_table")),
                                tabPanel("Design of Wang and Tsiatis method",
                                         plotOutput("wt"),
                                         br(),pbreak),
                                         #tableOutput("wttable")),
                                tabPanel("Comparaison of All method",
                                         br(),br(),br(),
                                         textOutput("pocok.of.wt")
                                         #printOutput("comparaison_table")
                                         ))))
           
         )),
##############################################################################################

tabPanel(h4(strong("Adaptive Design"),style = "color: #fcfcfc"),
    sidebarLayout(
      sidebarPanel(
        

 #TOP PANEL
selectInput("Which_params", "", c("Show Basic Parameters" = "1",
                                          "Show Advanced Parameters" = "2"),selected = 2),

actionButton(inputId = "input_action", label = "Apply to Show"),

        

conditionalPanel(condition = "input.Which_params == '1'",
                        sliderInput("p1_user_defined","Subpopulation 1 proportion", min = 0, max = 1, value = 0.33, step = 0.1),
                        sliderInput("p10_user_defined","Prob. outcome = 1 under control, subpopulation 1", min = 0.1, max = 0.99, value = 0.25, step = 0.1),
                        sliderInput("p20_user_defined","Prob. outcome = 1 under control, subpopulation 2", min = 0.1, max = 0.99, value = 0.2, step = 0.1),
                        sliderInput("p11_user_defined","Prob. outcome = 1 under treatment for subpopulation 1", min = 0.1, max = 0.99, value = 0.37, step = 0.1),
                        sliderInput("per_stage_sample_size_combined_adaptive_design_user_defined","Per stage sample size, combined population, for adaptive design", min = 0, max = 1000, value = 280, step = 1),
                        sliderInput("per_stage_sample_size_when_only_subpop_1_enrolled_adaptive_design_user_defined","Per stage sample size for stages where only subpopulation 1 is enrolled, for adaptive design", min = 0, max = 1000, value = 148, step = 1),
                        sliderInput("alpha_FWER_user_defined","Alpha (FWER) requirement for all designs", min = 0.005, max = 0.1, value = 0.025, step = 0.005),
                        sliderInput("alpha_H0C_proportion_user_defined","Proportion of Alpha allocated to H0C for adaptive design", min = 0, max = 1, value = 0.09, step = 0.1)
),

#ADVANCED BOXES
conditionalPanel(condition = "input.Which_params == '2'",
                 numericInput("Delta","Delta", min = -0.5, max = 0.5, value = -0.5, step = 0.1),
                 numericInput("iter","Of Iterations for simulation", min = 0, max = 100000, value = 7000, step = 1),
                 numericInput("time_limit","Time limit for simulation, in seconds", min = 5, max = 180, value = 45, step = 1),
                 numericInput("total_number_stages","Total number of stages", min = 1, max = 20, value = 5, step = 1),
                 numericInput("last_stage_subpop_2_enrolled_adaptive_design","Last stage subpopulation 2 is enrolled under adaptive design", min = 1, max = 20, value = 3, step = 1),
                 numericInput("enrollment_rate_combined_population","Participants enrolled from combined population", min = 0, max = 1000, value = 420, step = 1),
                 numericInput("per_stage_sample_size_combined_standard_design_H0C","Per stage sample size for standard group sequential design (SC) enrolling combined pop.", min = 0, max = 5000, value = 106, step = 10),
                 numericInput("per_stage_sample_size_combined_standard_design_H01","Per stage sample size for standard group sequential design (SS) enrolling only subpop. 1", min = 0, max = 5000, value = 100, step = 10),
                 numericInput("subpopulation_2_stopping_boundary_proportionality_constant_adaptive_design","Stopping boundary proportionality constant for subpopulation 2 enrollment for adaptive design", min =-10, max = 10, value = 0, step = 0.1),
                 numericInput("H01_futility_boundary_proportionality_constant_adaptive_design","H01 futility boundary proportionality constant for adaptive design", min = -10, max = 10, value = 0, step = 0.1),
                 numericInput("H0C_futility_boundary_proportionality_constant_standard_design","H0C futility boundary proportionality constant for standard design", min = -10, max = 10, value = -0.1, step = 0.1),
                 numericInput("H01_futility_boundary_proportionality_constant_standard_design","H01 futility boundary proportionality constant for standard design", min = -10, max = 10, value = -0.1, step = 0.1)
 )),
      
###Main
      mainPanel(

        #WARNINGS
        h4(textOutput('warn1')),
        h4(textOutput('warn2')),
        h4(textOutput('warn3')),
        #br(),br(),
        
        #OUTPUT
        fluidRow(
          column(6, radioButtons("OutputSelection", em(strong("Output selection")),
                                 c("About App" = "1", "Designs" = "2", "Performance" = "3"), selected="2")),
          column(6, radioButtons("OutputSelection.1", em(strong("Adaptive design technique")),
                                 c("Pocock" = "1", "O’Brien and Fleming" = "2"), selected="1"))),
        br(), pbreak,

        conditionalPanel(condition = "input.OutputSelection == '1'",
                         HTML(paste(readHelpTabHTML,collapse='')),
                         HTML('<Center><img src="bd.1.PNG" width="450" height="400"></Center>')),
        
        conditionalPanel(condition = "input.OutputSelection == '2'",
                         em(strong("Designs")),
                         tabsetPanel(
                           tabPanel("Adaptive",
                                    my_plotOutput("adapt_boundary_plot"),
                                    br(),pbreak,
                                    tableOutput("adaptive_design_sample_sizes_and_boundaries_table")),
                           tabPanel("Standard, Total Population",
                                    my_plotOutput("standard_H0C_boundary_plot"),
                                    br(),pbreak,
                                    tableOutput("standard_H0C_design_sample_sizes_and_boundaries_table")),
                           tabPanel("Standard, Subpop. 1 only",
                                    my_plotOutput("standard_H01_boundary_plot"),
                                    br(),pbreak,
                                    tableOutput("standard_H01_design_sample_sizes_and_boundaries_table")),
                           tabPanel("All designs",
                                    tableOutput("adaptive_design_sample_sizes_and_boundaries_table.2"),
                                    pbreak,
                                    tableOutput("standard_H0C_design_sample_sizes_and_boundaries_table.2"),
                                    pbreak,
                                    tableOutput("standard_H01_design_sample_sizes_and_boundaries_table.2")),
                           selected="Adaptive")
        ),
        
        conditionalPanel( condition = "input.OutputSelection == '3'",
                          tabsetPanel(
                            tabPanel("Power", my_plotOutput("power_curve_plot")),
                            tabPanel("Sample Size", my_plotOutput("expected_sample_size_plot")),
                            tabPanel("Duration", my_plotOutput("expected_duration_plot")),
                            selected='Power'),
                          pbreak,
                          tableOutput("performance_table")))))
))