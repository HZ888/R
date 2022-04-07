library(shiny)
library(lavaan)
library(fmsb)
library(rmarkdown)
library(knitr)
library(xtable)
library(shinyWidgets)
library(rsconnect)

ui <- fluidPage(
  titlePanel(HTML("<em><strong>BayesiCALLY</strong> -- <strong> Bayesi</strong>an <strong>C</strong>onfirmatory f<strong>A</strong>ctor ana<strong>L</strong>ysis too<strong>L</strong> through shin<strong>Y</strong></em>")),
  img(height = 120, width = 1100, src = "funding.jpg", align = "right"),
  sidebarLayout(position = "left", 
                sidebarPanel(
                  width=5,
                  tags$strong("For WHOM?"),
                  p(HTML("This web application is for researchers, data analysts or investigators who have developed a questionnaire and wish to establish its <strong>internal construct validity</strong> based on empirical survey data using Bayesian Confirmatory Factor Analysis (BCFA).")),
                  tags$strong("For WHAT purpose?"),
                  p(HTML("The software will compute estimates of <strong>item-domain correlations</strong> (standardized <strong>factor loadings</strong>) for pre-specified variable sets (i.e. the latent domains or factors). Both, conventional confirmatory factor analysis and BCFA will be conducted. For the Bayesian analysis, expert prior information regarding the truly underlying item-domain correlations will be utilized. At this stage, two different Bayesian modeling approaches are implemented (see documentation LINK).")),
                  uiOutput("tab"),
                  tags$br(),
                  tags$strong("EXAMPLE scenario"),
                  p("For instance, a questionnaire may be used to measure the three
                    latent factors anxiety, confidence 
                    and comformity. None of these attributes can be measured directly by a singular data inquiry. 
                    However, one may believe that the responses to a set of questions that relate to each factor 
                    (the questionnaire items) may give an approximate idea about an individual's positioning
                    with respect to  the latent domains that the questionnaire is aiming to measure."),
                  p("Some items may be more closely related to a latent factor than other items.
                    The magnitude of a question's estimated item-domain correlation is an indicator for how 
                    meaningful this question is in measuring the latent factor. To allow for conclusive estimation 
                    of factor loadings, the computed uncertainty intervals must exceed clinically relevant
                    thresholds. One necessary requirement to achieve this is large sample size (frequentist CFA, recommended
                    sample size n=100 to 300) or much lower sample sizes paired with valuable prior information
                    (Bayesian CFA)."),
                  p("This web app allows, 
                    for numerical (ordinal Likert scale) questionnaire data,
                    incorporation of such prior information and computation of 
                    the respective estimated item-domain correlations along with 
                    the associated uncertainty intervals."),
                  
                  tags$br(),
                  tags$strong("HOW is it done"),
                  p("Using R shiny, each domain (latent factor) of a questionnaire will be individually
                    evaluated and the corresponding item-domain correlations (factor loadings) will be estimated along with
                    uncertainty intervals (confidence intervals for frequentist analysis and 
                    credible intervals for Bayesian analysis)."),
                  tags$strong("How much does it cost?"),
                  p("Free under certain conditions."),
                  tags$strong("Disclaimer: BayesiCALLY is free and comes with no warranty."),
                  selectInput("understood","I understand the above claim",
                              c("Choose one option",
                                   "Agree",
                                   "Do not agree")),
                  conditionalPanel(condition="input.understood=='Agree'",
                                   

                  tags$br(),
                  tags$hr(style="border-color: black;"),
                  tags$br(),
                  img(height = 176, width = 200, src = "exampledata.jpg", class = "pull-right"),
                  p("Input: a CSV file (numerical, no missing value, a complete list of questions of a domain,
                    column names). Each column shall represent responses to one question.
                    Each row shall represent one participant."),
                  p("Output: tables, graphs and downloadable report that summarize
                    the results of the confirmatory factor analyses (frequentist and
                    Bayesian CFA) including estimated item-domain correlations (factor loadings) with uncertainty
                    intervals"),
                  fileInput('file1', label='Steps 1 - 2: Step 1: Upload a file in CSV format (note that in the free version, the sample size is 50 to 150)',
                            accept = c("text/csv",
                                       "text/comma-separated-values,text/plain", 
                                       ".csv")),
                  tags$hr(),
                  uiOutput("dimensionval"),
                  tags$hr(),
                  verbatimTextOutput("dimension"),
                  conditionalPanel(condition="output.dimension == 'We are happy to help!'",
                                   tags$hr(),
                                   uiOutput("independents"),
                                   tags$hr(),
                                   p(HTML("<strong>Cronbach's Alpha</strong> for the selected variables:")),
                                   uiOutput("CA"),
                                   tags$hr(),
                                   numericInput("nexpert","Choose the total number of experts in the prior data:",0,min = 1,max = 100),
                  #verbatimTextOutput("numexpert"),
                  tags$hr(),
                  tags$strong("Is the prior data aggregated or from individual experts?"),
                  selectInput("priorsource","",
                              c("Choose one option",
                                "Aggregated",
                                "From individuals")),
                  conditionalPanel(condition="input.priorsource=='From individuals'",
                  tags$hr(),
                  p("Step 3: Parameter for beta distribution:"),
                  p("(Please refer to the graph on the top right panel for graphical representation of low, medium, and high.)"),
                  #uiOutput('heightprior'),
                                  fluidRow(
                                     column(12,        
                                     uiOutput('finetuningpriora2'))
                                    )
                  ),
                  conditionalPanel(condition="input.priorsource=='Aggregated'",
                                   tags$hr(),
                                   fluidRow(
                                     column(12,
                                            p("Step 3: Parameter for beta distribution:"),
                                            uiOutput('finetuningpriora'),
                                            p("Please refer to the graph on the top right panel for graphical representation of low, medium, and high.")
                                     )
                                   )),
                  tags$hr(),
                  p("Step 5: Run R shiny to perform frequentist and Bayesian confirmatory factor analysis:"),
                  tags$br(),
                  uiOutput('ui.action'),
                  tags$br(),
                  p("This might take some time."),
                  tags$hr(),
                  p("Step 6: Compile a report in Word format."),
                  downloadButton('downloadReport')),
                  tags$hr(),
                  p("BayesiCALLY is a free web app that comes with no warranty. It is developed by Hao Zhang from the method development platform of the Quebec Strategy for Patient-Oriented Research 
                    Support for People and Patient-Oriented Research and Trials (SPOR-SUPPORT) 
                    Unit at the Department of Family Medicine at McGill University. For more information, please visit https://github.com/HZ888/Bayesian-Questionnaire-Validation."),
                  tags$hr(),
                  p("Copyright @ Quebec SPOR-SUPPORT. All rights reserved."),
                  p("Last updated on 2019-06-19.")
                  ),
                  conditionalPanel(condition="input.understood=='Do not agree'",
                                   p("Thank you for your interest!"))
                  
                  ),
                mainPanel(
                  width=7,
                  conditionalPanel(condition="input.understood=='Agree'",
                  tags$hr(),
                  p("The following are visual representations of prior item-domain options"),
                  img(height = 1000, width = 700, src = "priors.jpg", align = "center"),
                  tags$hr(),
                  textOutput('moneyface'),
                  conditionalPanel(condition="output.moneyface == 'The sample size is out of the range for free service.'",
                                   helpText("needs to pay for service fees"),
                                   img(src="moneyface.png",height = 700, width = 700,align="center")),
                  conditionalPanel(condition="output.moneyface !== 'The sample size is out of the range for free service.'"),
                  HTML("<br><br><br><br><br><br>
                       <br><br><br><br><br><br>
                       <br><br><br><br><br><br>
                       <br><br><br><br><br><br>
                       <br><br><br><br><br>"),
                  tags$hr(),
                  p("Here's the prior plot input: prior for item-domain correlation (note this is interactive) :"), 
                  conditionalPanel(condition="input.priorsource =='Aggregated'",
                  plotOutput('priorplot',height="400px")
                  ),
                  conditionalPanel(condition="input.priorsource =='From individuals'",
                                   plotOutput('priorplot2',height = "auto")
                  ),
                  HTML("<br><br><br><br><br><br>
                       <br><br><br><br><br><br>"),
                  tags$hr(),
                  p("Here's the output from factor analysis (note this might take sometime to show) :"), 
                  p("Item-domain correlation (Factor Loadings) Plot"),
                  conditionalPanel(condition="input.priorsource =='Aggregated'",
                                  plotOutput('plot'),
                                  p("Item-domain correlation (Factor Loadings) Table"), 
                                  verbatimTextOutput('FactorLoadings')),
                  conditionalPanel(condition="input.priorsource =='From individuals'",
                                   plotOutput('plot2'),
                                   p("Item-domain correlation (Factor Loadings) Table"),
                                   verbatimTextOutput('FactorLoadings2'))
                  ),
                  conditionalPanel(condition="input.understood=='Do not agree'",
                                   img(height = 500, width = 700, src = "write-research-proposal.gif", align = "center"),
                                   p("from The Boss Baby"))
                  )
                  )
                  )
  