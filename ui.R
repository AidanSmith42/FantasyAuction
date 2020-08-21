library(shinythemes)
library(shiny)
library(shinydashboard)
library(DT)
dashboardPage(skin='black',
              dashboardHeader(
                              title = span("Auction Optimizer", 
                                                style = "font-size: 88%; font-family: Roboto Mono;")),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Add Players", startExpanded=T, icon=icon("plus-square"), 
                           selectInput('Position', 'Select Position', choices = c("QB", "RB", "WR", "TE", "D")),
                           selectInput('Player', 'Select Player: ', choices = NULL),
                           numericInput('Salary', 'Enter Salary: ', value = 0),
                           actionButton('Submit', 'Submit', icon=icon('refresh'), width='85%'),
                           htmlOutput('Value'),
                           hr(),
                           actionButton('Remove', 'Remove Player',icon =icon("minus-square"), width='85%'),
                           br()),
                  menuItem("Settings", icon=icon('cog'),
                           selectInput('Scoring', 'Scoring Settings', choices = c('Half-PPR', 'PPR', 'Standard')),
                           numericInput('QB', '# of QBs', 1),
                           numericInput('RB', '# of RBs', 1),
                           numericInput('WR', '# of WRs', 2),
                           numericInput('TE', '# of TEs', 1),
                           numericInput('FLEX', '# of FLEX', 2),
                           actionButton('SaveChanges', 'Save Roster Settings',icon =icon("save"), width='85%')),
                  menuItem("Log", icon=icon("history"),
                           htmlOutput('log')))),
              dashboardBody(
                
                tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Roboto+Mono" rel="stylesheet">')),
                tags$head(HTML('<style>* {font-size: 98%; font-family: Roboto Mono;}</style>')),
                conditionalPanel(condition="input.Submit != 0",
                wellPanel(style = "background-color: #FFFAFA; border-color: #2c3e50;",
                          fluidRow(column(6,h3(HTML("<center> My Team </center>")),
                          DTOutput("table2"),
                          htmlOutput('text')),
                          column(6,
                          h3(HTML("<center> Optimal Remaining </center>")),
                          
                          DTOutput("table3"))))),
                wellPanel(style = "background-color: #FFFAFA; border-color: #2c3e50;",
                          h3(HTML("<center>Best Team</center>")),
                          DTOutput("table"),
                          br(),
                          hr(),
                          h3(HTML("<center>Players</center>")),
                          DTOutput("dat"),
                          fluidRow(column(4,
                          fileInput("file1", "Import Custom Projections", accept = ".csv")),
                          column(4, 
                                 br(),
                                 actionButton('upload', "Upload:", icon=icon("upload"))),
                          column(4,
                          br(),
                          downloadButton('downloadData', 'Download:', icon=icon("download")))),
                          HTML("<div style='float:center'>
                                    <a href='https://twitter.com/share' 
                               class='twitter-share-button' 
                               align='middle' 
                               data-url='psmith54.shinyapps.io/auction' 
                               data-text='Build an optimal auction draft with this app. #Draft #fantasyFootball' 
                               data-size='large'>Tweet
                               </a>
                               <script>!function(d,s,id){
                               var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';
                               if(!d.getElementById(id)){
                               js=d.createElement(s);
                               js.id=id;
                               js.src=p+'://platform.twitter.com/widgets.js';
                               fjs.parentNode.insertBefore(js,fjs);
                               }
                               }(document, 'script', 'twitter-wjs');
                               </script>
                               </div>"))
              )       
)