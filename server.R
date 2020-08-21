library(dplyr)
library(DT)
library(data.table)
library(dummies)
library(lpSolve)


server <- function(input, output, session){
  

  #dat <- fread("dfs3.csv") %>% na.omit() %>% as.data.frame()
  dat <- fread("DataProj.csv") %>% na.omit() %>% as.data.frame() %>% select(Name, Position,FDPT, Salary)
  #dat <- left_join(dfs %>% na.omit() %>% as.data.frame(), dat)

  dat <- dat %>% filter(Position != 'K')

  
  #Projections <- fread("Projections.csv")  %>% as.data.frame()
  
  
  #dat[1:length(Projections$V2),3] <- round(Projections$V2[1:length(Projections$V2)], 2)
  #dat <- dat %>% filter(!is.na(Name))
  #dat[(length(Projections$V2)+1):nrow(dat),3 ] <- round(seq(2,1,-(1)/(nrow(dat)-(length(Projections$V2)+1))),2)
  
  
  Names <- dat %>% select(Name)
  updateSelectInput(session, "Player", choices=Names$Name)
  
  TAKEN <- character()
  
  output$dat <- renderDT(dat, class='cell-border stripe',options=list(pageLength=20))

  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste('fantasydata-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(dat, con)
    }
  )
  
  
  observeEvent(input$Position,{
    Names <- dat2 %>% filter(dat2$Position == input$Position) %>% select(Name)
    updateSelectInput(session, "Player", choices=Names$Name)
  })
  
  
  names(dat) <- c("Name", "Position", "Points", "Salary")
  fd <- dat[order(dat[, "Position"]), ]
  Position.Mat <- dummy(fd[, "Position"])
  colnames(Position.Mat) <- c("D", "QB", "RB", "TE", "WR")
  
  Position.Mat <- cbind(Position.Mat, Flex = rowSums(Position.Mat[, c("RB", "TE", "WR")]))
  
  f.obj <- fd[, "Points"]
  
  f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
  colnames(f.con) <- fd$Name
  f.dir <- rep(0, nrow(f.con))
  f.rhs <- rep(0, nrow(f.con))
  
  f.dir[1] <- "<="
  f.rhs[1] <- 190#60000
  
  f.dir[2] <- "="
  f.rhs[2] <- 1
  
  f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
  f.rhs[3:nrow(f.con)] <- c(1, 1, 1, 2, 6)
  
  
  opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
  picks <- fd[which(opt$solution == 1), ]
  picks <- as.data.table(picks)
  names(picks) <- c("Name", "Position", "Points", "Salary")

  output$table <- renderDT(picks,class='cell-border stripe', options=list(dom="t"))
  myTeam <- data.frame('Pos' = NA, 'Player' = NA, 'Salary'= NA, 'Value' = NA) 
  
  dat2 <- fread("DataProj.csv") %>% na.omit() %>% as.data.frame() %>% select(Name,  Position, FDPT, Salary)
    #fread("dfs3.csv") %>% na.omit() %>% as.data.frame()
  
  names(dat2) <- c("Name", "Position", "Points", "Salary")

  
  #dat[1:length(Projections$V2),3] <- round(Projections$V2[1:length(Projections$V2)], 2)
  dat <- dat %>% filter(!is.na(Name))
  #dat[(length(Projections$V2)+1):nrow(dat),3 ] <- round(seq(2,1,-(1)/(nrow(dat)-(length(Projections$V2)+1))),2)
  Names <- dat2  %>% select(Name)
  
  team <- myTeam %>% na.omit()
  dat2<- dat2 %>% filter(!(Name %in% team$Player))
  dat2 <- dat2 %>% filter(!is.na(Name))
  #dat2[1:length(Projections$V2),3] <- round(Projections$V2[1:length(Projections$V2)], 2)
  #dat2[(length(Projections$V2)+1):nrow(dat),3 ] <- round(seq(2,1,-(1)/(nrow(dat2)-(length(Projections$V2)+1))),2)
  
  ################## ALLOW FOR CUSTOM INPUT VALUES###################################
  observeEvent(c(input$SaveChanges, input$upload),
               {
                if(input$upload !=0 ){  
                 if(substring(input$file1$name, gregexpr('\\.', input$file1$name)  %>% as.numeric()) != ".csv"){
                   
                   showModal(
                     modalDialog(
                       size = 'l',
                       title="Please upload a csv file",
                       easyClose = T
                     )
                   )
                   return(NA)
                 }

                 if(ncol(fread(input$file1$datapath) %>% na.omit() %>% as.data.frame()) != 4){
                   showModal(
                     modalDialog(
                       size = 'l',
                       title="Invalid Format, expecting 4 columns",
                       easyClose = T
                     )
                   )
                 return(NA)
               }
                 
                 dat <- fread(input$file1$datapath) %>% na.omit() %>% as.data.frame()
                 dat2 <- fread(input$file1$datapath) %>% na.omit() %>% as.data.frame()
                 
                }
                 if(input$upload == 0){
                   if(input$Scoring == 'Half-PPR'){
                   dat <<- fread("DataProj.csv") %>% na.omit() %>% as.data.frame() %>% select(Name, Position, FDPT, Salary)
                   dat2 <<- dat
                   }
                   if(input$Scoring == 'PPR'){
                     dat <<- fread("DataProj.csv") %>% na.omit() %>% as.data.frame() %>% select(Name, Position, PPR, Salary)
                     dat2 <<- dat
                   }
                   if(input$Scoring == 'Standard'){
                     dat <<- fread("DataProj.csv") %>% na.omit() %>% as.data.frame() %>% select(Name, Position, Standard, Salary)
                     dat2 <<- dat
                   }
                   names(dat) <- c("Name", "Position", "Points", "Salary")
                   names(dat2) <- c("Name", "Position", "Points", "Salary")
                 }
                 #dat <- fread("DataProj.csv") %>% na.omit() %>% as.data.frame() %>% select(Name, Position, FDPT, Salary)
                 dat <- dat %>% filter(Position != 'K')
                
                 names(dat) <- c("Name", "Position", "Points", "Salary")
           
                 output$dat <- renderDT(dat, class='cell-border stripe',options=list(pageLength=20))
                 fd <- dat[order(dat[, "Position"]), ]
                 Position.Mat <- dummy(fd[, "Position"])
                 colnames(Position.Mat) <- c("D", "QB", "RB", "TE", "WR")
                 
                 Position.Mat <- cbind(Position.Mat, Flex = rowSums(Position.Mat[, c("RB", "TE", "WR")]))
                 
                 f.obj <- fd[, "Points"]
                 
                 f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
                 colnames(f.con) <- fd$Name
                 f.dir <- rep(0, nrow(f.con))
                 f.rhs <- rep(0, nrow(f.con))
                 
                 f.dir[1] <- "<="
                 f.rhs[1] <- 190#60000
                 
                 f.dir[2] <- "="
                 f.rhs[2] <- 1
                 
                 f.dir[3:nrow(f.con)] <- c("=", ">=", ">=", ">=", "=")
                 f.rhs[3:nrow(f.con)] <- c( input$QB, input$RB, input$TE, input$WR, (input$FLEX + input$WR+ input$RB+input$TE))
                   #c(1, 1, 1, 2, 6)
                 
                 
                 opt <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
                 picks <- fd[which(opt$solution == 1), ]
                 picks <- as.data.table(picks)
                 names(picks) <- c("Name", "Position", "Points", "Salary")

                 output$table <- renderDT(picks,class='cell-border stripe', options=list(dom="t"))
                 myTeam <- data.frame('Pos' = NA, 'Player' = NA, 'Salary'= NA, 'Value' = NA) 
                 
                 #names(dat2) <- c("Name", "Position", "Points", "Salary")
                 
                 
                 
                 #Names <- dat2  %>% select(Name)
                 
                 #team <- myTeam %>% na.omit()
                 #dat2<<- dat2 %>% filter(!(Name %in% team$Player))
                # dat2 <<- dat2 %>% filter(!is.na(Name))
                 
                 
               })
  
  observeEvent(input$Remove,{

    names(dat) <- c("Name", "Position", "Points", "Salary")
    names(dat2) <- c("Name", "Position", "Points", "Salary")
    dat2 <- dat2 %>% filter(Name != input$Player)
    Names <- dat2  %>% select(Name)
    TAKEN <<- c(TAKEN, paste0(input$Player, " selected"))
    #for(i in 1:length(TAKEN))
    output$log <- renderUI(HTML(paste0("<center> ", TAKEN , "<br>")))
  
    updateSelectInput(session, "Player", choices=Names$Name)
    positionUpdate <- dat2 %>% filter(Name == Names$Name[1]) %>% select(Position) %>% as.character()
    updateSelectInput(session, "Position", selected=positionUpdate)
    team <- myTeam %>% na.omit()
    dat2 <- dat2 %>% filter(!(Name %in% team$Player))
    
    
    
    Positions <-c("D","QB", "RB", "TE", "WR", "FLEX")
    S <-        c( "=","=", ">=", "=", ">=", "=")
    mat <- rbind.data.frame(Positions, S, stringsAsFactors = F)
    names(mat) <- Positions
    count <- team %>% filter(!(Pos %in% c("QB", "D", "TE"))) %>% nrow()
    countD <- 0
    countQB <- 0
    countRB <- team %>% filter(Pos == 'RB') %>% nrow()
    countWR <- team %>% filter(Pos == 'WR') %>% nrow()
    countTE <- 0
    
    if("D" %in% team$Pos){
      dat2 <- dat2 %>% filter(Position != "D")
      Positions <- Positions[!grepl("D", Positions)]
      countD<- countD +1 
      mat <- mat[Positions]
    }
    if("QB" %in% team$Pos){
      dat2 <- dat2 %>% filter(Position != 'QB')
      Positions <- Positions[!grepl("QB", Positions)]
      countQB <- countQB + 1
      mat <- mat[Positions]
    } 
    
    if(any(c("RB", "WR", "TE") %in% team$Pos)){
      if("RB" %in% team$Pos) {
        countRB <- team %>% filter(Pos == 'RB') %>% nrow()
        mat <- mat[Positions]
      }
      if("WR" %in% team$Pos){
        countWR <- team %>% filter(Pos == 'WR') %>% nrow()
        if(countWR == 2){
          mat <- mat[Positions]}
        if(countWR == 3){ 
          
          dat2<- dat2 %>% mutate(Salary = ifelse(Position=='WR', Salary+50, Salary))}
      } 
      
      
      if("TE" %in% team$Pos){
        dat2<- dat2 %>% filter(Position != 'TE')
        countTE <- countTE+1
        Positions <- Positions[!grepl("TE", Positions)]
        mat <- mat[Positions]
      } 
      
    }
    
    
    fd <- dat2[order(dat2[, "Position"]), ]
    Position.Mat <- dummy(fd[, "Position"])
    
    colnames(Position.Mat) <- Positions[-length(Positions)]
    
    Position.Mat <- cbind(Position.Mat, Flex = rowSums(Position.Mat[, c("RB", "WR")]))
    
    f.obj <- fd[, "Points"]
    
    f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
    colnames(f.con) <- fd$Name
    f.dir <- rep(0, nrow(f.con))
    f.rhs <- rep(0, nrow(f.con))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 190 - sum(myTeam$Salary %>% na.omit())
    
    #test <- c( 1-countD, 1-countQB, 1-countRB, 1-countTE, 2-countWR, 5-count)
    test <- c( 1-countD, input$QB-countQB, input$RB-countRB, input$TE-countTE, input$WR-countWR, (input$FLEX + input$WR + input$RB)-count)
    
    
    for(i in 1:length(test)){
      
      if(i != 3 & i!=5 )
      {
        if(test[i] ==0) test[i] <- NA
      }
    }
    
    test <- test %>% na.omit()
    
    f.rhs[2:(nrow(f.con))] <- test
    
    f.dir[2:(nrow(f.con))] <- as.character(mat[2,])   
    
    
    opt2 <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
    picks2 <- fd[which(opt2$solution == 1), ]
    picks2 <- as.data.table(picks2)
    
    names(picks2) <- c("Name", "Position", "Value", "Salary")
    output$table3 <- renderDT(picks2, class='cell-border stripe', options=list(dom="t"))
    dat2 <<- dat2 
    
  })
  
  
  observeEvent(input$Submit,  {

    if(is.na(input$Salary) | input$Salary == 0)return(NA)
    names(dat) <- c("Name", "Position", "Points", "Salary")
    names(dat2) <- c("Name", "Position", "Points", "Salary")
    if(input$Salary != 0){
      PlayerValue <- dat %>% filter(Name == input$Player) 
      PlayerValue <- round(PlayerValue$Points/input$Salary,2)
    }

    Player <- c( input$Position, input$Player, as.numeric(input$Salary), PlayerValue)
    myTeam <<- rbind(myTeam, Player)
    myTeam$Salary <<- as.numeric(myTeam$Salary)
    DTable <- datatable(myTeam %>% na.omit(), options= list(dom='t')) %>% formatStyle(
      'Value',
      color = styleInterval(c(1, 2, 3, 5), c('black','green', '#1E90FF', 'purple', '#FFA500'))) #c(.75, .9, 1.15, 2) 
    
    output$table2 <- renderDT(DTable, class='cell-border stripe', options=list(dom="t"), rownames = F)
    output$text <- renderUI(HTML(paste0("<center> <h4> <b>Money Remaining: $", 190- sum(myTeam$Salary %>% na.omit()), "</h4><b><br>")))
    
    TAKEN <<- c(TAKEN, paste0(input$Player, " selected by you"))
    output$log <- renderUI(HTML(paste0("<center>", TAKEN , "<br>")))
    
    team <- myTeam %>% na.omit()
    dat2<- dat2 %>% filter(!(Name %in% team$Player))

    

    Positions <-c("D","QB", "RB", "TE", "WR", "FLEX")
    S <-        c( "=","=", ">=", "=", ">=", "=")
    mat <- rbind.data.frame(Positions, S, stringsAsFactors = F)
    names(mat) <- Positions
    count <- team %>% filter(!(Pos %in% c("QB", "D", "TE"))) %>% nrow()
    countD <- 0
    countQB <- 0
    countRB <- team %>% filter(Pos == 'RB') %>% nrow()
    countWR <- team %>% filter(Pos == 'WR') %>% nrow()
    countTE <- 0
    
    if("D" %in% team$Pos){
      dat2 <- dat2 %>% filter(Position != "D")
      Positions <- Positions[!grepl("D", Positions)]
      countD<- countD +1 
      mat <- mat[Positions]
    }
    if("QB" %in% team$Pos){
      dat2<- dat2 %>% filter(Position != 'QB')
      Positions <- Positions[!grepl("QB", Positions)]
      countQB <- countQB + 1
      mat <- mat[Positions]
    } 
    
    if(any(c("RB", "WR", "TE") %in% team$Pos)){
      if("RB" %in% team$Pos) {
        countRB <- team %>% filter(Pos == 'RB') %>% nrow()
        mat <- mat[Positions]
      }
      if("WR" %in% team$Pos){
        countWR <- team %>% filter(Pos == 'WR') %>% nrow()
        if(countWR == 2){
          mat <- mat[Positions]}
        if(countWR == 3){ 
          
          dat2<- dat2 %>% mutate(Salary = ifelse(Position=='WR', Salary+50, Salary))}
      } 
      
      
      if("TE" %in% team$Pos){
        dat2<- dat2 %>% filter(Position != 'TE')
        countTE <- countTE+1
        Positions <- Positions[!grepl("TE", Positions)]
        mat <- mat[Positions]
      } 
      
    }
    
    
    fd <- dat2[order(dat2[, "Position"]), ]
    Position.Mat <- dummy(fd[, "Position"])

    colnames(Position.Mat) <- Positions[-length(Positions)]
    
    Position.Mat <- cbind(Position.Mat, Flex = rowSums(Position.Mat[, c("RB", "WR")]))
    f.obj <- fd[, "Points"]
    
    f.con <- t(cbind(Salary = fd[, "Salary"], Position.Mat))
    colnames(f.con) <- fd$Name
    f.dir <- rep(0, nrow(f.con))
    f.rhs <- rep(0, nrow(f.con))
    
    f.dir[1] <- "<="
    f.rhs[1] <- 190 - sum(myTeam$Salary %>% na.omit())
    
    test <- c( 1-countD, input$QB-countQB, input$RB-countRB, input$TE-countTE, input$WR-countWR, (input$FLEX + input$WR + input$RB)-count)
    
    for(i in 1:length(test)){
      if(i != 3 & i!=5 )
      {
        if(test[i] ==0) test[i] <- NA
      }
    }
    
    test <- test %>% na.omit()
    
    f.rhs[2:(nrow(f.con))] <- test
    
   
    f.dir[2:(nrow(f.con))] <- as.character(mat[2,])   #  S#c("=", ">=", ">=", ">=", "=")#S 
    
    
    opt2 <- lp("max", f.obj, f.con, f.dir, f.rhs, all.bin = TRUE)
    picks2 <- fd[which(opt2$solution == 1), ]
    picks2 <- as.data.table(picks2)
    
    names(picks2) <- c("Name", "Position", "Value", "Salary")
  
    output$table3 <- renderDT(picks2, class='cell-border stripe', options=list(dom="t"))
    Names <- dat2  %>% select(Name)
    positionUpdate <- dat2 %>% filter(Name == Names$Name[1]) %>% select(Position) %>% as.character()
    updateSelectInput(session, "Position", selected=positionUpdate)
    #updateSelectInput(session, "Player", selected=dat2 %>% filter(Name == Names$Name[1]) %>% select(Name) %>% as.character())
  })
  
  observeEvent(input$Salary,{
  if(is.na(input$Salary))return(NA)
    if(input$Salary != 0){
    PlayerValue <- dat %>% filter(Name == input$Player) 
    names(PlayerValue)[3] <- "Points"
    PlayerValue <- round(PlayerValue$Points/input$Salary,2)
    if(PlayerValue < 1) color = "grey"
    if(PlayerValue >= 1 & PlayerValue < 2) color= "green"
    if(PlayerValue >= 2 & PlayerValue < 3) color= "#1E90FF"
    if(PlayerValue >= 3 & PlayerValue <5) color = "#ffa335ee"
    if(PlayerValue >= 5) color = "#FFA500"
    output$Value <- renderUI(HTML(paste0("<center> <h5> <b> Value: <font color=", color, ">", PlayerValue, "</font></h5><b>")))
    }
  })
}

# if(PlayerValue < .75) color = "grey"
# if(PlayerValue >= .75 & PlayerValue < .9) color= "green"
# if(PlayerValue >= .9 & PlayerValue < 1.15) color= "#1E90FF"
# if(PlayerValue >= 1.15 & PlayerValue <2) color = "#ffa335ee"
# if(PlayerValue >= 2) color = "#FFA500"
