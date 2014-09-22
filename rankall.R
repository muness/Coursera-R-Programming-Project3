rankall <- function(outcome, num="best") {

  if (! outcome %in% c("pneumonia", "heart attack", "heart failure")) stop("invalid outcome")
  
  outcome_data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)
  
  outcome_column_regex<-paste("^Hospital.30.Day.Death..mortality.*", sub(" ", ".", outcome), sep="")
  outcome_column<-grep(outcome_column_regex,colnames(outcome_data), ignore.case=TRUE, value=TRUE)
  hospitals<-subset(outcome_data, select=c("Hospital.Name", outcome_column, "State"), )
  colnames(hospitals)<-c("name", "deaths", "state")
  hospitals$deaths<-suppressWarnings(as.numeric(hospitals$deaths))
  
  ranked<-subset(hospitals,!is.na(hospitals$deaths))
  ranked<-ranked[order(ranked$deaths, ranked$name),]
  
  if (num=="worst") {
    ranked$rank_within_state<-ave(ranked$deaths,ranked$state, FUN = function(x) rank(-x, ties.method = "first"))
    num<-1
  } else {
    ranked$rank_within_state<-ave(ranked$deaths,ranked$state, FUN = function(x) rank(x, ties.method = "first"))  
  }
  
  if (num=="best") num <-1
  
  colnames(ranked)<-c("hospital", "deaths", "state", "rank_within_state")
  
  hospitals<-subset(ranked, rank_within_state==num, select=c("hospital", "state"))

  states<-unique(subset(outcome_data, select=c("State")))
  colnames(states)<-c("state")
  hospitals<-merge(hospitals, states, by="state", all.y=TRUE)
  
  hospitals<-hospitals[order(hospitals$state, hospitals$hospital), ]
  hospitals
}