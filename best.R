best <- function(state, outcome) {
  
  if (! outcome %in% c("pneumonia", "heart attack", "heart failure")) stop("invalid outcome")
  
  outcome_data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors=FALSE)

  outcome_for_state<-subset(outcome_data, State==state)  
  if(nrow(outcome_for_state) < 1) stop("invalid state")
  
  outcome_column_regex<-paste("^Hospital.30.Day.Death..mortality.*", sub(" ", ".", outcome), sep="")
  outcome_column<-grep(outcome_column_regex,colnames(outcome_data), ignore.case=TRUE, value=TRUE)
  hospitals<-subset(outcome_for_state, select=c("Hospital.Name", outcome_column), )
  colnames(hospitals)<-c("name", "deaths")
  hospitals$deaths<-suppressWarnings(as.numeric(hospitals$deaths))
  best<-hospitals[order(hospitals$deaths, hospitals$name),]
  best[1,1]
}