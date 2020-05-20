responsesDir <- file.path("responses")

loadData <- function() {

  files <- list.files(file.path(responsesDir), full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE)
  data <- bind_rows(data)
  data
}


  


# sumdata <- function(){
# d<- loadData() 
# c1 <- c(mean(d$Child_age_months), min(d$Child_age_months), max(d$Child_age_months), max(d$Child_age_months) - min(d$Child_age_months),sd(d$Child_age_months))

# d <- filter(d, ACE_Total_Score!="")
# c2 <- c(mean(d$ACE_Total_Score),min(d$ACE_Total_Score),max(d$ACE_Total_Score),max(d$ACE_Total_Score)-min(d$ACE_Total_Score),sd(d$ACE_Total_Score))

# c3 <- c("Average", "Min", "Max", "Range", "SD")
# d <- data.frame("Summarise"= c3, "Child Age"=c1, "ACEs Score"=c2)
# d
# }
#d<- loadData() 

#write.csv(x = d, file = file.path(responsesDir, "data"),
#          row.names = FALSE, quote = TRUE)





