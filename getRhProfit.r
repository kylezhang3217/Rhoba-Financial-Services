#gather reports
getIncome <- function(term) {
  identifier = toString(paste0(toString(term), "%", collapse=NULL))
  
  #income overview
  query1 <- toString(paste0("select currency, sum(amount) from incomes where release_date LIKE '", identifier, "' group by currency ORDER BY currency ASC", collapse=NULL))
  m_income <- dbSendQuery(mydb, query1)
  total_income <- fetch(m_income, n=-1)
  
  #income by type
  query2 <- toString(paste0("select currency, type, sum(amount) from incomes where release_date LIKE '", identifier, "' group by currency, type ORDER BY currency ASC, sum(amount) DESC", collapse=NULL))
  t_income <- dbSendQuery(mydb, query2)
  type_income <- fetch(t_income, n=-1)
  
  #income by consultant
  query3 <- toString(paste0("select consultant, sum(amount) from incomes where release_date LIKE '", identifier, "' AND currency='HKD' AND type='new business' group by consultant ORDER BY sum(amount) DESC", collapse=NULL))
  c_income <- dbSendQuery(mydb, query3)
  consult_income <- fetch(c_income, n=-1)
  
  #income by company
  query4 <- toString(paste0("select company, sum(amount) from incomes where release_date LIKE '", identifier, "' AND currency='HKD' AND type='new business' group by company ORDER BY sum(amount) DESC", collapse=NULL))
  d_income <- dbSendQuery(mydb, query4)
  company_income <- fetch(d_income, n=-1)

  #gather release schedules
  query1 <- toString(paste0("select rel_currency, sum(amount) from rel_schedules where date LIKE '", identifier, "' group by rel_currency ORDER BY rel_currency ASC, sum(amount) DESC", collapse=NULL))
  m_comm <- dbSendQuery(mydb, query1)
  total_comm <- fetch(m_comm, n=-1)
  
  #gather schedules via type
  query2 <- toString(paste0("select rel_currency, type, sum(amount) from rel_schedules where date LIKE '", identifier, "' group by rel_currency, type ORDER BY rel_currency ASC, sum(amount) DESC", collapse=NULL))
  t_comm <- dbSendQuery(mydb, query2)
  type_comm <- fetch(t_comm, n=-1)
  
  #gather schedules via consultant
  query3 <- toString(paste0("select consultant, sum(amount) from rel_schedules where date LIKE '", identifier, "' AND rel_currency='HKD' AND type='new business' group by consultant ORDER BY sum(amount) DESC", collapse=NULL))
  c_comm <- dbSendQuery(mydb, query3)
  consult_comm <- fetch(c_comm, n=-1)
  
  #gather schedules via company
  query4 <- toString(paste0("select company, sum(amount) from rel_schedules where date LIKE '", identifier, "' AND rel_currency='HKD' AND type='new business' group by company ORDER BY sum(amount) DESC", collapse=NULL))
  com_comm <- dbSendQuery(mydb, query4)
  company_comm <- fetch(com_comm, n=-1)
  
  #compile tables together
  total_incomes <- merge(total_income, total_comm, by.x='currency', by.y='rel_currency')
  names(total_incomes) <- c("currency", "total_income", "total_paid out")
  print(total_incomes)
  
  total_type <- merge(type_income, type_comm, by.x=c("currency", "type"), by.y=c("rel_currency", "type"))
  names(total_type) <- c("currency", "type", "income", "paid out")
  print(total_type)
  
  total_consultant <- merge(consult_income, consult_comm, by.x='consultant', by.y='consultant')
  names(total_consultant) <- c("consultant", "income", "paid out")
  print(total_consultant)
  
  total_company <- merge(company_income, company_comm, by.x='company', by.y='company')
  names(total_company) <- c("company", "income", "paid out")
  print(total_company)
}

#load yearly graph
getGraph <- function(term) {
  if (is.na(as.integer(term))) {
    print("Not year. No graph to load.")
  } else {
    identifier = toString(paste0(toString(term), "%", collapse=NULL))
    
    query1 <- toString(paste0("select release_date, sum(amount) from incomes where currency = 'HKD' AND release_date LIKE '", identifier, "' group by release_date", collapse=NULL))
    y_income <- dbSendQuery(mydb, query1)
    total_income <- fetch(y_income, n=-1)
    names(total_income) <- c("Release_date", "Total_Income")
    print(total_income)
    
    #plot incomes
    values <- c(total_income$"Total_Income")
    plot(values, main=paste(toString(term), "Incomes"), xlab="Month", ylab="HKD", type="b")
    
  }
}

#Prompt for month / year
date <- readline(prompt="Enter Year(YYYY) or Month(YYYY-MM): ")

#Load Library and Database
library(RMySQL)
mydb = dbConnect(MySQL(), user='tester', password='rhoba123',
                 dbname='i-copy', host='192.168.1.12')

#load tables
getIncome(date)
getGraph(date)

#Disconnect
dbDisconnect(mydb)







