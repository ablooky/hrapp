library(xlsx)
df<-read.xlsx('data/clientA.xlsx', sheetName='Raw Results')
departments<-c('Finance','Accounting','Administration',
               'Creative','Marketing','Communications',
               'Human Resources','Operations','IT')

first_names<-read.csv('data/new-top-firstNames.csv')
surnames<-read.csv('data/new-top-surnames.csv')
Employees_list<-c()
random1<-sample(first_names[,2], 50,replace=FALSE)
random2<-sample(surnames[,2], 50,replace=FALSE)
for (i in 1:50){
  Employees_list<-c(paste0(random1[i],' ',random2[i]),Employees_list)
}

new_template<-df
colnames(new_template)[4:53]<-Employees_list
  for (j in 1:nrow(new_template)){
    t<-sample(1:5,50,replace=TRUE)
    n<-sample(1:50,4,replace = FALSE)
    t[n]<-0
    new_template[j,4:53]<-t
}

profile_categories<-read.xlsx('data/clientA.xlsx', sheetName='Headers')
profile_matrix<-matrix(0,nrow=50,ncol=nrow(profile_categories))
rownames(profile_matrix)<-Employees_list
colnames(profile_matrix)<-profile_categories[,2]
n<-sample(1:50,50,replace = FALSE)
profile_matrix[n[1:4],1]<-1
profile_matrix[n[5:9],2]<-1
profile_matrix[n[10:15],3]<-1
profile_matrix[n[16:50],4]<-1
n<-sample(1:50,50,replace = FALSE)
profile_matrix[n[1:8],5]<-1
profile_matrix[n[9:16],6]<-1
profile_matrix[n[17:24],7]<-1
profile_matrix[n[25:32],8]<-1
profile_matrix[n[33:40],9]<-1
profile_matrix[n[41:50],10]<-1
n<-sample(1:50,25,replace = FALSE)
profile_matrix[n[1:25],11]<-1
counter=1
for(p in 1:nrow(profile_matrix)){
  if(profile_matrix[p,11] == 0){
    profile_matrix[p,12]<-1
    profile_matrix[p,13]<-0
    
  }
  if(profile_matrix[p,11] == 1){
    profile_matrix[p,12]<-0
    profile_matrix[p,13]<-0
    
  }
  if(p%%10 == 0){
    profile_matrix[p,11]<-0
    profile_matrix[p,12]<-0
    profile_matrix[p,13]<-1
    
  }
  counter=counter+1
}
n<-sample(1:50,50,replace = FALSE)
profile_matrix[n[1:20],14]<-1
profile_matrix[n[21:30],16]<-1
profile_matrix[n[31:40],17]<-1
profile_matrix[n[41:50],18]<-1

write.xlsx(new_template,'data/client/companyA.xlsx',sheetName = 'Raw Results', row.names = FALSE)
write.xlsx(profile_matrix,'data/client/companyA.xlsx',sheetName = 'Profiles', append = TRUE  , row.names = TRUE)
write.xlsx(profile_categories,'data/client/companyA.xlsx',sheetName = 'Headers', append = TRUE, row.names = FALSE)

generate_fake_results<-function(df){
  sample_raw<-df
  col_index<-as.numeric(ncol(sample_raw))
  for (n in 1:nrow(sample_raw)) {
    for (k in 3:col_index) {
      sample_raw[n,k]<-round(runif(1,min=0,max=5),0)
    }
  }
  return(sample_raw)
}  

generate_non_participants<-function(){
  #Evelina Boone, Andrea Runner, Eldon Liston, Todd Mcgrane
}