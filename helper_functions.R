# helper_functions.R
library(xlsx)
library(dplyr)
library(tidyr)
library(plotly)
library (ggplot2)
library(reactable)
library(sparkline)
library(knitr)
#library(pander)
# Datasets ----
# Preparing for import
files_list <- list.files('data/client/')
get_clients_list<-function(){
  clients_list<-c()
  files_list <- list.files('data/client/')
  for (x in 1:length(files_list)){
    clients_list<-c(gsub('.xlsx','',files_list[x]),clients_list)
  }
  return(clients_list)
}
add_dataset <- function(filename,sheetname,startrow) {
  read.xlsx2(
    filename,
    sheetName = sheetname,
    #skipEmptyRows = TRUE,
    #skipEmptyCols = TRUE,
   # colIndex = colindex,
    colClasses="character", 
    startRow = startrow,
    header = TRUE,
    #na.strings = TRUE
  )
}
import_client_data <- function(client_name,sheetname,startrow) {
  filename <- paste0('data/client/',client_name)
  ds <- add_dataset(filename,sheetname,startrow)
  return(ds)
}

# Importing profiles and survey results
get_categories_df<-function(){
  df<-raw_results<-import_client_data(files_list[1],'Headers',1)
  return(df)
}
categories_df<-get_categories_df()
categories_desc<-categories_df[,2]
categories<-unique(categories_df[,1])
get_raw_data<-function(){
  ds<-import_client_data(files_list[1],'Raw Results',1)
  names(ds)[1:3]<-c('Question_number','Question_category','Question_desc')
  return(ds)
}
raw_results<-get_raw_data()
questions_table<-raw_results[,1:3]
questions_table$question<-paste0('Question_',questions_table$Question_number)
get_profiles<-function(){
  profiles_ds<-import_client_data(files_list[1],'Profiles',1)
  colnames(profiles_ds)<-c('Participants',categories_desc)
  return(profiles_ds)
}
profile_dataset<-get_profiles()
participants_list<-profile_dataset[,'Participants']
colnames(raw_results)<-c('Question_number', 'Question_category','Question_desc', participants_list)
participants_sorted<-sort(participants_list,decreasing = FALSE)

# Modify datasets
string_replace_headers<-function(df){
  #names(df) <- gsub("\\.", "_", names(df))
  names(df) <- gsub("[[:punct:]]", "_", names(df))
  return(df)
}
modify_profile<-function(profile_dataset){
  #colmax1<-nrow(categories_df)+1
  #profile_dataset<- import_client_data(client_list[1],'Profiles',1)
  
  #Generate profile table
  #colnames(profile_dataset)<-c('participant', headers_list)
  profile_melted<- tidyr::gather(profile_dataset,key='category_desc', value='score',-Participants)
  profile_valid<-profile_melted %>% filter(score==1)
  profile_valid<-profile_valid %>% select(!score)
  profile_valid$category<-NA
  #participants_list<-sort(unique(profile_dataset$participant),decreasing = FALSE) 
  for (p in 1:nrow(profile_valid)) {
    for (n in 1:nrow(categories_df)){
      if(categories_df[n,'category_desc'] == profile_valid[p,'category_desc']) {
        profile_valid[p,'category']<-categories_df[n,'category']
      }
    }
  }
  #create a revised profile 
  #categories_list<-sort(unique(categories_df$category),decreasing = FALSE)
  profile_revised<- matrix(data=NA,ncol=length(categories),nrow=length(participants_list))
  colnames(profile_revised)<-categories
  rownames(profile_revised)<-participants_list
  #Populate profile_revised
  for (p in participants_list) {
    profile_individual<-profile_valid %>% filter(Participants==p)
    for (c in 1:nrow(profile_individual)) {
      profile_revised[p,profile_individual[c,'category']]<-profile_individual[c,'category_desc']
    }
  }
  #profile_transposed<-t(profile_revised)
  return(profile_revised)
}
modify_raw_results<-function(raw_results){
  #colmax2<-nrow(profile_dataset)+2
  #raw_results<-import_client_data(client_list[1],'Raw Results',2,1:colmax2)
  #raw_results<-get_raw_data(profile_dataset)
  #participants_list<-sort(profile_dataset[,'participant'],decreasing = FALSE)
  #raw_results populating
  #colnames(raw_results)[1]<-'Question_category'
  #colnames(raw_results)[2]<-'question_desc'
  # for(r in 1:nrow(raw_results)){
  #   if(raw_results[r,'Question_category']==''){
  #     raw_results[r,'Question_category']<-raw_results[(r-1),'Question_category']
  #   }
  # }
  #raw_results_modified<-raw_results %>% filter(question_desc != '')
  #questions_list<-data.frame(questions_number<-rownames(raw_results_modified))
  #raw_results_modified<-cbind(questions_list,raw_results_modified)
  #change_column_name<-colnames(raw_results_modified)
  raw_results<-rename(raw_results_modified,question=change_column_name[1])
  for (n in 1:nrow(raw_results)){
    raw_results[n,'question']<-paste0('Question_',raw_results[n,'question'])
  }
  return(raw_results)
}
profile_revised<-modify_profile(profile_dataset)

# Generate datasets ----
generate_formatted_results<-function(raw_results){
  temp_df<-raw_results %>% select(! c(Question_number,Question_category,Question_desc))
  #temp_df<-raw_results %>% select(! c(Question_category,Question_desc))
  #questions_list<-raw_results %>% select(Question_desc)
  #participants_list<-sort(rownames(profile_revised),decreasing = FALSE)
  #unsorted_participants_list<-names(temp_df)
  # temp_df<-raw_results[,2:(ncol(raw_results))]
  #temp_df<- sapply(temp_df, function(x) as.numeric(as.character(x)))
  #t<-data.table(Participants=names(temp_df),t(temp_df))
  #t<-data.table:transpose(temp_df)
  #results<-t(temp_df, .keep.names='col')
  results<-t(temp_df)
  results<-data.table(results)
  results<-as.matrix(results)
  rownames(results)<-participants_list
  
  #names(results)<-questions_table$question_number
  #colnames(results)<-c("Participants",questions_table$question_number)
  #t<-t(results)
  #results2<-results[sort(rownames(results)),]
  colnames(results)<-as.character(questions_table$Question_number)
  #rownames(t)<-participants_list
  other<-function(){
    for(b in 1:nrow(temp_df)){
      for (d in 1:ncol(temp_df)){
        temp_df[b,d]<-as.numeric(as.character(temp_df[b,d]))
      }
    }
  }
  return(results)
}
generate_scored_results<-function (raw_results_transposed){
  add_score<-matrix(0,nrow=nrow(raw_results_transposed),ncol=1)
  colnames(add_score)<-'overall_score'
  rownames(add_score)<- rownames(raw_results_transposed)
  raw_results_scored<-cbind(raw_results_transposed, add_score)
  for (p in 1:nrow(raw_results_scored)){
    raw_results_scored[p,'overall_score']=sum(as.numeric(raw_results_scored[p,]),na.rm=TRUE)
  }
  return(raw_results_scored)
}
max_score<-nrow(questions_table)*5
generate_merged_dataset<-function(scored_results,profile_dataset){
  merged_df<-cbind(scored_results,profile_revised)
  return(merged_df)
}
wrapper <- function(label, dev_width = dev.size("in")[1], dev_scaler = 12)  {   
  paste(strwrap(label, dev_width * dev_scaler), collapse = "\n") 
}
#erase this one at the end
generate_final_dataset<-function(df){
  final_dataset<-matrix(0, nrow=nrow(questions_table),ncol=nrow(categories_df))
  colnames(final_dataset)<-categories_df$category_desc
  rownames(final_dataset)<-questions_table$question
  leadership
  #for(n in 1:ncol(final_dataset)){
  for (n in categories_df$category_desc){
    subset_df<-subset(df,)
    for (i in 1:nrow(final_dataset)){
      
    }
  }
}

# Global variables
get_color_palette<-function(type='main'){
  cp<-c('#2B2E4A', '#e84545','#903749','#709fb0')
  if(type == 'blue'){ cp<-c('#413c69','#4a47a3','#709fb0','#a7c5eb')}
  if(type == 'second'){ cp<-c('#e84545', '#E9EF29','#032761')}
  return(cp)
}
color_palette<-get_color_palette()
color_palette2<-get_color_palette(type='blue')
retrieve_questions_table<-function(raw_results){
  questions_table<- raw_results[,c('Question_number', 'Question_category','Question_desc')]
  for (i in 1:nrow(questions_table)) {
    questions_table[i,'merged_question']<-paste(questions_table[i,'Question_number'],
                                                questions_table[i,'Question_category'],
                                                questions_table[i,'Question_desc'],sep=' | ')
  }
  return(questions_table)
}

# Generate plots ----
## summary analysis
convert_scores<-function(ds){
  for (i in 1:nrow(ds)){
    ds[i,]<- gsub(c(0),'DNK',ds[i,])
    ds[i,]<- gsub(1,'Unfavorable',ds[i,])
    ds[i,]<- gsub(2,'Unfavorable',ds[i,])
    ds[i,]<- gsub(3,'Neutral',ds[i,])
    ds[i,]<- gsub(4,'Favorable',ds[i,])
    ds[i,]<- gsub(5,'Favorable',ds[i,])
  }
  return(ds)
}
generate_scores_per_question<-function(df){
  add_score<-matrix(0,nrow=1,ncol=ncol(df))
  rownames(add_score)<-'overall_question_score'
  colnames(add_score)<- colnames(df)
  df_scored<-rbind(df, add_score)
  for (p in 1:ncol(df_scored)){
    df_scored['overall_question_score',p]=round(sum(as.numeric(df_scored[,p]),na.rm=TRUE),digits=0)
  }
  
  return(df_scored)
}
summarize_score<-function(df,order_type){
  t<-convert_scores(t(df))
  total_number_participants<-ncol(t)
  scored_df<-data.frame(rownames(t))
  for (n in 1:nrow(t)){
    favorability_counter<-0
    #s<-calculate_favorability2(t[n,])
    for(m in 1:ncol(t)){
      #temp<-calculate_favorability2(t_converted[n,])
      if(t[n,m]=='Favorable'){
        favorability_counter<-favorability_counter + 1
      }
    }
    scored_df[n,'favorability_score']<-favorability_counter
    scored_df[n,'Percent_Favorability']<-100*favorability_counter/total_number_participants
  }

  scored_df2<-cbind(scored_df,questions_table)
  t_asc<-scored_df2[with(scored_df2,order(Percent_Favorability)), ]
  t_desc<-scored_df2[with(scored_df2,order(-Percent_Favorability)), ]
  results<-data.frame()
  if(order_type=='lowest'){
    results<-t_asc %>% slice(1:5)
  }
  if(order_type=='highest'){
    results<-t_desc %>% slice(1:5)
  }
  #results_selected
  results_finalized<-results %>% select(c(Question_number, Percent_Favorability,
                                       Question_category,Question_desc))
  #names(results)<-c('Question','Percent Favorability')
 # results_finalized<-results_selected 
  #  mutate(Question_number=gsub('Question_','',Question_number)) %>% 
   # select(-question)
  #results<-results[c('question_number','overall_question_score', 'Question_category','question_desc')]
  #results$overall_question_score<-round(results$overall_question_score,0)
  #results<-round_number(results,0)
  names(results_finalized)<-c("Question #",'Favorability Percent Score','Category','Question')
  #results_finalized<-results_finalized %>% select(`Question #`,`Favorability Percent Score`,Category,Question)
  
  return(results_finalized)
}
summarize_participation<-function(df){
  total_number_participants<-as.numeric(nrow(df))
  abscence<-0
  for (i in 1:nrow(df)){
    sum_of_row<-sum(as.numeric(df[i,]))
    if(sum_of_row == 0){
      abscence<-abscence+1
    }
  }
  participation_rate<-(1-(abscence/total_number_participants))*100
  abscence_rate<-abscence/total_number_participants*100
  engagement_df<-data.frame(items=c('abscence','Participation'),
                            engagement=c(abscence, (total_number_participants-abscence)),
                            rate=c(abscence_rate,participation_rate))
  deleted<-function(){
    fig <- ggplot(engagement_df, aes(
      x = 2,
      y = Count,xmin=3,xmax=4,
      fill=items
    )) + geom_bar(stat = 'identity',
                  color = 'white') + coord_polar(theta = 'y',
                                                 start = 0) + geom_text(aes(label = engagement), color = "white") +
      #scale_fill_manual(values = mycols) +
      theme_void() +
      theme(legend.position = 'none')+
      xlim(0.5, 4)
    fig
  }
  fig<-plot_ly(engagement_df, labels = ~items, values = ~engagement, type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               insidetextorientation='radial',
               hoverinfo = 'text',
               text = ~paste(engagement, 'responders'),
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
  fig <- fig %>% layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  fig
  return(fig)
}
calculate_favorability<-function(df){
  favorable_score=0
  neutral_score=0
  non_favorable_score=0
  dont_know_score=0
  
  for (m in 2:nrow(df)){
    for (n in 1:ncol(df)){
      if(df[m,n]==0){
        dont_know_score = dont_know_score + 1
      }
      if(df[m,n] > 0 & df[m,n] < 3 ){
        non_favorable_score=non_favorable_score+1
      }
      if(df[m,n] > 3 ){
        favorable_score = favorable_score + 1
      }
      if(df[m,n]== 3 ){
        neutral_score = neutral_score + 1
      }
    }
  }
  # all datapoints
  summary_df<-data.frame(score = c(favorable_score, neutral_score,non_favorable_score,dont_know_score),
                         description=c('Favorable', 'Neutral', 'Unfavorable', 'DNK'))
  return(summary_df)
}
calculate_favorability2<-function(df){
  favorable_score=0
  neutral_score=0
  non_favorable_score=0
  dont_know_score=0
  description=c('Favorable', 'Neutral', 'Unfavorable','DNK')
  #for(x in description){
    for (m in 1:nrow(df)){
      for (n in 1:ncol(df)){
        if(df[m,n]==description[4]){
          dont_know_score = dont_know_score + 1
        }
        if(df[m,n]==description[3]){
          non_favorable_score=non_favorable_score+1
        }
        if(df[m,n]==description[1]){
          favorable_score = favorable_score + 1
        }
        if(df[m,n]==description[2]){
          neutral_score = neutral_score + 1
        }
      }
    }
   # all datapoints
  summary_df<-data.frame(description,
                         score = c(favorable_score, neutral_score,non_favorable_score,dont_know_score)
                         )
  for (n in 1:nrow(summary_df)){
    summary_df$percent_score[n]<-summary_df$score[n]/sum(summary_df$score)}
  #summary_df
  return(summary_df)
}
summarize_favorability<-function(df){
  summary_df<-calculate_favorability2(convert_scores(df))
  fig <- plot_ly(summary_df, 
                   labels = ~description, 
                   values = ~score,
                   #text=~description, 
                   textfont=list(color='white')
                   ) %>%
                 add_pie(hole=0.6) %>%
      layout(title = "",  showlegend = FALSE,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             annotations=list(x=0, 
                              y=0,
                              xref='x',
                              yref='y',
                              text=paste0(round((100*summary_df[summary_df$description=='Favorable','percent_score']),digits=0),'%'),
                              showarrow=F,
             font=list(size=50, 
                       color=color_palette2[1])))
    
  #fig
 # return(fig)
  #}
  deleted<-function(){
  summary_df$ymax <- cumsum(summary_df$score)
  summary_df$ymin <- c(0, head(summary_df$ymax, n=-1))
  summary_df$labelPosition <- (summary_df$ymax + summary_df$ymin)/2
  #summary_df$label <- paste0(round(summary_df$score/sum(summary_df$score)*100,digits=0), "%")
  #summary_df$label <- paste(summary_df$description,'\n ',round(summary_df$score/sum(summary_df$score)*100,digits=0))
  summary_df$label <- paste(summary_df$score) 
  labelmax <- paste0(round((max(summary_df$score)/sum(summary_df$score)*100),digits=0), "% \n FAVORABLE")
  # Compute label position
  g<-ggplot(summary_df, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = description)) +
    #geom_bar(stat = "identity", color = "white") +
    geom_rect()+
    coord_polar(theta = "y")+
    geom_text(aes(label=label,
              x=3.5, 
              y=labelPosition),
              #color='white',
              color='red',
              size=5
              )+
    scale_fill_manual(values = color_palette) +
    scale_y_discrete()+
    theme_void() +  
    labs(title='')+
    theme(legend.position = "right"
          #legend.title = element_text('Responses'),
          )+
    xlim(c(0, 4))+
    annotate(geom = 'text', x = 0, y = 0, size=8,color=color_palette[1],label=labelmax)
    g  
  }
  #fig<-plotly1()
  return(fig)
}

## Profile analysis ----
analyze_participant<-function(df,participant_name){
  participant_data<-list()
  #participant_name<-"Chantelle Fane"
  # data table for one participant
  ds<-df[1,]
  new_df<-convert_scores(data.frame(ds))
  participant_table_results<-cbind(questions_table, new_df)
  counter=1
  participant_data[[counter]]<- participant_table_results
  category_list<-unique(participant_table_results$Question_category)
  for (x in length(category_list))
  subset_data<-subset(participant_table_results, Question_category == x)
    data_items <-subset_data %>% gather(key=question,
                                        value=score) %>% mutate(score= factor(score),
                                                                    question=factor(question)) 
  g<-ggplot(data_items, aes(x= question)) + 
    geom_bar(aes(fill = score),
             position = 'fill') + coord_flip()
  g
  participant_data[[1]]<-g
  return(participant_data)
  
}
participant_overall_favorability<-function(df,participant_name){
  ds<-df[participant_name,]
  ds<-data.frame(ds)
  ds_converted<-convert_scores(ds)
  data<-count(ds_converted,vars=ds)
  data$fraction<-numeric(3)
  data$labelPosition<-numeric(3)
  data$label<-numeric(3)
  data$ymin<-numeric(3)
  data$ymax<-numeric(3)
  for (i in 1:nrow(data)){
    data$fraction[i] <- data$n[i]/sum(data$n)*100
   
  }
  data$ymax <- cumsum(data$fraction)
  data$ymin <- c(0, head(data$ymax, n=-1))
  data$labelPosition <- (data$ymax + data$ymin) / 2
  data$label <- paste0(data$vars, "\n value: ", round(data$fraction,0),'%')
  # Compute label position
   
  fig <- ggplot(data,aes( ymax=ymax, ymin=ymin, xmax=4, xmin=3,fill=vars))+                  geom_rect() + 
    geom_rect(colour='white') +
    coord_polar(theta = 'y') + ext()
    geom_text(x=2, aes(label = label,y=labelPosition),size=4)+
    scale_fill_manual(values = color_palette) +
    #scale_color_brewer(palette=3)+
    scale_y_discrete()+
    theme_void() +  
    theme(legend.position = "none")+
   xlim(c(-1, 4))
  fig
  return(fig)
}
participant_category_analysis<-function(df,participant_name){
  ds<-df[participant_name,]
  ds<-data.frame(ds)
  ds_converted<-convert_scores(ds)
  merged_df<-cbind(questions_table, ds_converted)
  category_df<-cbind(merged_df$Question_category,merged_df$ds)
  category_df<-data.frame(category_df)
  names(category_df)<-c('category','favorability')
  favorability_list<-unique(category_df$favorability)
  #questions_category<-unique(questions_table$Question_category)
  category_df$score<-1
  ds1<-category_df %>%  filter(favorability=='Favorable')  %>% select(-favorability) %>%  group_by(category) %>% summarise(favorable=sum(score),.groups='keep')
  ds2<-category_df %>% filter(favorability=='Neutral')  %>% select(-favorability) %>%  group_by(category)  %>% summarise(neutral=sum(score),.groups='keep')
  ds3<-category_df %>% filter(favorability=='Unfavorable')   %>% select(-favorability) %>%  group_by(category) %>% summarise(not_favorable=sum(score),.groups='keep')
  ds4<-category_df %>% filter(favorability=='DNK')  %>% select(-favorability) %>%  group_by(category) %>% summarise(dnk=sum(score),.groups='keep')
  ds_total<-category_df   %>% select(-favorability) %>%  group_by(category)%>% summarise(total=sum(score),.groups='keep')
  
  ds_merged<-merge(ds1,ds2,by = 'category',all = TRUE)
  ds_merged<-merge(ds_merged,ds3,by = 'category',all = TRUE)
  ds_merged<-merge(ds_merged,ds4,by = 'category',all = TRUE)
  ds_merged<-merge(ds_merged,ds_total,by = 'category',all = TRUE)
  
  ds_merged$percentage<-ds_merged$favorable/ds_merged$total*100
  ds_merged<-ds_merged %>% dplyr::arrange(desc(percentage)) 
  ds_merged[is.na(ds_merged)] <- 0
  data <- data.frame(lapply(ds_merged, function(x){ if(is.numeric(x)){round(x, 0)}else{x}}))    
return(data)
  
}
participant_category_analysis_chart<-function(df,participant_name){
  ds_merged<-participant_category_analysis(df,participant_name)
  library(forcats)
  data<-ds_merged %>% 
    dplyr::arrange(desc(favorable))
  g<-data %>% ggplot(aes(x=category, y=percentage))+
    #geom_bar(stat="identity", width = 0.5) +
    geom_segment( aes(xend=category, yend=0)) +
    geom_point( size=4, color=color_palette[1]) +
    xlab("")+
    coord_flip()+
    scale_color_manual(values=color_palette)+
    labs(title='Favorability Score per Question Category',
        caption=participant_name)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = color_palette[5]),
          plot.title=element_text(size=18, 
                                  face="bold", 
                                  family="Arial",
                                  color=color_palette[1],
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # X axis title
          axis.title.y=element_text(size=14,family="Arial",color=color_palette[1]),  # Y axis title
          axis.text.x=element_text(size=15,
                                   family="Arial",
                                   color=color_palette[3],
                                   vjust=.5),  # X axis text
          axis.text.y=element_text(size=12,
                                   family="Arial",
                                   color=color_palette[1]))
  g
  
  return(g)
  
}
participant_detailed_favorability<-function(df,participant_name){
  ds<-df[participant_name,]
  ds<-data.frame(ds)
  # ds_converted<-convert_scores(ds) 
  merged_df<-cbind(questions_table, ds)
  data<-merged_df %>% dplyr::arrange(desc(ds))
  #data <-merged_df %>% select()
  g<-data  %>%  ggplot( aes(x=question, y=ds)) +
    geom_bar(stat="identity") +
    coord_flip() +
    xlab("")
  g
  return(g)
}
department_list<-na.omit(dplyr::distinct(data.frame(profile_revised),Department))
department_list<-department_list[,1]
department_category_analysis_chart<-function(df,department_type){
  library(forcats)
  #data<-plyr::arrange(df,desc(score))
  data<-df[order(df[,'score'],decreasing=TRUE),]
  data$category<-factor(data$category,levels=data$category)
  g<-data %>% ggplot(aes(x=category, y=score,label=score))+
    geom_segment( aes(xend=category, yend=0),size=2,color=color_palette[1]) +
    geom_point( size=9, color=color_palette[1]) +
    scale_x_discrete()+
    #annotate('text',y=data$score, x=data$category,label=data$score, hjust = 2, colour = "purple")+
    geom_text(color='white',size=5)+
    xlab("")+
    coord_flip()+
    scale_color_manual(values=color_palette)+
    labs(title='Favorability Score per Question Category',
         caption=department_type)+
    theme(legend.position = 'none',
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.background = element_rect(fill = color_palette[5]),
          plot.title=element_text(size=18, 
                                  face="bold", 
                                  family="Arial",
                                  color=color_palette[1],
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # X axis title
          axis.title.y=element_text(size=14,family="Arial",color=color_palette[1]),  # Y axis title
          axis.text.x=element_text(size=15,
                                   family="Arial",
                                   color=color_palette[3],
                                   vjust=.5),  # X axis text
          axis.text.y=element_text(size=12,
                                   family="Arial",
                                   color=color_palette[3]))
  g
  
  return(g)
  
}
department_analysis<-function(df,department_type,profile_revised){
  participants<-c()
  participants_list<-rownames(profile_revised)
  temp_df<-data.frame(profile_revised)
  for (m in 1:nrow(temp_df)){
    if(!is.na(temp_df[m,'Department'])){
      if(temp_df$Department[m]== department_type){
        #if(temp_df[m,'Department'] == ){
        participants<-c(participants,participants_list[m])
      }
    }
  }
  temp_df2<-data.frame()
  #df<-t(df)
    for(x in 1:length(participants)){
        temp_df2<-rbind(temp_df2, 
                    formatted_results[participants[x],])
    }
  temp_df3<-t(temp_df2)
  temp_df4<-convert_scores(temp_df3)
  temp_df5<-cbind(questions_table$Question_category,
                  temp_df4)
  calculate_favorability2(temp_df5)
  category_list<-unique(temp_df5[,1])
  temp_df8<-data.frame(category=NA,score=NA)
  for (p in 1:length(category_list)){
    temp_df6<-subset(temp_df5,temp_df5[,1] == category_list[p])
    temp_df7<-calculate_favorability2(temp_df6)
    temp_df8[p,1]<-category_list[p]
    temp_df8[p,2]<-temp_df7[1,'score']
  }
  names(temp_df8)<-c('category','score')
  p<-department_category_analysis_chart(temp_df8,department_type)
  return(p)
}

## Detailed analysis ----
generate_Question_category_plots<-function(df,question_type){
  list_of_graphs<-list()  
  fav_labels<-c('Favourable', 'Neutral', 'Not Favourable', 'Does Not Know')
  data<-data.frame(t(df))
  #data<-data.frame(t(convert_scores(df)))
  data<-cbind(questions_table$Question_category,data)
  names(data)[1]<-'Question_category'
  #names(data)[2]<-'question'
  data_filtered<-data %>% filter(Question_category==question_type) %>%
    select(-Question_category)
  
  for (x in 1:nrow(data_filtered)){
    individual_matrix<-data_filtered[x,]
    individual_matrix<-data.frame(t(individual_matrix)) 
    question_number<-names(individual_matrix)[1]
    question_selected<-filter(questions_table,question==question_number)
    merged_question<-paste(question_selected$question,question_selected$question_desc,sep=' | ')
    names(individual_matrix)[1]<-'question'
    labels<-unique(individual_matrix$question)
    data<-individual_matrix %>% 
      dplyr::group_by(question) %>%
      summarise(count=n()) %>% 
      mutate(percentage=count/sum(count))
    g<-ggplot(data, aes(x=factor(question),
                                     y=percentage*100,
                                     fill=factor(question))
              ) + geom_bar(width = 0.5, stat='identity') +
      theme(legend.position = 'none',
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = color_palette[5]),
            plot.title=element_text(size=16, 
                                    face="bold", 
                                    family="Arial",
                                    color=color_palette[1],
                                    hjust=0.5,
                                    lineheight=1.2),  # title
            axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # X axis title
            axis.title.y=element_text(size=12,family="Arial",color=color_palette[1]),  # Y axis title
            axis.text.x=element_text(size=10,
                                     family="Arial",
                                     color=color_palette[3],
                                     vjust=.5),  # X axis text
            axis.text.y=element_text(size=10,
                                     family="Arial",
                                     color=color_palette[1])
            )+
      coord_flip()+
      scale_fill_manual(values=color_palette,aesthetics = c("colour", "fill"))+
      labs(title=strwrap("Score Distribution"), x="Response", y="Percentage", subtitle =Question_number, caption = wrapper(merged_question))
    g
    list_of_graphs[[x]]<-g
  }
  return(patchwork::wrap_plots(list_of_graphs,ncol=3))
}
generate_Question_category_plots2<-function(df,question_type){
  #df<-formatted_results
  data<-data.frame(t(df))
  data<-cbind(questions_table$Question_category,data)
  names(data)[1]<-'Question_category'
  data_filtered<-data %>% filter(Question_category==question_type) %>%
    select(-Question_category)
  data_converted<-convert_scores(data_filtered)
  new_df<-data.frame()
  for (x in 1:nrow(data_filtered)){
    data_calculated<-calculate_favorability2(data_converted[x,])
    data_calculated$question<-rownames(data_filtered)[x]
    data_calculated$percent_score<-data_calculated$percent_score*100
    data_calculated<- data_calculated %>% mutate(lab_ypos = cumsum(percent_score) + 0.5 * percent_score) 
    new_df<-rbind(new_df,data_calculated)
    
  }
  
  new_df=new_df %>% arrange(desc(question))
 # questions<-unique(new_df$question)
  #new_df$question<-factor(new_df$question, levels=sort(new_df$question, decreasing = TRUE))
  g<-ggplot(new_df, aes(x= question,y=percent_score,fill=factor(description,
                                                                        levels=c('DNK',
                                                                                 'Unfavorable',
                                                                                 'Neutral',
                                                                                 'Favorable'
                                                                                 )))) + 
    geom_bar(width = 0.6, stat='identity',position='fill') +
    theme_void()+ 
    #scale_x_discrete(order(new_df$question,desc=TRUE))+
    scale_x_discrete(sort(new_df$question, decreasing=TRUE))+
    scale_y_continuous()+
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          #legend.text = element_text(sort)
          axis.text.y=element_text())+
      coord_flip()+
      #scale_fill_manual(values=color_palette)+
      labs(title='PERCENT SCORE DISTRIBUTION',caption =question_type)+
      geom_label(aes(x=question, y=lab_ypos,label=percent_score),position='fill',color='white')
  g
  
  return(g)
}
get_detailed_analysis<-function(df,question_type){
  fav_labels<-c('Favourable', 'Neutral', 'Not Favourable', 'Does Not Know')
  y_axis_lablel<-questions_table$merged_question

  if(question_type=='All Questions'){
    data<-data.frame(df)
    data_items <-data %>% gather(key=question,value=score) %>% mutate(score= factor(score),
               question=factor(question)) 
    data_items2<-data_items %>% dplyr::count(question,score) %>%
      mutate(y_pos = cumsum(n)/nrow(data) - (0.5 * n/nrow(data)),
             y_cumsum = cumsum(n)) %>% 
      mutate(items_num = question)
    data_merged<-merge(data_items, questions_table, by='question',keep_all=TRUE)
    g<-ggplot(data_merged, aes(x= merged_question)) + 
      geom_bar(aes(fill = score),
               #position = position_stack(reverse = TRUE))
               position = 'fill')+
      coord_flip()+
      scale_fill_manual(values=color_palette)+
      labs(title="DISTRIBUTION OF FAVORABILITY SCORE",y="Percentage") +
      theme(legend.position='bottom', 
            plot.title=element_text(size=16, 
                                    face="bold", 
                                    family="Arial",
                                    color=color_palette[1],
                                    hjust=0.5,
                                    lineheight=1.2),  # title
            axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # X axis title
            axis.title.y=element_blank(),
            #axis.title.y=element_text(size=10,family="Arial",color=color_palette[1]),  # Y axis title
            #axis.title.y=element_text(size=10,family="Arial",color=color_palette[1]),  # Y axis title
            axis.text.x=element_text(size=10,
                                     family="Arial",
                                     color=color_palette[3],
                                     vjust=.5),  # X axis text
            axis.text.y=element_text(size=11,
                                     family="Arial",
                                     color=color_palette[1]),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            #panel.border = element_rect(fill = "white"),
            axis.ticks = element_blank()
    )
    g
    #list_of_graphs[[1]]<-g
  }
  else{
    g<-generate_Question_category_plots(df,question_type)
  }
  return(g)            
}
generate_barplot<-function(plot,questions_category){
  filename<-paste0('barplots/',questions_category,'.jpg')
  ggsave(plot,file=filename,width=4,height=1)
}
make_reactable_df2<-function(data){
  reactable(data,
            columns=list(
             Response = colDef(cell=function(values){
                sparkline(values,type='bar', 
                          stackedBarColor=color_palette,
                          #width = 200,
                          height=20,
                          zeroAxis=FALSE,
                          chartRangeMin = 0, 
                          chartRangeMax = 100,
                          barColor: '#3366cc',
                          
                          
                          )
              })
            ))
}
make_reactable_df<-function(df,participants_list){
  library(htmltools)
  bar_chart <- function(label, width = "100%", height = "16px", fill = color_palette[1], background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
  }
  df<-df %>% select(-Response)
  g<-reactable(df, 
               defaultSorted = "Favorable",
               columns = list(
                 Favorable= colDef(name = "fav score", 
                                   align = "left", 
                                   defaultSortOrder = "desc",
                                   cell = function(value) {
                                     width <- paste0(value*100/(length(participants_list)*5), "%")
                                     value <- format(value, width = 9, justify = "right")
                                     bar_chart(value, width = width)
                                   },
                                   style = list(fontFamily = "monospace", whiteSpace = "pre")), 
                 Not_Favorable= colDef(name = "Unfavorable", 
                                       align = "right", 
                                       defaultSortOrder = "desc",
                                       cell = function(value) {
                                         width <- paste0(value, "%")
                                         bar_chart(value, width = width,fill=color_palette[2])
                                       },
                                       style = list(fontFamily = "monospace", whiteSpace = "pre")) ))
  g
  return(g)
}
generate_subset_df<-function(df,category){
  questions_by_category<-data.frame()
  subset_df<-data.frame() #subset data 
  data<-data.frame(t(convert_scores(df)))
  data<-cbind(questions_table$Question_category,
              questions_table$question_desc,
              data)
  names(data)[1]<-'Question_category'
  names(data)[2]<-'question'
  data_converted<-convert_scores(data)
  data_filtered<-data_converted %>% filter(Question_category==category)
  subset_df<-data_filtered %>% select(-Question_category)
  subset_summary<-calculate_favorability2(subset_df)
  df1<-data.frame()
  for (x in 1:nrow(subset_df)){
    one_row<-calculate_favorability2(subset_df[x,])
    one_row$question<-subset_df[x,'question']
    df1<-rbind(df1,one_row)
  }
  df1$percent_score<-df1$percent_score*100
  df2<-reshape2::dcast(df1,question~description, value.var = 'percent_score')
  df3<-df2 %>% select(question,Favorable,Unfavorable)
  return(df3)
}
make_reactable_df3<-function(df,participants_list){
  library(htmltools)
  bar_chart <- function(label, width = "100%", height = "16px", fill = color_palette[1], background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
  }
  df<-df %>% select(-Response)
  #df2<-df1 %>% arrange(desc(Favorable))
  g<-reactable(df,
               showSortIcon = TRUE,
               details=function(index){
                 htmltools::div(
                   "Questions Breakdown (%)",
                   htmltools::tags$pre(paste(capture.output(print(generate_subset_df(formatted_results,df$Category[index]))), collapse = "\n")), 
                   align='justified' 
                   #wrap = TRUE,resizable=TRUE,fullWidth = TRUE)
                   #paste(capture.output(print(generate_subset_df(formatted_results,df$Category[index]))), collapse = "\n"), 
                 )
                 
               },
               defaultSorted = "Favorable",
               columns = list(
                 Favorable= colDef(name = "Percent Favorability", 
                                   align = "left", 
                                   defaultSortOrder = "desc",
                                   cell = function(value) {
                                     width <- paste0(value*100/(length(participants_list)*5), "%")
                                     value <- format(value, width = 9, justify = "right")
                                     bar_chart(value, width = width)
                                   },
                                   style = list(fontFamily = "monospace", whiteSpace = "pre")), 
                 Not_Favorable= colDef(name = "Unfavorable", 
                                       align = "left", 
                                       defaultSortOrder = "desc",
                                       cell = function(value) {
                                         width <- paste0(value, "%")
                                         value <- format(value, width = 9, justify = "right")
                                         bar_chart(value, width = width,fill=color_palette[2])
                                       },
                                       style = list(fontFamily = "monospace", whiteSpace = "pre")) ))
  g
  return(g)
}
make_reactable_df4<-function(df,participants_list){
  library(htmltools)
  bar_chart <- function(label, width = "100%", height = "16px", fill = color_palette[1], background = NULL) {
    bar <- div(style = list(background = fill, width = width, height = height))
    chart <- div(style = list(flexGrow = 1, marginLeft = "8px", background = background), bar)
    div(style = list(display = "flex", alignItems = "center"), label, chart)
  }
  df<-df %>% select(-Response)
  g<-reactable(df,
               showSortIcon = TRUE,
               details=colDef(
                 details = function(index){
                   htmltools::div(
                     "Questions Breakdown (%)",
                     htmltools::tags$pre(paste(capture.output(print(generate_subset_df(formatted_results,df$Category[index]))), collapse = "\n"))
                   )
                    
                                },
                 resizable=TRUE, align='left'
                 
               ),
               defaultSorted = "Favorable",
               columns = list(
                 Favorable= colDef(name = "Percent Favorability", 
                                   align = "left", 
                                   defaultSortOrder = "desc",
                                   cell = function(value) {
                                     width <- paste0(value*100/(length(participants_list)*5), "%")
                                     value <- format(value, width = 9, justify = "right")
                                     bar_chart(value, width = width)
                                   },
                                   style = list(fontFamily = "monospace", whiteSpace = "pre")), 
                 Not_Favorable= colDef(name = "Unfavorable", 
                                       align = "left", 
                                       defaultSortOrder = "desc",
                                       cell = function(value) {
                                         width <- paste0(value, "%")
                                         value <- format(value, width = 9, justify = "right")
                                         bar_chart(value, width = width,fill=color_palette[2])
                                       },
                                       style = list(fontFamily = "monospace", whiteSpace = "pre")) ))
  g
  return(g)
}
deleted<-function(){
  data_items <-data %>% gather(key=question,value=score) %>% mutate(score= factor(score),
                                                                    question=factor(question)) 
  data_items2<-data_items %>% dplyr::count(question,score) %>%
    mutate(y_pos = cumsum(n)/nrow(data) - (0.5 * n/nrow(data)),
           y_cumsum = cumsum(n)) %>% 
    mutate(items_num = question)
  data_merged<-merge(data_items, questions_table, by='question',keep_all=TRUE)
  
  g<-ggplot(data_merged, aes(x= merged_question)) + 
    geom_bar(aes(fill = score),
             #position = position_stack(reverse = TRUE))
             position = 'fill')+
    coord_flip()+
    scale_fill_manual(values=color_palette)+
    labs(title="DISTRIBUTION OF FAVORABILITY SCORE",y="Percentage") +
    theme(legend.position='bottom', 
          plot.title=element_text(size=16, 
                                  face="bold", 
                                  family="Arial",
                                  color=color_palette[1],
                                  hjust=0.5,
                                  lineheight=1.2),  # title
          axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # X axis title
          axis.title.y=element_blank(),
          #axis.title.y=element_text(size=10,family="Arial",color=color_palette[1]),  # Y axis title
          #axis.title.y=element_text(size=10,family="Arial",color=color_palette[1]),  # Y axis title
          axis.text.x=element_text(size=10,
                                   family="Arial",
                                   color=color_palette[3],
                                   vjust=.5),  # X axis text
          axis.text.y=element_text(size=11,
                                   family="Arial",
                                   color=color_palette[1]),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          #panel.border = element_rect(fill = "white"),
          axis.ticks = element_blank()
    )
  g
  g<-ggplot(summary_df,aes(x=score,y=description,fill=description))+
    geom_bar(position = "fill",stat = "identity", width=1) + 
    coord_flip() +
    scale_fill_manual(values=color_palette)+
    theme_void()+
    #geom_text(aes(label=Question))+
    theme(legend.position = 'none')
  #g
  generate_barplot(g,cat[q,1])
  #list_of_graphs[[1]]<-g
}
get_detailed_analysis_2<-function(df,question_type){
  fav_labels<-c('Favourable', 'Neutral', 'Unfavorable', 'DNK')
  y_axis_lablel<-questions_table$merged_question
  num_participants<-nrow(df)
  g<-list()
  if(question_type=='All Questions'){
    data<-data.frame(t(convert_scores(df)))
    data<-cbind(questions_table$Question_category,
                #questions_table$question_desc,
                data)
    names(data)[1]<-'Question_category'
    cat<-data %>% select(Question_category) %>% distinct(Question_category)
    new_df<-data.frame(Category=NA, Response=NA,Favorable=NA, Neutral=NA,
                       Not_Favorable=NA, DNK=NA)
    subset_df<-data.frame() #subset data 
    for (q in 1:nrow(cat)){
      temp_df<-data.frame(Category=NA, Response=NA,Favorable=NA, Neutral=NA,
                          Not_Favorable=NA, DNK=NA)
      data_filtered<-data %>% filter(Question_category==cat[q,1]) %>%
        select(-Question_category)
      total<-num_participants*nrow(data_filtered)
      summary_df<-calculate_favorability2(data_filtered)
            temp_df[q,'Category']<-cat[q,1]
      temp_df[q,'Favorable']<-round(summary_df$score[1]/total*100,digits=1)
      temp_df[q,'Neutral']<-round(summary_df$score[2]/total*100,digits=1)
      temp_df[q,'Not_Favorable']<-round(summary_df$score[3]/total*100,digits=1)
      temp_df[q,'DNK']<-round(summary_df$score[4]/total*100,digits=1)
      temp_df[q,'Response'][[1]]<-list(list(summary_df$percent_score))
      new_df<-rbind(new_df,temp_df)
      data_filtered<-data %>% filter(Question_category==cat[q,1]) %>%
        select(-Question_category)
      data_converted<-convert_scores(data_filtered)
      }
    
    new_df2<-distinct(new_df)
    new_df3<-new_df2[2:nrow(new_df2),]
   #g<-make_reactable_df(new_df3,participants_list)
    g<-make_reactable_df3(new_df3,participants_list = rownames(formatted_results))
    g
    }
  else{
    g<-generate_Question_category_plots2(df,question_type)
  }
  return(g)
  
}
#g<-get_detailed_analysis_2(formatted_results,question_type)
get_questions<-function(question_type){
  #df<-formatted_results
  data<-questions_table %>% 
    filter(Question_category==question_type) %>%
    arrange(desc(question)) %>%
    mutate(merged_question2=paste(question,question_desc,sep = ' | ')) %>%
    select(merged_question2) 
    #colnames(data)[1]
return(data)
  }
make_barplot<-function(data){
  data=data.frame(Question=c('question','question','question','question') ,
                  description=fav_labels,
                  score=c(72,6,8,16))
  data=data %>% mutate(lab_ypos = cumsum(score) - 0.5 * score) 
  g<-ggplot(data,aes(x=Question, y=score,fill=description))+
    geom_bar(position='fill',stat='identity')+
    geom_text(data,aes(y=lab_ypos,label=score,group=description),
              #color='black',
              hjust=0.5)+
    #geom_text(aes(y=lab_ypos,label=score))+ 
    coord_flip()+
    theme_void()+
    theme(legend.position = 'none')+
    #scale_y_continuous()+
    scale_fill_manual(values=color_palette)
    #annotate(g,x='Favorable',y=score)
    #geom_text(aes(x=Question, y=score,label=score))
  g
  df1<-as.matrix(data)
  p<-barplot(height = df1,
             horiz = TRUE, 
             names=description)
  text(labels=score)
  p
return(g)
}
## demographic analysis ----
add_overall_score<-function(columnnames,summary_matrix) {
  add_score<-matrix(0, nrow=1,ncol=length(columnnames))
  colnames(add_score)<-columnnames
  rownames(add_score)<-'Overall Score'
  for ( s in columnnames){
    add_score['Overall Score',s]<-sum(summary_matrix[,s])
  }
  summary_ds<-rbind(add_score, summary_matrix)
  return(summary_ds)
}
generate_demographics_ds<-function(profile_dataset, formatted_results){
  #columnnames<-colnames(profile_dataset)
  #columnnames<-columnnames[columnnames!='Participants']
  analysis<-matrix(0,ncol=length(categories_desc),nrow=nrow(questions_table))
  colnames(analysis)<-categories_desc
  rownames(analysis)<-questions_table$Question_number
  for (x in categories_desc){
    temp_df<- profile_dataset %>%  select('Participants',all_of(x)) 
    temp_df[,x]<-as.numeric(temp_df[,x])
    temp_df<-filter(temp_df, temp_df[,x]==1)
    temp_names<-temp_df['Participants']
    if(nrow(temp_names)==0){
    ##  print('no results')
      analysis[r,x]<-0
    }
    else {
      for (r in rownames(analysis)){
        ave_per_question<-0
        sum_per_question<-0
        for(p in (temp_names[,'Participants'])){
          sum_per_question<-sum_per_question + as.numeric(formatted_results[p,r])
        }
        ave_per_question<-round(sum_per_question/nrow(temp_names),3)
        analysis[r,x]<-ave_per_question
      }
    }
    
  }
  #analysis_reordered<-analysis[order(row.names(analysis)),]
  return(analysis)
}
generate_demographics_ds_scored<-function(analysis){
  columnnames<-colnames(analysis)
  summary_ds<-add_overall_score(columnnames,analysis)
  for (k in colnames(summary_ds)){
    for (j in rownames(summary_ds)){
      summary_ds[j,k]<-round_number(summary_ds[j,k],1)
    }
  }
  return(summary_ds)
}
round_number<- function(x,max_digits){
    if(is.numeric(x)){
      if(is.integer(x)){
        x
      }
      else{
        x<-round(x,digits=max_digits)
      }
    }
    else {x}
    return(x)
    }
generate_summary_demographics_ds<-function(ds){
  columnnames<-colnames(ds)
  summary_matrix<-matrix(0,ncol=length(columnnames),nrow=length(questions_category))
  colnames(summary_matrix)<-columnnames
  rownames(summary_matrix)<-sort(questions_category)
  for(col in columnnames){
    for(q in questions_category){
      get_questions_number<-c()
      for (x in 1:nrow(questions_table)){
        if (questions_table[x,'Question_category']==q){
          get_questions_number<-c(get_questions_number,questions_table[x,'Question_number'])
        }
      }
      questions_category_score<-0
      for (r in get_questions_number){
        questions_category_score<-questions_category_score+ds[r,col]
      }
      summary_matrix[q,col]<-questions_category_score
    }
  }
  return(summary_matrix)
}
generate_summary_demographics_scored<-function(summary_matrix){
  columnnames<-colnames(summary_matrix)
  summary_ds<-add_overall_score(columnnames, summary_matrix)
  return(summary_ds)
  }
analyze_demographics<-function(demographics_table, attribute,query_type){
    df1<-round_number(demographics_table,1)
    get_column_name<-c()
    temp_df<-matrix(0,nrow=nrow(df1),ncol=1)
    rownames(temp_df)<-rownames(df1)
    categories_df2<-cbind(categories_df,names(profile_dataset)[2:length(profile_dataset)])
    names(categories_df2)<-c('category','category_desc','category_desc2')
    for (x in 1:nrow(categories_df2)){
      if(attribute == categories_df[x,'category']){
        get_column_name<-c(get_column_name,categories_df2[x,'category_desc2'])
      }
    }
    for (y in 1:length(get_column_name)){
      temp_df<-cbind(temp_df,df1[,get_column_name[y]])
    }
    if(ncol(temp_df)>1){temp_df<-temp_df[,2:ncol(temp_df)]}
    colnames(temp_df)<-get_column_name
    temp_df<-data.frame(temp_df)
    temp_df<-tibble::rownames_to_column(temp_df)
    plot_list<-list()
    for(n in 2:ncol(temp_df)){
      #df<-data.frame(temp_df[,n])
      df<-cbind(temp_df[,1],
                temp_df[,n])
      parameter<-colnames(temp_df)[n]
      #rownames(df)<-rownames(temp_df)
      colnames(df)[2]<-'parameter'
      df2<-data.frame()
      # library(tibble)
      #df2<-tibble::rownames_to_column(df)
      if(query_type == 'question'){
        df2<-cbind(questions_table$merged_question,df)
        colnames(df2)[1]<-'question'
        }
      if(query_type == 'category'){
        df2<-df
        # #df2<-cbind(questions_table$question,
        #            questions_table$Question_category,df)
        colnames(df2)[1]<-'question'
        # names(df2)[1]<-'Question_category'
      }
     df3<-data.frame(df2)
     df3$parameter<-as.numeric(df3$parameter)
      #df4<-df3[order('parameter',decreasing = TRUE),]
     df4<-df3[with(df3,order(parameter,decreasing = TRUE)),]
     #df4<-df3 %>% dplyr::arrange(desc(parameter))
     df4$question<-factor( df4$question,levels= df4$question)
        g<-ggplot(df4,aes(x=question,y=parameter,label=parameter)) +
       geom_segment(aes(yend=0,xend=question),
                    size=2,color=color_palette[1]) +   
       #geom_segment(aes(yend=parameter,xend=question,x=question,y=0)) +   
       geom_point( size=12, color=color_palette[1])+
          coord_flip()+
       #scale_x_discrete()+
          geom_text(color='white',size=5)+
          labs(title = paste(parameter),
                 y='Average Score',
                 caption = attribute) + 
          theme(legend.position = 'none',
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_rect(fill = color_palette[5]),
                plot.title=element_text(size=18, 
                                        face="bold", 
                                        family="Arial",
                                        color=color_palette[1],
                                        hjust=0.5,
                                        lineheight=1.2),  # title
                axis.title.y=element_blank(),  # X axis title
                axis.title.x=element_text(size=12,family="Arial",color=color_palette[1]),  # Y axis title
                axis.text.x=element_text(size=10,
                                         family="Arial",
                                         color=color_palette[3],
                                         vjust=.5),  # X axis text
                axis.text.y=element_text(size=15,
                                         family="Arial",
                                         lineheight = 40,
                                         hjust = 1,
                                         color=color_palette[1]),
                axis.ticks.y = element_blank())+
          scale_x_discrete()
          g
        plot_list[[n-1]]<-g
    }
    plot_length<-length(plot_list)
    multi_plot<-patchwork::wrap_plots(plot_list,nrow=plot_length,ncol=1)
    multi_plot
    return(multi_plot)
}
#p<-analyze_demographics(summary_demographics_table,'Department','category')


demographics_participant_stats<-function(profile_revised,attribute){
  number_of_elements<-ncol(profile_revised)
  #data<-data.frame(profile_revised)
  #plots_list<-list()
  #fig <- plot_ly()
  #for (i in 1:number_of_elements){
   # subset_data<-data.frame(attribute<-data[,i])
    subset_data<-data.frame(profile_revised[,attribute])
    names(subset_data)<-'attribute'
    summary_subset<-subset_data %>% 
      dplyr::group_by(attribute) %>% 
      summarise(count_attribute = n())
      fig<-plot_ly(summary_subset,
      #fig <- fig %>% add_pie(data=summary_subset, 
                             labels = ~attribute,
                             values = ~count_attribute, 
                     type = 'pie',
                     name=attribute,
                     #domain=list(row=0, column=i),
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     text = ~paste(count_attribute, ' participants'),
                     marker = list(colors = blue_gradient_color_palette,
                                   line = list(color = '#FFFFFF', width = 1)),
      showlegend = FALSE
                     ) 
      #plots_list[[i]] <- fig
 # }
  fig<-fig %>%  layout(
    #title = paste(attribute, 'distribution'),
    title='Demographic Breakdown of Survey',
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  fig
   #fig<-patchwork::wrap_plots(plots_list,ncol=4)
  return(fig)
}
get_number_of_cat_elements<-function(attribute){
  get_column_name<-c()
  categories_df2<-cbind(categories_df,names(profile_dataset)[2:length(profile_dataset)])
  names(categories_df2)<-c('category','category_desc','category_desc2')
  for (x in 1:nrow(categories_df2)){
    if(attribute == categories_df2[x,'category']){
      get_column_name<-c(get_column_name,categories_df2[x,'category_desc2'])
    }
  }
  number_of_elements<-length(get_column_name)
  #plot_height<-500*number_of_elements
  return(number_of_elements)
}

# Test functions

# raw_results<-modify_raw_results(raw_results)
# formatted_results<-generate_formatted_results(raw_results)
# questions_list<-questions_table$question
# questions_category<-unique(questions_table$Question_category)
# results_scored_by_questions<-generate_scores_per_question(formatted_results)
# scored_results<-generate_scored_results(formatted_results)
# merged_df<-generate_merged_dataset(formatted_results,profile_revised)
# merged_df_scored<-generate_merged_dataset(scored_results,profile_revised)
#demographics_table<-generate_demographics_ds(profile_dataset, formatted_results)
#summary_demographics_table<-generate_summary_demographics_ds(demographics_table)
# favorability_plot<-summarize_favorability(formatted_results)
# participlation_plot<-summarize_participation(scored_results)
# ds<-convert_scores(formatted_results)
# detailed_questions<-get_detailed_analysis(ds,'All Questions')


