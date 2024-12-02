



selection_pays_der<-function(data = taux_pauvre,pays="Angola",debut=2010,fin=2020){
  
  taux_pauvre_pays = data  %>% filter(ref_area.label==pays) %>%filter(time %in% debut:fin)
  taux_pauvre_pays_sex  =taux_pauvre_pays[taux_pauvre_pays$classif1.label=='Age (Youth, adults): 15+' & str_ends(taux_pauvre_pays$sex.label,'tal'),c(4,7,6,1)] %>% group_by(time,sex.label) %>% summarise(taux_pauvre=mean(as.double(obs_value),na.rm = TRUE))
  
  
  taux_pauvre_pays_sex = taux_pauvre_pays_sex %>% pivot_wider(values_from = taux_pauvre,names_from = sex.label)
  
  return(taux_pauvre_pays_sex[dim(taux_pauvre_pays_sex)[1],])
  
}


selection_pays<-function(data = taux_pauvre,pays="Angola",debut=2010,fin=2020){
  
  taux_pauvre_pays = data  %>% filter(ref_area.label==pays) %>%filter(time %in% debut:fin)
  taux_pauvre_pays_sex  =taux_pauvre_pays[taux_pauvre_pays$classif1.label=='Age (Youth, adults): 15+' & str_ends(taux_pauvre_pays$sex.label,'ale'),c(4,7,6,1)] %>% group_by(time,sex.label) %>% summarise(taux_pauvre=mean(as.double(obs_value),na.rm = TRUE))
  
  
  taux_pauvre_pays_sex = taux_pauvre_pays_sex %>% pivot_wider(values_from = taux_pauvre,names_from = sex.label)
  
  data <- taux_pauvre_pays_sex %>%
    mutate(Male = -Male)
  
  # Créer le graphique en pyramide avec des étiquettes
  plot_ly(data, x = ~Male, y = ~time, type = 'bar', name = 'Males', orientation = 'h',
          text = ~abs(Male), textposition = 'auto') %>%
    add_trace(x = ~Female, name = 'Females', orientation = 'h',
              text = ~Female, textposition = 'auto') %>%
    layout(
      title = "Distribution du taux de pauvrete par sexe et par annees",
      barmode = 'overlay',
      xaxis = list(title = "Taux"),
      yaxis = list(title = "Annees"),
      showlegend = TRUE,
      xaxis = list(tickvals = seq(-1000, 1000, by = 200), ticktext = abs(seq(-1000, 1000, by = 200)))
    )
}


selection_pays_evol_total<-function(data = taux_pauvre,pays="Angola",debut=2000,fin=2020){
  
  taux_pauvre_pays = data  %>% filter(ref_area.label==pays) %>%filter(time %in% debut:fin)
  taux_pauvre_pays_sex  =taux_pauvre_pays[taux_pauvre_pays$classif1.label=='Age (Youth, adults): 15+' & str_ends(taux_pauvre_pays$sex.label,'tal'),c(4,7,6,1,8)] %>% group_by(time,sex.label) %>% summarise(taux_pauvre=mean(as.double(obs_value),na.rm = TRUE))
  
  
  taux_pauvre_pays_sex = taux_pauvre_pays_sex %>% pivot_wider(values_from = taux_pauvre,names_from = sex.label)
  taux_pauvre_pays_sex['pays'] = pays
  
  return(taux_pauvre_pays_sex)
  
}


selection_pays_evol_sous_region<-function(data = taux_pauvre,region="Afrique Centrale",debut=2000,fin=2020){
  
  taux_pauvre_pays = data  %>% filter(Région==region) %>%filter(time %in% debut:fin)
  taux_pauvre_pays_sex  =taux_pauvre_pays[taux_pauvre_pays$classif1.label=='Age (Youth, adults): 15+' & str_ends(taux_pauvre_pays$sex.label,'tal'),c(4,7,6,1,8)] %>% group_by(time,sex.label) %>% summarise(taux_pauvre=mean(as.double(obs_value),na.rm = TRUE))
  
  
  taux_pauvre_pays_sex = taux_pauvre_pays_sex 
  taux_pauvre_pays_sex['region'] = region
  
  return(taux_pauvre_pays_sex)
  
}


pauvrete_pays_sous_region<-function(pays='Angola'){
  data = selection_pays_evol_total(pays = pays)
  data1 = selection_pays_evol_sous_region(region = as.character(unique(taux_pauvre %>% filter(ref_area.label==pays) %>% select(Région))))
  
  ggplotly(ggplot(data) + geom_point(aes(x=time,y=Total))  + geom_line(aes(x=time,y=Total, group = 1,color=pays)) + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank()) + labs(title = "Evolution du taux de pauvrete",y='Taux de pauvrete',legend='')  + 
             geom_point(data = data1,aes(x=time,y=taux_pauvre))  + geom_line(data = data1,aes(x=time,y=taux_pauvre, group = 1,color=region)) + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank()) + labs(title = "Evolution du taux de pauvrete suivant le pays et la zones du pays",y='Taux de pauvrete',legend=''))
}


chom_annee_sex<-function(pays="Angola"){
  total = Taux_de_chômag %>% filter(ref_area.label==pays,str_starts(as.vector(unlist(as.vector(Taux_de_chômag[,3]))),'SDG indicator 8.5.2'),str_ends(Taux_de_chômag$sex.label,"ale")) %>% select(ref_area.label,sex.label,obs_value,time) %>% group_by(time,sex.label) %>% summarise(obs_value=mean(obs_value)) 
  
  ggplotly(ggplot(total,aes(x=time,y=obs_value,fill=sex.label)) + geom_bar(stat="identity",position = "fill",width = 0.5) + geom_text(aes(label =round(obs_value,2) ), 
                                                                                                                                      position = position_fill(vjust = 0.5),  # Centrer les étiquettes
                                                                                                                                      size = 3.5) +theme(axis.text.x = element_blank(),
                                                                                                                                                         plot.title = element_text(hjust = 0.5),
                                                                                                                                                         axis.ticks.y = element_blank(),) +labs(title = "Répartition du taux de chomage par annees et par sexe", x = "", y = "") +coord_flip() +theme_minimal()) %>% layout(
                                                                                                                                                           legend = list(title="sex",
                                                                                                                                                                         orientation = "h",
                                                                                                                                                                         x = 0.5,
                                                                                                                                                                         xanchor = "center",
                                                                                                                                                                         y = -0.2),xaxis = list(title = "", showticklabels = FALSE)) 
  
  
}

chomage_pays_sous_pay<-function(pays='Angola'){
  total = Taux_de_chômag %>% filter(ref_area.label==pays,str_starts(as.vector(unlist(as.vector(Taux_de_chômag[,3]))),'SDG indicator 8.5.2'),str_ends(Taux_de_chômag$sex.label,"al")) %>% select(ref_area.label,sex.label,obs_value,time) %>% group_by(time) %>% summarise(obs_value=mean(obs_value)) 
  total['pays']=pays
  total
}

chomage_pays_sous_regi<-function(region='Afrique Centrale'){
  total = Taux_de_chômag %>% filter(Région==region,str_starts(as.vector(unlist(as.vector(Taux_de_chômag[,3]))),'SDG indicator 8.5.2'),str_ends(Taux_de_chômag$sex.label,"al")) %>% select(ref_area.label,sex.label,obs_value,time) %>% group_by(time) %>% summarise(obs_value=mean(obs_value)) 
  total['region']=region
  total
}


chomage_vs_region<-function(regio){
  
  data = chomage_pays_sous_pay(regio)
  data1 = chomage_pays_sous_regi(as.character(unique(Taux_de_chômag %>% filter(ref_area.label==regio) %>% select(Région)))) 
  
  
  ggplotly(ggplot(data) + geom_point(aes(x=time,y=obs_value))  + geom_line(aes(x=time,y=obs_value, group = 1,color=pays)) + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank()) + labs(title = "Evolution du taux de chomage du pays par rapport a la zone",y='Taux de chomage',legend='')  + 
             geom_point(data = data1,aes(x=time,y=obs_value))  + geom_line(data = data1,aes(x=time,y=obs_value, group = 1,color=region)) + theme_minimal() +theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank()) +labs(color='legende')) %>% layout(legend = list(title="sex", orientation = "h", x = 0.5, xanchor = "center", y = -0.2)) 
  
}


indicateur_chomage<-function(pays='Angola'){
  s = chomage_pays_sous_pay(pays)
  s[dim(s)[1],c(1,2)]
}


























