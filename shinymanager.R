library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(plotly)
library(shinycssloaders)
library(waiter)
library(bs4Dash)
library(fontawesome)
library(DT)
library(RColorBrewer)
library(forecast)
source('generale.R')
source("them1_securite.R")
source("emploi fornmel.R")

df_=read_excel("base_prevision.xlsx")

data_previ <- list(
  "Taux de chômage" = ts(df_$Taux_chomage, start = 2000),
  "Taux d'emploi" = ts(df_$Taux_emploi, start = 2000))

ui=dashboardPage(
  
  ######
  dashboardHeader(
                  title = dashboardBrand(
                    title = sprintf("IOT"),
                    href = "image2.png",
                    image = "image2.jpg",
                    opacity = 0.8
                  )),
  dashboardSidebar(
    sidebarMenu(
      menuItem("vue global", tabName = "graphs", icon = icon("eye", "fa-fw")),
      menuItem("Vue en details",tabName = 'view', icon = icon("search-plus", "fa-fw"),
               menuSubItem("Sécurité au travail", tabName = "sec_cond", icon = icon("shield-alt", "fa-fw")),
               menuSubItem("Emploi formel ", tabName = "emp_form", icon = icon("hard-hat", "fa-fw")),
               menuSubItem("Emploi informel", tabName = "emp_infor", icon = icon("exclamation-triangle", "fa-fw")),
               menuSubItem("insertion professionnelle", tabName = "form_inser", icon = icon("handshake", "fa-fw"))
               
               ),
      menuItem("Telechargement", tabName = "tel",icon =  icon("download"),
               menuSubItem('base de donnees',tabName = 'Telechargement'),
                menuSubItem("Rapport", tabName = "rapport"))
      ,
      menuItem("À propos", tabName = "apropos", icon = icon("info-circle"),
               menuSubItem("Qui sommes-nous", tabName = "equipe"),
               menuSubItem("Présentation ISSEA", tabName = "issea")
      ),
      menuItem('Prevision',tabName = 'prevision',icon=icon("chart-line") )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content {
          background-image: url('image.jpg');
        }
        .tab-content {
          background: transparent; /* Rend le fond du tabItem transparent */
        }
        .issea-image {
          max-width: 60%;
          height: auto;
          margin: 20px auto;
          display: block;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
         .cover-image {
          width: 100%;          /* Prend toute la largeur disponible */
          height: 200px;        /* Hauteur fixe de 200px */
          margin: 0;            /* Supprime les marges */
          display: block;
          object-fit: cover;    /* Assure que l'image couvre tout l'espace */
          object-position: center; /* Centre l'image */
        }
        .team-member {
          margin-bottom: 25px;
          padding: 15px;
          border-left: 3px solid #3c8dbc;
          display: flex;
          align-items: center;
          background: #f8f9fa;
          border-radius: 5px;
        }
        .member-photo {
          width: 100px;
          height: 100px;
          border-radius: 50%;
          margin-right: 20px;
          object-fit: cover;
          border: 3px solid #3c8dbc;
        }
        .member-info {
          flex-grow: 1;
          font-family: 'Palatino Linotype';
        }
        .member-social {
          margin-top: 10px;
        }
        .member-social a {
          margin-right: 15px;
          color: #3c8dbc;
          text-decoration: none;
        }
        .member-social a:hover {
          color: #367fa9;
        }
        .nationality {
          color: #666;
          font-style: italic;
          margin-top: 5px;
        }
        .issea-content {
          padding: 20px;
        }
        .description {
          font-size: 16px;
          line-height: 1.6;
          color: #2c3e50;
          margin-bottom: 20px;
        }
      "))),
    tabItems(
      ############
      tabItem(tabName = "graphs",
              fluidRow(
                valueBox(round(as.double(d_evol_chom[dim(d_evol_chom)[1],2]),2), paste("Chomage moyen en afrique 2023",'(pourcent)'),width=3, icon = icon("user-slash","fa-fw"), color = "lightblue"),
                valueBox(as.double(evol_emploir_total[dim(evol_emploir_total)[1],2]), paste("Nb Emplois en Afrique en ",as.double(evol_emploir_total[dim(evol_emploir_total)[1],1]),'(milliers)'),width=3, icon = icon("user-plus"), color = "teal"),
                valueBox(as.double(pop_age_travailer_tot[dim(pop_age_travailer_tot)[1],2]), paste("Total pop en age de travailler",as.double(pop_age_travailer_tot[dim(pop_age_travailer_tot)[1],1]),"(milliers)"),width=3, icon = icon("briefcase","fa-fw"), color = "olive"),
                valueBox(as.double(emp_nat[dim(emp_nat)[1],2]), paste("Emploie dans le secteur public",as.double(emp_nat[dim(emp_nat)[1],1]),'(millier)'),width=3, icon = icon("building","fa-fw"), color = "purple"),
                valueBox(round(pourcent_enf_act_total[dim(pourcent_enf_act_total)[1],2],2), paste("% Enfant dans l'activite economique en ",pourcent_enf_act_total[dim(pourcent_enf_act_total)[1],1]),width=3, icon = icon("users"), color = "olive"),
                valueBox(round(pourcent_femme_poste_directio[dim(pourcent_femme_poste_directio)[1],2],2), paste("% femmes dans les direction admin en ",pourcent_femme_poste_directio[dim(pourcent_femme_poste_directio)[1],1]),width=3, icon = icon("female","fa-fw"), color = "purple",),
                valueBox(accident_moy_sala_total[dim(accident_moy_sala_total)[1],2], paste("Total accident mortel en",accident_moy_sala_total[dim(accident_moy_sala_total)[1],1],"(pour 100 000)"),width=3, icon = icon("car-crash","fa-fw"), color = "lightblue"),
                valueBox(round(taux_pauvre_total[dim(taux_pauvre_total)[1],2],2), paste("Taux de pauvrete en emploie en",taux_pauvre_total[dim(taux_pauvre_total)[1],1],'(%)'),width=3, icon = icon("dollar-sign","fa-fw"), color = "teal")
                
                ),
        
              fluidRow(
                box(title = "Sécurité et conditions\n de travail", status = "primary", solidHeader = TRUE,
                    selectInput('securite_input','',c('accident emploie','chomage en emploie','pauvrete en emploie')),
                    withSpinner(plotlyOutput("Sécurité"), color = getOption("spinner.color", default = "#FFA500"))
                    ),
                box(title = "Emploi formel et secteur public", status = "primary", solidHeader = TRUE,
                    sliderInput('plage',label = '',min=1990,max=2023,value = c(2010,2020),animate = T),
                    withSpinner(plotlyOutput("secteur_public",height ="365px"), color = getOption("spinner.color", default = "#FFA500")),height  = 514 )
              ),
              fluidRow(
                box(title = "Emploi informel et inégalités ", status = "primary", solidHeader = TRUE,
                    selectInput('informal_input','',c('Emploie informel','Enfant en emploie')),
                    withSpinner(plotlyOutput("inégalités"), color = getOption("spinner.color", default = "#FFA500"))
                ),
                box(title = "Formation et insertion professionnelle", status = "primary", solidHeader = TRUE,
                    selectInput('professionnelle_input','',c('%femmes au bureau','%population en travail','jeune ni en emploi ni en formation')),
                    withSpinner(plotlyOutput("professionnelle"), color = getOption("spinner.color", default = "#FFA500")) )
              ),
              fluidRow(
                #carte(indicator1 = "NbrEmploiIntermediate",indicator2 = "NbrEmploiHomme",region="Afrique de l’Ouest"))
                box(title = "Visualisation spatiale", status = "primary", solidHeader = TRUE,width = 12,
                    
                    fluidRow(column(selectInput('input_region','',c('Afrique',"Afrique de l’Ouest", "Afrique du Nord","Afrique Centrale","Afrique Australe","Afrique de l’Est" )),width=4),
                    column(selectInput('input_indicateur1','',as.character(names(df_region))[-1][-22][-22]),width=4),
                    column(selectInput('input_indicateur2','',as.character(names(df_region))[-1][-22][-22]),width=4)
                    ),
                    withSpinner(girafeOutput("carte",height = 540), color = getOption("spinner.color", default = "#FFA500")) )
                
                 )
              
      ),
      
      ##################
      
      #####################################################################
      tabItem(tabName = "sec_cond",
          selectInput("selection_pays",'',choices = sort(unique(Taux_de_chômag$ref_area.label)),width = "100%"),
          ###selection_pays_der()
         
          fluidRow(
            plotOutput(outputId = "image_region_pays1",width = "25%",height = 150),
            valueBox(textOutput('pauvre1'), textOutput("pauvre11") ,width=3,icon = icon("dollar-sign","fa-fw"), color = "olive"),
            valueBox(textOutput('chomage1'), textOutput('chomage11'),width=3,icon = icon("user-slash","fa-fw"), color = "indigo"),
            valueBox(paste(length(unique(accident$ref_area.label)),'54',sep = "pays sur "), paste("NB pays avec des informations sur les accidents en rapport avec le travail"),width=3, icon = icon("car-crash","fa-fw"), color = "olive"),
            
          ),
        fluidRow(tabBox(width = 7,title = "Taux de pauvrete",
               tabPanel("Evolution par sexe",
                        withSpinner(plotlyOutput("Age_anee"), color = getOption("spinner.color", default = "#FFA500"))
               ),
               tabPanel('Evolution dans la zone',
                        withSpinner(plotlyOutput("zone_pays"), color = getOption("spinner.color", default = "#FFA500"))
                        )
        ),
        #
        tabBox(title = "Taux de chomage", width = 5,
            tabPanel("Evolution par sex",
                     withSpinner(plotlyOutput("chom_annee_sex"), color = getOption("spinner.color", default = "#FFA500"))),
            tabPanel("Evolution dans la zone",
                     withSpinner(plotlyOutput("chom_zone_pays"), color = getOption("spinner.color", default = "#FFA500")))
            )
        
        
        
        ),
        fluidRow(  #unique(accident$ref_area.label)
          tabBox(title = "Nombre d accident en millier", width = 12,
                 tabPanel("Nombre d accident en millier(visuel disponible uniquement pour les quelques pays) ",
                          selectInput('pays_accid','',unique(accident$ref_area.label)),
                          DTOutput("accident_nb"))
          )
        )
        
      ),
      #######
      tabItem(tabName = "emp_form",
              
              selectInput("selection_pays_emp",'',choices = sort(unique(emp_pub_priv$ref_area.label)),width = "100%"),
              
              
              fluidRow(
                plotOutput(outputId = "image_region_pays2",width = "25%",height = 150), 
                
                valueBox(textOutput('sec_pub1'), textOutput("sec_pub11") ,width=2,icon = icon("female","fa-fw"), color = "orange"),
                valueBox(textOutput('sec_pub2'), textOutput('sec_pub22'),width=2,icon = icon("male","fa-fw"), color = "maroon"),
                valueBox(textOutput('rur_1'), textOutput('rur_11'),width=2, icon = icon("home","fa-fw"), color = "fuchsia"),
                valueBox(textOutput('rur_2'),textOutput('rur_22'),width=3, icon = icon("city","fa-fw"), color = "purple")
                
              ),
              fluidRow(
                
                box(title = "Emploie formel (sexe)", status = "primary", solidHeader = TRUE,width = 7,
                    withSpinner(plotlyOutput("emp_formel_viz"), color = getOption("spinner.color", default = "#FFA500"))
                ),
                box(title = "Emploie formel (residence) ", status = "primary", solidHeader = TRUE,width = 5,
                    withSpinner(plotlyOutput("emp_formel_sexe"), color = getOption("spinner.color", default = "#FFA500"))
                ),
                tabBox(title = "Emploie informel dans les pays de la meme zone", width = 12,
                       tabPanel("Emploie informel (sexe ,residence)",
                                DTOutput("emp_sex_resi"))
                )
              )
              
      ),
      #######
      tabItem(tabName = "emp_infor",
              fluidRow(
                column(6,
                  selectInput("country", "Select Country",
                              choices = unique(femme_direction$ref_area.label),
                              selected = "Angola",width = "100%")),
                  column(6,
                      sliderInput("year_range", "Select Year Range",
                                  min = min(femme_direction$time),
                                  max = max(femme_direction$time),
                                  value = c(min(femme_direction$time), max(femme_direction$time)),
                                  step = 1,
                                  sep = "",width = "100%"))
                  
              ),
              #######
              fluidRow(
                plotOutput(outputId = "image_region_pays3",width = "25%",height = 150),#1
                valueBoxOutput("avg_all", width = 3),
                valueBoxOutput("latest_male", width = 2),
                valueBoxOutput("latest_female", width = 2),
                valueBoxOutput("latest_total", width = 2)
              )
              ,#######
              fluidRow(
                box(width = 6, status = "primary", solidHeader = TRUE,title = "Femme ayant les postes de bureau",
                    withSpinner(plotlyOutput("trend_plot"), color = getOption("spinner.color", default = "#FFA500"))
                ),
                box(status = "primary", solidHeader = TRUE,title = "Informal Employment Distribution by Sex",
                    width = 6,
                    withSpinner(plotlyOutput("bar_plot"), color = getOption("spinner.color", default = "#FFA500"))
                )
              ),
              
              fluidRow(
                box(status = "primary", solidHeader = TRUE,title = "Enfant en activite suivant",
                    width =12 ,
                    uiOutput("year_selector"),
                    withSpinner(plotlyOutput("pie_chart"), color = getOption("spinner.color", default = "#FFA500"))
                )
                
              ), #1
              fluidRow(
                selectInput('input_region_secteur_informel','',c('Afrique',"Afrique de l’Ouest", "Afrique du Nord","Afrique Centrale","Afrique Australe","Afrique de l’Est" ),
                            width ="100%")
              ),
              fluidRow( 
                box(status = "primary", solidHeader = TRUE,title = "Emploi informel par secteur économique",
                    width =6,
                    withSpinner(plotlyOutput("plot_economic_activity"), color = getOption("spinner.color", default = "#FFA500"))
                ),
                box(status = "primary",solidHeader = TRUE,title = "Emploi informel par niveau d'instruction", 
                    width =6,
                    withSpinner(plotlyOutput("plot_emploi_informel_niveauEduc"), color = getOption("spinner.color", default = "#FFA500"))
                )
              )
      ),
    ##########  
    tabItem(tabName = "form_inser",
            fluidRow(
              column(width = 6,
                     selectInput("pays", "Sélectionner un pays",
                                 choices = unique(emploi_stem$ref_area.label), selected='Zambia', width = '100%')
              ),
              column(width=6,
                     sliderInput("annee_range", "Sélectionner la période",
                                 min = min(as.numeric(emploi_stem$time)),
                                 max = max(as.numeric(emploi_stem$time)),
                                 value = c(min(as.numeric(emploi_stem$time)),
                                           max(as.numeric(emploi_stem$time))),
                                 step = 1,width = '100%')
              )
            ),
            fluidRow(
              plotOutput(outputId = "image_region_pays4",width = "25%",height = 150),#1
              valueBoxOutput("total_stem", width = 3), #1
              valueBoxOutput("total_participation", width = 3),#1
              valueBoxOutput("revenu_total", width = 3) #
            ),
            fluidRow(
              box( #1
                width = 6, plotlyOutput("bar_chart"),status = "primary",title="Emploi STEM",solidHeader = TRUE
              ),
              box(width = 6,title="Type de Rémunération",#1
                  plotlyOutput("bar_chart4"),status = "primary",solidHeader = TRUE
              )
              
            ),
            fluidRow(
              box(#1
                width = 6, title="Revenu Mensuel par Sexe",
                plotlyOutput("bar_chart8"), status = "primary", height = 500,solidHeader = TRUE
              ),
              box(#1
                width = 6, title="Jeune ni en emploie ni en formation",
                plotlyOutput("bar_chart9"), status = "primary", height = 500,solidHeader = TRUE
              )
            )
    ),
      #######
     tabItem(tabName = 'Telechargement',
       fluidRow(
         ###########
         column(4,
                selectInput("zone1", "Sélectionner une zone",
                            choices = c('Afrique',as.character(na.omit(unique(Taux_de_chômag$Région)))),
                            selected='Zambia', width = '100%')
         ) ,
         column(4,
                selectInput("pays1", "Sélectionner un pays",
                            choices = unique(emploi_stem$ref_area.label), selected='Zambia', width = '100%')
         ),
         column(4,
                selectInput("indicateur1", "Sélectionner un Indicateur",
                            choices = c(
                              "Accidents mortels au travail" = "Accidents_mortels_travail.csv",
                              "Base exemple" = "Base_exemple.xlsx",
                              "Emploi dans le secteur public par sexe et zones rurales urbaines" = "Emploi_secteur_public_sexe_zones_rurales_urbaines.csv",
                              "Emploi dans les professions STEM par sexe et nationalité" = "Emploi_professions_STEM_sexe_nationalite.csv",
                              "Emploi informel par sexe" = "Emploi_informel_sexe.csv",
                              "Pays et région" = "Pays_region.xlsx",
                              "Population en âge de travailler par sexe âge et situation sur le marché du travail" = "Population_age_sexe_situation_marché_travail.csv",
                              "Proportion de femmes occupant des postes de direction" = "Proportion_femmes_postes_direction.csv",
                              "Proportion de jeunes non scolarisés ni employés ni en formation" = "Proportion_jeunes_non_scolarisés_employés_formation.csv",
                              "Proportion d'enfants engagés dans une activité économique" = "Proportion_enfants_activite_economique.csv",
                              "Revenu mensuel moyen des salariés selon statut d'invalidité" = "Revenu_mensuel_moyen_déclarés_statut_invalidité.csv",
                              "Taux de chômage" = "Taux_chômage.xlsx",
                              "Taux de participation à l'apprentissage en milieu de travail par sexe" = "Taux_participation_apprentissage_travail_sexe.csv",
                              "Taux de pauvreté des travailleurs vivant avec moins de PPA" = "Taux_pauvreté_travailleurs_moins_PPA.csv"
                            ), width = '100%')
         )
         ###########
         ),
       fluidRow(
         box(width = 12,
         DTOutput('bdd')
         )
       ),
       fluidRow(
         column(offset = 10,width=2,
         downloadButton("downloadData", "Télécharger les données")
         )
       )
       )
     ,
    
    #############
    tabItem(tabName = "rapport",
            fluidRow(
              box(
                width = 12,
                title = "Rapport sur l'Emploi des Jeunes en Afrique",
                status = "primary",
                solidHeader = TRUE,
                
                tags$img(
                  src = "statistics.png",
                  class = "cover-image"
                ),
                
                div(class = "description",
                    p("Ce rapport analyse en profondeur la situation de l'emploi et de l'insertion professionnelle 
                des jeunes en Afrique. Il présente les défis actuels, les opportunités émergentes et 
                les stratégies efficaces pour améliorer l'employabilité des jeunes sur le continent."),
                    
                    p("Les principales thématiques abordées incluent :",
                      tags$ul(
                        tags$li("L'état actuel du marché du travail pour les jeunes"),
                        tags$li("Les programmes de formation et d'insertion professionnelle"),
                        tags$li("Les success stories et les bonnes pratiques"),
                        tags$li("Les recommandations pour les décideurs politiques")
                      )
                    )
                ),
                
                div(style = "text-align: center; margin-top: 20px;",
                    downloadButton("downloadReport", "Télécharger le rapport",
                                   class = "btn-lg"
                    )
                )
              )
            )
    ),
   ################## 
    #########
    #Onglet Équipe
    tabItem(tabName = "equipe",
            box(
              width = 12,
              title = "Notre Équipe",
              status = "primary",
              solidHeader = TRUE,
              
              div(class = "description",
                  p("Nous sommes des étudiants en deuxième année à l'Institut Sous-régional de Statistique 
              et d'Économie Appliquée (ISSEA). Notre équipe est composée de cinq membres passionnés 
              par la statistique et l'analyse économique."),
                  
                  div(class = "team-member",
                      tags$img(src = "EEIA_2024.jpg", class = "member-photo"),
                      div(class = "member-info",
                          h4("BANZOUZI MIAMPASSI Hermann"),
                          div(class = "nationality", "Nationalité: Congolaise"),
                          p(" ", icon("envelope"), " miampassimihermann@gmail.com"),
                          div(class = "member-social",
                              a(href = "https://www.linkedin.com/in/banzouzi-miampassi-hermann-b609891ba/", target = "_blank",
                                icon("linkedin"), " LinkedIn"
                              )
                          )
                      )
                  ),
                  
                  div(class = "team-member",
                      tags$img(src = "crepin.png", class = "member-photo"),
                      div(class = "member-info",
                          h4("KOULOU Crépin"),
                          div(class = "nationality", "Nationalité: Togolaise"),
                          p("", icon("envelope"), " crepinkoulo@gmail.com"),
                          div(class = "member-social",
                              a(href = "https://www.linkedin.com/in/crepin-koulou-912aa5248/", target = "_blank",
                                icon("linkedin"), " LinkedIn"
                              )
                          )
                      )
                  ),
                  
                  div(class = "team-member",
                      tags$img(src = "henoc.png", class = "member-photo"),
                      div(class = "member-info",
                          h4("GAKPETO Henoc"),
                          div(class = "nationality", "Nationalité: Togolaise"),
                          p("", icon("envelope"), " henocgakpeto04@gmail.com"),
                          div(class = "member-social",
                              a(href = "https://www.linkedin.com/in/henoc-gakpeto-a10404249?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=android_app", target = "_blank",
                                icon("linkedin"), " LinkedIn"
                              )
                          )
                      )
                  ),
                  
                  div(class = "team-member",
                      tags$img(src = "irch.png", class = "member-photo"),
                      div(class = "member-info",
                          h4("NGOULOU NGOUBILI Irch Defluviaire"),
                          div(class = "nationality", "Nationalité: Congolaise"),
                          p("", icon("envelope"), " ngoubiliirch@gmail.com"),
                          div(class = "member-social",
                              a(href = "https://www.linkedin.com/in/irch-ngoubili-061900244/", target = "_blank",
                                icon("linkedin"), " LinkedIn"
                              )
                          )
                      )
                  ),
                  
                  div(class = "team-member",
                      tags$img(src = "varnel.jpg", class = "member-photo"),
                      div(class = "member-info",
                          h4("TIENTCHEU Varnel"),
                          div(class = "nationality", "Nationalité: Camerounaise"),
                          p("", icon("envelope"), " justintientcheu5@gmail.com"),
                          div(class = "member-social",
                              a(href = "https://www.linkedin.com/in/varnel-tientcheu-269b31237?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=android_app", target = "_blank",
                                icon("linkedin"), " LinkedIn"
                              )
                          )
                      )
                  )
              )
            )
    ),
    
    # Onglet ISSEA
    tabItem(tabName = "issea",
            box(
              width = 12,
              title = "L'Institut Sous-régional de Statistique et d'Économie Appliquée (ISSEA)",
              status = "primary",
              solidHeader = TRUE,
              
              div(class = "issea-content",
                  tags$img(
                    src = "ISSEABG.jpg",
                    class = "issea-image"
                  ),
                  
                  p("L'ISSEA est un établissement d'excellence spécialisé dans la formation en statistique 
              et en économie appliquée. Créé pour répondre aux besoins en cadres statisticiens des 
              pays d'Afrique centrale, l'institut forme des experts capables de répondre aux défis 
              du développement économique et social."),
                  
                  h4("Notre Mission"),
                  p("Former des statisticiens et des économistes de haut niveau capables de :",
                    tags$ul(
                      tags$li("Collecter et analyser des données statistiques"),
                      tags$li("Réaliser des études économiques et sociales"),
                      tags$li("Contribuer à la prise de décision dans les secteurs public et privé"),
                      tags$li("Participer au développement des systèmes statistiques nationaux")
                    )
                  ),
                  
                  h4("Notre Vision"),
                  p("Être un centre d'excellence reconnu en Afrique dans la formation statistique et 
              économique, contribuant activement au développement du continent à travers la 
              production et l'analyse de données fiables.")
              )
            )
    ),
    tabItem(tabName = 'prevision',
      fluidRow(
        box(title = "Choisissez un indicateur", status = "primary",solidHeader = TRUE,
          width = 12,
          selectInput("indicator", "Choisir l'indicateur:",
                      choices = names(data_previ),
                      selected = names(data_previ)[1])
        )),
      fluidRow(
        box(width = 12,title = "Prevision de l emploie et du chomage sur les 5 prochaines annees", status = "primary", solidHeader = TRUE,
            plotlyOutput("forecast_plot")
        )
      ),
      fluidRow(
        box(width = 12,title = "UTILE", status = "primary", solidHeader = TRUE,
            DTOutput("forecast_table")
        )
      )
    )
    )
    
    ##############
  
    
    ),
  preloader = list(html = tagList(spin_1(), "Chargement ..."), color = "lightblue")
)

# Serveur
server <- function(input, output,session) {
  
  
  VB_Empl_STEM=function(var1,var2){
    
    total <- emploi_stem %>%
      filter(ref_area.label == var1,
             sex.label == "Sex: Total",
             classif1.label == "Citizenship: Total") %>%
      filter(time == max(time)) %>%
      pull(obs_value)
    
    valueBox(
      value = format(round(total, 1), big.mark = " "),
      subtitle = paste("Nombre total d'emplois STEM (en milliers) -", max(var2)),
      icon = icon("laptop-code"),
      color = "danger"
    )
  }  #Nombre total d'emplois STEM (en milliers)
  
  Evol_Empl_STEM=function(var1,var2){
    req(var1, var2)
    
    donnees_graph <- emploi_stem %>%
      filter(ref_area.label == var1,
             sex.label %in% c("Sex: Male", "Sex: Female"),
             classif1.label == "Citizenship: Total",
             as.numeric(time) >= var2[1],
             as.numeric(time) <= var2[2]) %>%
      mutate(sex.label = sub("Sex: ", "", sex.label))
    
    plot_ly(donnees_graph,
            x = ~time,
            y = ~obs_value,
            color = ~sex.label,
            type = "bar",
            text = ~round(obs_value, 1),
            textposition = "auto") %>%
      layout(
        title = paste("Évolution de l'emploi STEM par sexe -", var1),
        xaxis = list(title = "Année"),
        yaxis = list(title = "Nombre d'emplois (milliers)"),
        barmode = "group"
      )
  }  #Évolution de l'emploi STEM par sexe
  
  VB_taux_parti=function(var1,var2){
    total <- Tau_part_apprenti %>%
      filter(ref_area.label == var1,
             sex.label == "Sex: Total",
             classif1.label == "Age (Aggregate bands): Total",
             classif2.label == "Paid or unpaid: Total trainees") %>%
      filter(time == max(time)) %>%
      pull(obs_value)
    
    valueBox(
      value = paste0(format(round(total, 1), big.mark = " "), "‰"),
      subtitle = paste("Taux de participation à l'apprentissage -", max(var2)),
      icon = icon("graduation-cap"),
      color = "teal"
    )
  }  #Value box Taux de participation à l'apprentissage (pour 1000 personnes)
  
  Taux_parti_rem=function(var1,var2){
    req(input$pays, input$annee_range)
    
    donnees_graph <- Tau_part_apprenti %>%
      filter(ref_area.label == var1,
             sex.label == "Sex: Total",
             classif1.label == "Age (Aggregate bands): Total",
             as.numeric(time) >= var2[1],
             as.numeric(time) <= var2[2]) %>%
      mutate(classif2.label = sub("Paid or unpaid: ", "", classif2.label))
    
    plot_ly(donnees_graph,
            x = ~time,
            y = ~obs_value,
            color = ~classif2.label,
            type = "bar",
            text = ~paste0(round(obs_value, 1), "‰"),
            textposition = "auto") %>%
      layout(
        title = paste("Taux de participation par type de rémunération -", var1),
        xaxis = list(title = "Année"),
        yaxis = list(title = "Taux de participation (‰)"),
        barmode = "stack"
      )
  }  #Taux de participation par type de rémunération
  
  
  
  jeunes_NEET=function(var1,var2){
    
    req(var1, var2)
    
    donnees_graph <- Proportion_jeunes %>%
      filter(ref_area.label == var1,
             time == var2,
             sex.label != "Sex: Total") %>%
      mutate(sex.label = sub("Sex: ", "", sex.label))
    
    plot_ly(donnees_graph, 
            labels = ~sex.label, 
            values = ~obs_value,
            type = 'pie',
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(sex.label, 
                          "\nValeur:", round(obs_value, 1), "%")) %>%
      layout(title = paste("Distribution des jeunes NEET par sexe -",
                           var1, var2),
             showlegend = TRUE)
  }  #Distribution des jeunes NEET par sexe
  
  VB_rev_total=function(var1,var2){
    revenu <- Revenu_mensuel_moyen %>%
      filter(ref_area.label ==var1 ,
             sex.label == "Sex: Total",
             classif1.label == "Disability status (Aggregate): Total",
             classif2.label == "Currency: U.S. dollars") %>%
      filter(time == max(time)) %>%
      pull(obs_value)
    
    valueBox(
      value = paste0("$", format(round(revenu, 2), big.mark = " ")),
      subtitle = paste("Revenu mensuel moyen -", max(var2)),
      icon = icon("dollar-sign"),
      color = "info"
    )
  } # value box evolution mensuel moyen
  
  Evol_rev_men=function(variable,variable2){
    donnees_graph <- Revenu_mensuel_moyen %>%
      filter(ref_area.label == variable,
             sex.label != "Sex: Total",
             classif1.label == "Disability status (Aggregate): Total",
             classif2.label == "Currency: U.S. dollars",
             as.numeric(time) >= variable2[1],
             as.numeric(time) <= variable2[2]) %>%
      mutate(sex.label = sub("Sex: ", "", sex.label))
    
    plot_ly(donnees_graph,
            x = ~time,
            y = ~obs_value,
            color = ~sex.label,
            type = "bar",
            text = ~paste0("$", round(obs_value, 2)),
            textposition = "auto") %>%
      layout(
        title = paste("Évolution du revenu mensuel par sexe -", variable),
        xaxis = list(title = "Année"),
        yaxis = list(title = "Revenu mensuel (USD)"),
        barmode = "group"
      )
  } # Évolution du revenu mensuel par sexe
  
  
  
  VB_femm_poste_direction=function(var1){
    avg <- mean((femme_direction %>%
                   filter(ref_area.label == var1))$obs_value)
    valueBox(
      value = sprintf("%.1f%%", avg),
      subtitle = "% de femmes occupant des postes de direction",
      icon = icon("percent"),
      color = "warning"
    )
  }
  
  graph_women_post_direction=function(var1,var2){
    ggplotly(ggplot(femme_direction %>%
             filter(ref_area.label == var1,
                    time >= var2[1],
                    time <= var2[2]), aes(x = time, y = obs_value)) +
      geom_line(color = "#2C3E50", size = 1) +
      geom_point(color = "#E74C3C", size = 3) +
      labs(
        title = paste("Women in Managerial Positions -", var1),
        x = "Year",
        y = "Proportion (%)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      ))
  }
  
  ###################################################
  
  select_annee_recente=function(var1){
    Emploi_inf_sex %>%
      filter(ref_area.label == var1) %>%
      group_by(time) %>%
      filter(n() >= 2) %>%  # Ensure we have both male and female data
      ungroup() %>%
      filter(time == max(time))
  }
  
  filter_base_emploi_informel=function(var1,var2){
    Emploi_inf_sex %>%
      filter(
        ref_area.label == var1,
        time >= var2[1],
        time <= var2[2],
        sex.label %in% c("Sex: Male", "Sex: Female")
      )
  }
  
  VB_Male_Informal_Employment=function(var1){
    value <- var1() %>%
      filter(sex.label == "Sex: Male") %>%
      pull(obs_value)
    valueBox(
      value = sprintf("%.1f K", value/1000),
      subtitle = "Male Informal Employment",
      icon = icon("male"),
      color = "olive"
    )
  }
  
  VB_Female_Informal_Employment=function(var1){
    value <- var1() %>%
      filter(sex.label == "Sex: Female") %>%
      pull(obs_value)
    valueBox(
      value = sprintf("%.1f K", value/1000),
      subtitle = "Female Informal Employment",
      icon = icon("female"),
      color = "purple"
    )
  }
  
  selec_last_year=function(var1){
    year <- max(var1()$time)
    valueBox(
      value = year,
      subtitle = "Latest Year",
      icon = icon("calendar"),
      color = "purple"
    )
  }
  
  Inf_Employ_Distr= function(var1,var2){
    plot_data <- var2()
    
    # Calculate percentages
    plot_data <- plot_data %>%
      group_by(time) %>%
      mutate(total = sum(obs_value),
             percentage = obs_value/total * 100)
    
    ggplotly(ggplot(plot_data, aes(x = factor(time), y = obs_value, fill = sex.label)) +
      geom_bar(stat = "identity", position = "fill") +
      geom_text(aes(label = sprintf("%.1f%%", percentage)),
                position = position_fill(vjust = 0.5)) +
      scale_fill_manual(values = c("Sex: Male" = "#3498db", "Sex: Female" = "#e74c3c")) +
      labs(
        title = paste("Informal Employment Distribution by Sex -", var1),
        x = "",
        y = "Number of People (thousands)",
        fill = "Sex"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "top"
      ))%>% layout(
        legend = list(title="sex",
                      orientation = "h",
                      x = 0.5,
                      xanchor = "center",
                      y = -0.2),
        annotations = list(
          list(
            showarrow = FALSE,
            textangle = 90,  # Orientation verticale des labels
            xanchor = 'center',
            yanchor = 'bottom'
          )
        )
        )
  }
  
  ########################################################
  selection_year=function(var1){
    Proportion_enfants %>%
      filter(ref_area.label == var1) %>%
      pull(time) %>%
      unique()
  }
  
  selection_child=function(var1){
    Proportion_enfants %>%
      filter(
        ref_area.label == var1,
        sex.label == "Sex: Total",
        classif1.label == "Age (Child labour bands): '5-17"
      ) %>%
      filter(time == max(time)) %>%
      pull(obs_value)
  }
  
  VB_child=function(var1,var2){
    
    valueBox(
      value = sprintf("%.1f%%", var2()),
      subtitle = "Proportion d'enfants en activité",
      icon = icon("child"),
      color = "secondary",
      width = 12
    )
  } # Latest Total Child Labour Rate
  
  pie_child=function(var1,var2,b=''){
    
    plot_data <- Proportion_enfants %>%
      filter(
        ref_area.label == var1,
        time == var2,
        sex.label %in% c("Sex: Male", "Sex: Female"),
        classif1.label == "Age (Child labour bands): '5-17"
      )
    
      p = plot_ly(plot_data, 
                  labels = ~sex.label, 
                  values = ~obs_value,
                  type = 'pie',
                  marker = list(colors = c('#3498db', '#e74c3c')),
                  textinfo = 'label+percent',
                  hoverinfo = 'text',
                  text = ~paste(sex.label, "<br>",
                                "Value:", sprintf("%.1f%%", obs_value))) %>%
        layout(title = paste("Distribution d'enfants en activité selon le sexe -", 
                             var1, var2),
               showlegend = TRUE)
    
    p
  }  # Child Labour Distribution by Sex
  

 output$Sécurité= renderPlotly({
   ##accident_plot(debut = 1990,fin=2020)
   ##chom_plot(debut = 2000,fin = 2020), pauvr_travail_plot(debut = 2000,fin = 2020)
   if(input$securite_input=='accident emploie'){
     accident_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }else if(input$securite_input=='chomage en emploie'){
     chom_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }else{
     pauvr_travail_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }
    
  })
 
 output$secteur_public= renderPlotly({
   ##pop_rur_urb_emplo_plot(debut = 1990,fin=2010)
   
   pop_rur_urb_emplo_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
 })
 
 output$inégalités= renderPlotly({
   ##employ_plot(debut = 2000,fin=2010)
   ##pourcent_enfant_plot(debut = 2000,fin = 2020)
   if (input$informal_input=='Emploie informel'){
     employ_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }else{
     pourcent_enfant_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }
   
 })
 
 output$professionnelle= renderPlotly({
   ##plot_dir_femme()
   ##pop_en_travil_plot(debut = 1990,fin=2010)
   if(input$professionnelle_input=='%femmes au bureau'){
     plot_dir_femme(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }else if(input$professionnelle_input=='%population en travail'){
     pop_en_travil_plot(debut = as.integer(input$plage[1]),fin = as.integer(input$plage[2]))
   }else{
     function_jeune_niemp_ni_for(data=jeune_ni_emploi_niformation)  
   }
 })
 
 output$carte=renderGirafe(
   {
     if(input$input_region=="Afrique"){
       carte(indicator1 = input$input_indicateur1,indicator2 = input$input_indicateur2)
     }else{
       carte(indicator1 = input$input_indicateur1,indicator2 = input$input_indicateur2,region=input$input_region)
       
     }
            
   }
 )
 
 ##1
 #1
 output$plot_economic_activity= renderPlotly(
   if(input$input_region_secteur_informel=="Afrique"){
     plot_economic_activity(data=secteur_emploi)
   }else{
     plot_economic_activity(data=secteur_emploi,zone=input$input_region_secteur_informel)
   }
   
 )
 #1
 output$plot_emploi_informel_niveauEduc= renderPlotly(
   if(input$input_region_secteur_informel=="Afrique"){
     plot_emploi_informel_niveauEduc(data=emploi_informel_sexe_age_ni)
   }else{
     plot_emploi_informel_niveauEduc(data=emploi_informel_sexe_age_ni,zone=input$input_region_secteur_informel)
   }
   
 )
 
 #1
 output$image_region_pays1= renderPlot(
   zone_pays_carte(input$selection_pays)
 )
 #1 
 # Emploi formel
 output$image_region_pays2= renderPlot(
   zone_pays_carte(input$selection_pays_emp)
 )
 #1
 # emploi Informel
 
 
 output$image_region_pays3= renderPlot(
   zone_pays_carte(input$country)
 ) 
 
 # Insertion
 #1
 
 output$image_region_pays4= renderPlot(
   zone_pays_carte(input$pays)
 ) 
 
 
 
 
 output$Age_anee<-renderPlotly({
   selection_pays(data = taux_pauvre,pays=input$selection_pays,debut=2000,fin=2023)
 })

 output$pauvre1=renderText({
   as.character(selection_pays_der(pays=input$selection_pays)[2])
 })
 
 output$pauvre11=renderText({
   paste("Taux de pauvrete recent(pourcentage)"," en ",as.character(selection_pays_der(pays=input$selection_pays)[1]))
 })
 
 output$zone_pays =renderPlotly({
   pauvrete_pays_sous_region(input$selection_pays)
 })
 
 
 output$chom_annee_sex   = renderPlotly({
     chom_annee_sex(input$selection_pays)

})
 
 
 ##chomage_vs_region("Kenya")
 output$chom_zone_pays=renderPlotly({
   chomage_vs_region(input$selection_pays)
 })
 
 
 output$chomage11=renderText({
   paste("Taux de chomage recent ",round(as.double(indicateur_chomage(input$selection_pays)[1]),3),"(pourcentage)")
 })
 
 output$chomage1=renderText({
   round(as.double(indicateur_chomage(input$selection_pays)[2]),3)
 })
 
 output$accident_nb=renderDataTable({
   datatable(
   accident %>%  select(ref_area.label,sex.label,time,obs_value,Région) %>% filter(ref_area.label==input$pays_accid),
   options = list(pageLength = 6)
   )
   })
 
 
 output$emp_formel_viz=renderPlotly({
   emp_formel_viz(input$selection_pays_emp)
 })
 
 output$sec_pub1 = renderText({
   a = emp_formel_vi(input$selection_pays_emp)
   a[3]
 })
 output$sec_pub2 = renderText({
   a = emp_formel_vi(input$selection_pays_emp)
   a[2]
 })
 
 output$sec_pub11 = renderText({
   a = emp_formel_vi(input$selection_pays_emp)
   paste("Emploie formel recent dans le secteur public en ",a[1],'en milliers(homme)')
 })
 output$sec_pub22 = renderText({
   a = emp_formel_vi(input$selection_pays_emp)
   paste("Emploie formel recent dans le secteur public en ",a[1],'en milliers(femme)')
 })
 
 output$emp_formel_sexe=renderPlotly({
   emp_formel_v(input$selection_pays_emp)
 })
 
output$rur_1=renderText({
  b = emp_formel_rur(input$selection_pays_emp)
  b[2]
}) 

output$rur_22=renderText({
  b = emp_formel_rur(input$selection_pays_emp)
  
  paste("Emploie formel recent dans la zone urbaine en ",b[1],'en milliers')
}) 

output$rur_2=renderText({
  b = emp_formel_rur(input$selection_pays_emp)
  b[3]
}) 


output$rur_11=renderText({
  b = emp_formel_rur(input$selection_pays_emp)
  paste("Emploie formel recent dans la zone rural en ",b[1],'en milliers')
}) 

output$emp_sex_resi= renderDataTable({
  datatable(
  emp_pub_priv  %>% filter(str_ends(emp_pub_priv$sex.label,'tal')) %>% filter(Région==as.character(unique(emp_pub_sex_1 %>% filter(ref_area.label==input$selection_pays_emp) %>% select(Région))))   %>% select(ref_area.label,time,obs_value,Région,classif1.label) 
   ,options = list(pageLength = 6)
  )
  })
 

 ##########Value Boxes
output$avg_all <- renderValueBox({
  VB_femm_poste_direction(input$country)
})



# Trend Plot
output$trend_plot <- renderPlotly({
  graph_women_post_direction(input$country, input$year_range)
})


########################################## 2e indicateur ####################################

# Get latest data for selected country
latest_data <- reactive({
  select_annee_recente(input$country)
})

# Filtered data for plot
filtered_data1 <- reactive({
  filter_base_emploi_informel(input$country, input$year_range)
})

# Value Boxes
output$latest_male <- renderValueBox({
  VB_Male_Informal_Employment(latest_data)
})

output$latest_female <- renderValueBox({
  VB_Female_Informal_Employment(latest_data)
})


output$latest_total <- renderValueBox({
  selec_last_year(latest_data)
})

# Stacked Bar Plot
output$bar_plot <- renderPlotly({
  Inf_Employ_Distr(input$country,filtered_data1)
})

####################################################### 3e indicateur #############################


# Get available years for selected country
available_years <- reactive({
  
  selection_year(input$country)
})

############## Dynamic year selector
output$year_selector <- renderUI({
  selectInput("selected_year", "Select Year",
              choices = available_years(),
              selected = max(available_years()))
})

# Get latest total value for selected country
latest_total <- reactive({
  selection_child(input$country)
})

# Value Box
output$latest_total <- renderValueBox({
  
  VB_child(input$country,latest_total)
})
####################

# Pie Chart
output$pie_chart <- renderPlotly({
  
  pie_child(input$country,input$selected_year)
  
})
#############################################################fin 3ieme onglet#######################



# Value box pour le total STEM
output$total_stem <- renderValueBox({
  VB_Empl_STEM(input$pays,emploi_stem$time)
})

# Graphique à barres
output$bar_chart <- renderPlotly({
  Evol_Empl_STEM(input$pays,input$annee_range)
})


########################################## 2e indicateur ##########################

# Value box pour le taux total de participation
output$total_participation <- renderValueBox({
  
  VB_taux_parti(input$pays,emploi_stem$time)
})

# Graphique à barres
output$bar_chart4 <- renderPlotly({
  
  Taux_parti_rem(input$pays,input$annee_range)
  
})

############################################ proportion jeune #############
# Mise à jour des années disponibles







########################################### revenu ###################################




# Value box pour le revenu total le plus récent
output$revenu_total <- renderValueBox({
  VB_rev_total(input$pays,Revenu_mensuel_moyen$time)
})

# Graphique à barres juxtaposées
output$bar_chart8 <- renderPlotly({
  req(input$pays, input$annee_range)
  
  Evol_rev_men(input$pays,input$annee_range)
})
 

observeEvent(input$zone1, {
  if (input$zone1 == "Afrique") {
    updateSelectInput(session, "pays1", 
                      choices = pays_region$Pays)
  } else {
    updateSelectInput(session, "pays1", 
                      choices = pays_region$Pays[pays_region$Région==input$zone1])
  }
})
############
#
d=data.frame()
s=reactive({
  
  if(input$indicateur1=='Accidents_mortels_travail.csv'){
    d = accident
  }else if(input$indicateur1=='Base_exemple.xlsx'){
    d= base
  }else if(input$indicateur1=='Emploi_secteur_public_sexe_zones_rurales_urbaines.csv'){
    d = emp_pub_priv
  }else if(input$indicateur1=='Emploi_professions_STEM_sexe_nationalite.csv'){
    d = merger(emploi_stem)
  }else if(input$indicateur1=='Emploi_informel_sexe.csv'){
    d=  merger(Emploi_inf_sex)
  }else if(input$indicateur1=='Pays_region.xlsx'){
    d= pays_region
  }else if(input$indicateur1=="Population_age_sexe_situation_marché_travail.csv"){
    d= pop_age_travaile
  }else if(input$indicateur1=='Proportion_femmes_postes_direction.csv'){
    d= merger(femme_direction)
  }else if(input$indicateur1=='Proportion_jeunes_non_scolarisés_employés_formation.csv'){
    d= merger(Proportion_jeunes)
  }else if(input$indicateur1=='Proportion_enfants_activite_economique.csv'){
    d=merger(Proportion_enfants)
  }else if(input$indicateur1=='Revenu_mensuel_moyen_déclarés_statut_invalidité.csv'){
    d= merger(revenu_moy_sala)
  }else if(input$indicateur1=='Taux_chômage.xlsx'){
    d= Taux_de_chômag
  }else if(input$indicateur1=='Taux_participation_apprentissage_travail_sexe.csv'){
    d= merger(Tau_part_apprenti)
  }else if(input$indicateur1=='Taux_pauvreté_travailleurs_moins_PPA.csv'){
    d= taux_pauvre
  }
  if(input$zone1 == "Afrique" || input$indicateur1 %in% c('Base_exemple.xlsx',"Pays_region.xlsx")){
    d
  }else{
    
    d=d %>% filter(Région==input$zone1) %>% filter(ref_area.label==input$pays1) 
    
  }
  }
)

output$bdd=renderDT({
  
  
  datatable(s(), 
      options = list(pageLength = 5, scrollX = TRUE))
  
})


######
output$downloadData <- downloadHandler(
  filename = function() {
    paste("data", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(s(), file, row.names = FALSE)  # Écriture des données dans un fichier CSV
  }
)
######
output$downloadReport <- downloadHandler(
  filename = function() {
    paste("rapport_emploi_jeunes_", Sys.Date(), ".pdf", sep = "")
  },
  content = function(file) {
    file.copy("rapport.pdf", file)
  }
)
##############
tableau=function(var1){
  fc <- var1()
  forecast_df <- data.frame(
    Année = 2024:2028,
    Borne_inf=round(as.numeric(fc$lower), 2),
    Valeur = round(as.numeric(fc$mean), 2),
    Borne_sup=round(as.numeric(fc$upper), 2)
  )
  # Formatage du tableau avec coloration
  datatable(forecast_df,
            options = list(pageLength = 5,
                           dom = 't'),
            rownames = FALSE)
}


graphique=function(var1,var2){
  fc <- var1()
  
  # Données historiques
  historical_df <- data.frame(
    Year = 2000:2023,
    Value = data_previ[[var2]],
    Type = "Historique"
  )
  
  # Données prédites
  forecast_df <- data.frame(
    Year = 2024:2028,
    Value = var1(),
    Type = "Prévision"
  )
  
  # Combiner les données
  #all_data <- rbind(historical_df, forecast_df)
  
  # Créer le graphique avec plotly
  plot_ly() %>%
    add_trace(data = historical_df,
              x = historical_df$Year,
              y =  historical_df$Value,
              type = 'scatter',
              mode = 'lines',
              name = 'Historique',
              line = list(color = '#1f77b4')) %>%
    add_trace(data = forecast_df,
              x =forecast_df$Year,
              y =forecast_df$Value.Point.Forecast,
              type = 'scatter',
              mode = 'lines',
              name = 'Prévision',
              line = list(color = '#ff7f0e')) %>%
    layout(title = paste("Évolution et prévision -",var2),
           xaxis = list(title = "Année"),
           yaxis = list(title = var2),
           hovermode = "x unified")
}


# Fonction pour faire la prédiction ARIMA
forecast_data <- reactive({
  ts_data <- data_previ[[input$indicator]]
  model_arima <- auto.arima(ts_data)
  forecast_data <- forecast(model_arima, h = 5)
})

# Création du graphique
output$forecast_plot <- renderPlotly({
  graphique(forecast_data,input$indicator)
})

# Création du tableau
output$forecast_table <- renderDT({
  tableau(forecast_data)
})

output$bar_chart9 = renderPlotly({
  function_jeune_niemp_ni_for(data=jeune_ni_emploi_niformation,zone = df_region %>% filter(NAME == input$pays) %>% pull(Région) %>% unique())
})




}


shinyApp(ui, server
         
)




