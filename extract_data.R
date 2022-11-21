# read REHABASE
close.screen(all=TRUE)
rm(list=ls())
header=1;

library(dplyr)
library(tidyr)
library(mice)
library(parallel)
library(fastDummies)

##################
##################
#Load PEC
all_PEC2<-read.csv(file="C:/Users/Guillaume/Desktop/Recoverit 2022-04-17 at 22.09.53/_DATA/CRR/REHABase/REHABASE_PEC_allPEC2.csv", #2 (second extraction) ou RAS
                  header=TRUE, sep=",")
all_PEC=subset(all_PEC2,select=c("StudySubjectID","ProtocolID","LISTE_DOMAINE_SOIN",
                                "MODAL_PEC","NB_HEURES","DATE_DEB"))
all_PEC %>% 
  dplyr::distinct(StudySubjectID)

##################
##################
#Load scores
First_Network_REHABASE_tot = read.csv(file="C:/Users/Guillaume/Desktop/Recoverit 2022-04-17 at 22.09.53/_DATA/CRR/REHABase/extract13122021.csv",
                                      header=TRUE, sep=",")


all_W <- First_Network_REHABASE_tot %>%
  dplyr::select(Study.Subject.ID,Age, Sex,Education, Employment,
                Dx, Dx2, Dx_SOMA, GAF, CGI, Fam, Parent, Adresseur, RQTH,
                Illness_Duration, First_Contact, N_Admissions, Marginalisation, Forensic, Addictions,
                SQoL18_SEL,SQoL18_RES,SQoL18_AUT,SQoL18_PHY,SQoL18_PSY,SQoL18_FRI,SQoL18_FAM,SQoL18_ROM,SQoL18_TOT,
                WEMWBS_TOT,
                IS_Sympt,IS_Disease,IS_Treatment,IS_TOT,
                ISMI_Alien,ISMI_Approb,ISMI_Discrim,ISMI_Isol,ISMI_Intern,ISMI_Resist,ISMI_TOT,
                STORI,
                MAT, SIM, D2R_CCT, D2R_E, D2R_CC,
                # RLRI16_RLI, RLRI16_RLD, RLRI16_RTD, CVLT_RLI,CVLT_RLLT,MEMCHIF_MCD, MEMCHIF_MCI,
                # COMMISSIONS_time, COMMISSIONS_error,
                ACSo_TOT,TIME
                ) %>%
  mutate(across(c(Age,SQoL18_SEL:ACSo_TOT),~ gsub(",",".", .))) %>%
  mutate(across(c(Age,SQoL18_SEL:ISMI_TOT,MAT:ACSo_TOT),~ as.numeric(.))) %>%
  rename_with(~gsub("SQOL18_","",.x)) %>%
  #filter(Study.Subject.ID=="01ABMA0280") %>%
  na_if("") %>%
  na_if(8888) %>%
  ###
  # select(Study.Subject.ID,
  #        Diagnostic.psychiatrique.principal..codes.CIM10.reconnus.par.DSM5.,
  #        Dx) %>%
  ###
  dplyr::group_by(Study.Subject.ID) %>%
  # dplyr::mutate(across(Age:ACSo_TOT,~first(na.omit(.)))) %>%
  dplyr::mutate(across(Age:Addictions,~first(na.omit(.)))) %>%
  dplyr::mutate(across(SQoL18_SEL:D2R_CC,~first(.))) %>%
  dplyr::distinct(Study.Subject.ID,.keep_all = TRUE) 



##################
##################
#### Anal for Elodie

tmp1 <- all_W %>%
  group_by(Study.Subject.ID,Dx) %>%
  #filter(Study.Subject.ID=="01ABMA0280") %>%
  #filter(Dx=="1-Troubles NEURODEVELOPPEMENTAUX") %>%
  summarise(Add_type=unlist(strsplit(Addictions,","))) %>%
  mutate(Behav_add=case_when(Add_type==5~1,
                             Add_type==6~1,
                             Add_type==9~1,
                             Add_type==12~1,
                             Add_type==16~1,
                             Add_type==0~3,
                             TRUE~2)) %>%
  arrange(Behav_add)

tmp2 <- tmp1 %>%
  distinct(Study.Subject.ID,.keep_all=TRUE)#takes the FIRST one!

 tmp3<-tmp2 %>% 
   group_by(Dx,Behav_add) %>%
   summarise(n=n())
# 
#  Dx=case_when(Dx=="1-Troubles NEURODEVELOPPEMENTAUX" ~ "ASD",
#               Dx=="18-Troubles de la PERSONNALITE" ~ "PD",
#               Dx=="2-Spectre de la SCHIZOPHRENIE" ~ "SCZ",
#               Dx=="3-Troubles BIPOLAIRES" ~ "BAD",
#               Dx=="4-Troubles DEPRESSIFS" ~ "DEP",
#               Dx=="5-Troubles de l’ANXIETE" ~ "ANX" ,
#               Dx=="6-TOC" ~ "ANX",
#               Dx=="7-Troubles liés à STRESS ou TRAUMATISME" ~ "ANX"
#               
 tmp4 <- tmp3 %>%
   filter(Dx=="1-Troubles NEURODEVELOPPEMENTAUX" |Dx=="3-Troubles BIPOLAIRES") %>%
   pivot_wider(names_from=Behav_add,values_from=n)
 
 m=tmp4[-c(1,4)]# m=tmp4[-1]

 rownames(m)=tmp4$Dx
chisq.test(m)


#### fin anal Elodie Zante
##################
##################


##################
##################
#merge files
mat_A_W=left_join(all_PEC,all_W,by=c("StudySubjectID"="Study.Subject.ID"))
#Nparticipants
k<-mat_A_W %>% 
  group_by(StudySubjectID) %>%
  summarize(N=n())
  sum(k$N)
  

##################
##################
#select treatments of interest
mat_A_W_Aselect = mat_A_W %>% 
  dplyr::filter(#princeps, specific therapies
                  LISTE_DOMAINE_SOIN == "7-PSYCHO-EDUCATION/EDUCATION THERAPEUTIQUE" |
                  LISTE_DOMAINE_SOIN == "5-REMEDIATION COGNITIVE" |
                  LISTE_DOMAINE_SOIN == "8-Thérapies COGNITIVES et COMPORTEMENTALES"  | 
                    
                  LISTE_DOMAINE_SOIN == "6-COGNITION SOCIALE" |
                  
                  #sociotherapy
                  LISTE_DOMAINE_SOIN == "2-AUTONOMIE & OUVERTURE vers l'ESPACE SOCIAL" |
                  LISTE_DOMAINE_SOIN == "11-ATELIERS THERAPEUTIQUES & TECHNIQUES" |            
                  LISTE_DOMAINE_SOIN == "9-RESEAU PARTENAIRE" |
                  LISTE_DOMAINE_SOIN == "15-ESSORT" 
                  
                  #not enough data
                  # LISTE_DOMAINE_SOIN == "4-FAMILLE" |    
                  # LISTE_DOMAINE_SOIN == "13*-PAIR-AIDANCE" |
                  
                  #too aspecific, or nil treatment
                  # LISTE_DOMAINE_SOIN == "14-CASE MANAGEMENT de TRANSITION (CMT)"  | 
                  # LISTE_DOMAINE_SOIN == "3-COMPETENCES SOCIALES" | #includes psychotherapy
                  # LISTE_DOMAINE_SOIN == "12-ENTRETIENS INDIVIDUELS" | #includes psychotherapy
                  # LISTE_DOMAINE_SOIN == "10-PSYCHOTHERAPIES PSYCHODYNAMIQUES"
                  #LISTE_DOMAINE_SOIN == "99*-AUTRE type de PEC" #too aspecific
                  #LISTE_DOMAINE_SOIN == "17-Bilan de SUIVI ANNUEL"
                  #LISTE_DOMAINE_SOIN == "0-PAS de Prise en Charge au CR"
                  #LISTE_DOMAINE_SOIN == "16-BILAN Ã  l'ENTREE"
                  #LISTE_DOMAINE_SOIN == "18*-SYNTHESES PLURIDISCIPLINAIRES"
                  #LISTE_DOMAINE_SOIN == "1-EVALUATIONS APPROFFONDIES (Vie Sociale et TRAVAIL)"
                
                  )
mat_A_W_Aselect %>% 
    group_by(StudySubjectID) %>%
    summarize(N=n())

##################
##################
#Clean A and W
mat_A_W_final<-mat_A_W_Aselect %>% 
  mutate(
    PSR=case_when(LISTE_DOMAINE_SOIN=="7-PSYCHO-EDUCATION/EDUCATION THERAPEUTIQUE"~"EDUC",
                  LISTE_DOMAINE_SOIN == "5-REMEDIATION COGNITIVE"~"COG_REM",
                  LISTE_DOMAINE_SOIN == "8-Thérapies COGNITIVES et COMPORTEMENTALES"~"CBT",
                  # LISTE_DOMAINE_SOIN == "4-FAMILLE"~"FAM",
                  # LISTE_DOMAINE_SOIN == "13*-PAIR-AIDANCE"~"PSW",
                  LISTE_DOMAINE_SOIN == "6-COGNITION SOCIALE"~"COG_REM",#SOC_COG COG_REM
                  LISTE_DOMAINE_SOIN == "2-AUTONOMIE & OUVERTURE vers l'ESPACE SOCIAL"~"SOC_ASP",
                  LISTE_DOMAINE_SOIN == "11-ATELIERS THERAPEUTIQUES & TECHNIQUES"~"SOC_ASP",
                  LISTE_DOMAINE_SOIN == "9-RESEAU PARTENAIRE"~"SOC_ASP",
                  LISTE_DOMAINE_SOIN == "15-ESSORT"~"SOC_ASP",
                  # LISTE_DOMAINE_SOIN == "14-CASE MANAGEMENT de TRANSITION (CMT)" ~ "CMT",
                  # LISTE_DOMAINE_SOIN == "3-COMPETENCES SOCIALES" ~ "COMPET_SOC",
                  # LISTE_DOMAINE_SOIN == "10-PSYCHOTHERAPIES PSYCHODYNAMIQUES" ~ "ANAL",
                  TRUE~NA_character_),
    #CIM10=Diagnostic.psychiatrique.principal..codes.CIM10.reconnus.par.DSM5.,
    Sex=case_when(Sex=="Masculin"~"Male",
                Sex=="Féminin"~"Female",
                TRUE~NA_character_),
    MODAL_PEC=case_when(MODAL_PEC=="Groupale" | MODAL_PEC=="Individuelle" ~ as.character(MODAL_PEC),
                      TRUE~NA_character_),
    Education=case_when(as.numeric(sapply(strsplit(Education,"=",fixed=TRUE),function(x) x[1]))>=12~">= 12 years",
                        Education=="Non Cotable/Non Pertinent"~NA_character_,
                      is.character(Education) ~ "< 12 years",
                      TRUE~NA_character_),
    Employment=case_when(as.numeric(sapply(strsplit(Employment,",",fixed=TRUE),function(x) x[1]))==1~"EMPLOYED",
                       as.numeric(sapply(strsplit(Employment,",",fixed=TRUE),function(x) x[1]))==2~"EMPLOYED",
                       Employment=="99"~NA_character_,
                       is.character(Employment) ~ "UNEMPLOYED",
                       TRUE~NA_character_),
    N_Admissions=case_when(N_Admissions<=2~"<= 2",
                           N_Admissions>=3~">= 3",
                           #N_Admissions==2 | N_Admissions ==3~"2-3",
                           TRUE~NA_character_),
    First_Contact=case_when(First_Contact==">10 ans" ~ "> 10 years",
                            First_Contact=="5 à 10 ans" ~ "5 to 10 years",
                          First_Contact=="Non Cotable/Non Pertinent" ~NA_character_,
                          is.character(First_Contact)~"< 5 years",
                          TRUE~NA_character_),
    Fam=case_when(Fam=="Non demandé durant l'entretien" ~ NA_character_,
                Fam=="Marié(e)" ~ "In a relationship",
                Fam=="PACSE" ~ "In a relationship",
                Fam=="Union Libre" ~ "In a relationship",
                is.character(Fam)~"Not in a relationship",
                TRUE~NA_character_),
    Dx=case_when(Dx=="1-Troubles NEURODEVELOPPEMENTAUX" ~ "ASD",
               Dx=="18-Troubles de la PERSONNALITE" ~ "PD",
               Dx=="2-Spectre de la SCHIZOPHRENIE" ~ "SCZ",
               Dx=="3-Troubles BIPOLAIRES" ~ "BAD",
               Dx=="4-Troubles DEPRESSIFS" ~ "DEP",
               Dx=="5-Troubles de l’ANXIETE" ~ "ANX" ,
               Dx=="6-TOC" ~ "ANX",
               Dx=="7-Troubles liés à STRESS ou TRAUMATISME" ~ "ANX",
               TRUE~as.character(Dx)),
    Dx_SOMA=case_when(Dx_SOMA=="Non demandé durant l'entretien" ~ NA_character_,
                      TRUE~as.character(Dx_SOMA)),
    Addictions=case_when(Addictions=="0" ~ "Addictions-",
                         is.character(Addictions) ~ "Addictions+",
                         TRUE~NA_character_),
    Forensic=case_when(Forensic=="Non" ~ "Forensic-",
                       Forensic=="Oui" ~ "Forensic+",
                       TRUE~NA_character_),
    Marginalisation=case_when(Marginalisation=="Non" ~ "Marginalisation-",
                              Marginalisation=="Oui" ~ "Marginalisation+",
                              TRUE~NA_character_),
    RQTH=case_when(RQTH=="Demande en attente" ~ "RQTH-",
                   RQTH=="Non" ~ "RQTH-",
                   RQTH=="Oui" ~ "RQTH+",
                   TRUE~NA_character_),
    Adresseur=case_when(Adresseur=="Psychiatre libéral" ~ "Private",
                        Adresseur=="Psychologue libéral(e)" ~ "Private",
                        Adresseur=="Non demandé durant l'entretien" ~ NA_character_,
                        is.character(Adresseur) ~ "Public",
                        TRUE~NA_character_),
    STORI=case_when(STORI=="1 - MORATOIRE" ~ "MORATORIUM",
                    STORI=="2 - CONSCIENCE" ~ "REBUILDING" ,
                    STORI=="3 - PREPARATION" ~ "REBUILDING",
                    STORI=="4 - RECONSTRUCTION" ~ "REBUILDING",
                    STORI=="5 - CROISSANCE" ~ "GROWTH",
                    TRUE~NA_character_)
    
    )
#####################
#####################

#####################
#####################
#other changes
mat_A_W_final<-mat_A_W_final %>%
  mutate(across(everything(.))) %>%
  na_if("") %>%
  na_if(8888)

#when there are multiple PECs, same name = only one. Add on time spent. 
mat_A_W_rd_PEC<-mat_A_W_final %>%
  as_tibble() %>%
  group_by(StudySubjectID,PSR) %>%
  # dplyr::mutate(across(Age:Addictions,~first(na.omit(.)))) %>%
  # dplyr::mutate(across(SQoL18_SEL:D2R_CC,~first(.))) %>%
  # mutate(across(c("Age":"ACSo_TOT"),~first(na.omit(.)))) %>%
  #mutate(across("NB_HEURES",~sum(na.omit(NB_HEURES)))) %>%
  distinct(PSR,.keep_all = TRUE)

#sort by date
#lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", lct)
Sys.setlocale("LC_TIME","French")
mat_A_W_rd_PEC <- mat_A_W_rd_PEC %>%
  group_by(StudySubjectID) %>%
  mutate(DATE_DEB=as.Date(DATE_DEB,"%d-%B-%y")) %>%
  arrange(StudySubjectID,DATE_DEB)


#Sort PEC per pat
mat_A_W_rd_PEC<-mat_A_W_rd_PEC %>% 
  dplyr::group_by(StudySubjectID) %>%
  dplyr::mutate(N_PEC=row_number()) 
hist(mat_A_W_rd_PEC$N_PEC)


### 6 Dx
mat_A_W_rd_PEC_6dx <- mat_A_W_rd_PEC %>%
  filter(Dx=="ASD" |  Dx=="SCZ" |  Dx=="DEP" |  Dx=="BAD" |  Dx=="PD" |  Dx=="ANX" )

##pivot_wider one row per subject
mat_A_W_rd_PEC_6dx<-mat_A_W_rd_PEC_6dx %>%
  group_by(StudySubjectID) %>%
  pivot_wider(id_cols=StudySubjectID,
    names_from = PSR, 
    values_from = N_PEC,
    unused_fn=first)
table(mat_A_W_rd_PEC_6dx$Dx)
table(mat_A_W_rd_PEC_6dx$CBT,useNA = "ifany");table(mat_A_W_rd_PEC_6dx$COG_REM,useNA = "ifany");
table(mat_A_W_rd_PEC_6dx$SOC_ASP,useNA = "ifany");table(mat_A_W_rd_PEC_6dx$EDUC,useNA = "ifany");

######################
######################

################################
################################
#make factor/ordinal factors
mat_A_W_rd_PEC_6dx$StudySubjectID=factor(mat_A_W_rd_PEC_6dx$StudySubjectID)
#mat_A_W_rd_PEC_6dx$PSR=factor(mat_A_W_rd_PEC_6dx$PSR)

mat_A_W_rd_PEC_6dx$Sex=factor(mat_A_W_rd_PEC_6dx$Sex)
mat_A_W_rd_PEC_6dx$Education=factor(mat_A_W_rd_PEC_6dx$Education)
mat_A_W_rd_PEC_6dx$Employment=factor(mat_A_W_rd_PEC_6dx$Employment)
mat_A_W_rd_PEC_6dx$ProtocolID=factor(mat_A_W_rd_PEC_6dx$ProtocolID)
mat_A_W_rd_PEC_6dx$RQTH=factor(mat_A_W_rd_PEC_6dx$RQTH)
mat_A_W_rd_PEC_6dx$Fam=factor(mat_A_W_rd_PEC_6dx$Fam)
mat_A_W_rd_PEC_6dx$Adresseur=factor(mat_A_W_rd_PEC_6dx$Adresseur)
mat_A_W_rd_PEC_6dx$Marginalisation=factor(mat_A_W_rd_PEC_6dx$Marginalisation)
mat_A_W_rd_PEC_6dx$Forensic=factor(mat_A_W_rd_PEC_6dx$Forensic)
mat_A_W_rd_PEC_6dx$Dx=factor(mat_A_W_rd_PEC_6dx$Dx)
mat_A_W_rd_PEC_6dx$Dx2=factor(mat_A_W_rd_PEC_6dx$Dx2)
mat_A_W_rd_PEC_6dx$Dx_SOMA=factor(mat_A_W_rd_PEC_6dx$Dx_SOMA)
mat_A_W_rd_PEC_6dx$Addictions=factor(mat_A_W_rd_PEC_6dx$Addictions)


# mat_A_W_rd_PEC_6dx$N_PEC_bin=factor(mat_A_W_rd_PEC_6dx$N_PEC, 
#                                     order = FALSE, 
#                                     levels = c("N_PEC_1", "N_PEC_2", "N_PEC_3", "N_PEC_4")
# )

mat_A_W_rd_PEC_6dx$N_Admissions <- factor(mat_A_W_rd_PEC_6dx$N_Admissions, 
                                          order = FALSE, #TRUE  makes it an ordinal variable
                                          #levels = c("< 2","2-3","> 3"))
                                          levels = c("<= 2",">= 3"))

mat_A_W_rd_PEC_6dx$First_Contact <- factor(mat_A_W_rd_PEC_6dx$First_Contact, 
                                    order = FALSE, #this makes it an ordinal variable
                                    levels = c("< 5 years","5 to 10 years","> 10 years"))

mat_A_W_rd_PEC_6dx$STORI <- factor(mat_A_W_rd_PEC_6dx$STORI, 
                                           order = FALSE, #this makes it an ordinal variable
                                           levels = c("MORATORIUM",
                                                      "REBUILDING",
                                                      "GROWTH"))
#IS_Sympt
#IS_Disease

#no need to transform the below into factors
#WEMWBS
#SQoL
#ISMI
#IS_TOT
################################
################################

#######################
#######################
mat_A_W_rd_PEC_6dx_MHC_linear_cc <- mat_A_W_rd_PEC_6dx %>% 
  ungroup() %>%
  
  dplyr::select(c(StudySubjectID,
           #PSR,N_PEC,
           CBT,SOC_ASP,COG_REM,EDUC,
           Age,
           Sex,Education,Employment,ProtocolID,
           RQTH,Fam,Adresseur,Marginalisation,Forensic,
           Dx,Dx2,Dx_SOMA,Addictions,
           GAF,CGI,#N_PEC_bin,
           First_Contact,N_Admissions,
           
           SQoL18_SEL:SQoL18_TOT,
           WEMWBS_TOT,
           ISMI_Alien:ISMI_TOT,
           IS_Sympt:IS_TOT,
           STORI)) %>%
  dummy_cols( #dummyfy
    select_columns = c("Sex","Education","Employment","ProtocolID","RQTH","Fam","Adresseur",   
                       "Marginalisation","Forensic","Dx","Dx2","Dx_SOMA","Addictions",#"N_PEC_bin",
                       "N_Admissions","First_Contact"
                       ,"STORI"
    ),
    remove_first_dummy = TRUE,#to avoid multicol
    remove_most_frequent_dummy = FALSE,
    ignore_na = TRUE,
    split = NULL,
    remove_selected_columns = TRUE#removes the columns used to generate the dummy columns
  )

#######################
#######################
#all dummies
numbers_of_bins=2
mat_A_W_rd_PEC_6dx_MHC_alldum_pre <- mat_A_W_rd_PEC_6dx %>% 
  ungroup() %>%
  
  dplyr::select(c(StudySubjectID,
    #PSR,N_PEC,
    CBT,SOC_ASP,COG_REM,EDUC,
    Age,
    Sex,Education,Employment,ProtocolID,
    RQTH,Fam,Adresseur,Marginalisation,Forensic,
    Dx,Dx2,Dx_SOMA,Addictions,
    GAF,CGI,#N_PEC_bin,
    First_Contact,N_Admissions,
    
    SQoL18_SEL:SQoL18_TOT,
    WEMWBS_TOT,
    ISMI_TOT,
    IS_TOT,
    STORI)) %>%
  
  mutate(across(c(Age,GAF,CGI,
                  SQoL18_SEL:SQoL18_TOT,
                  WEMWBS_TOT,
                  ISMI_TOT,
                  IS_TOT
                  ), ~ cut(.x,
                           breaks = numbers_of_bins,
                           labels = FALSE,
                           include.lowest = TRUE),
                           )) %>%
  mutate(across(c(Age,GAF,CGI,
                  SQoL18_SEL:SQoL18_TOT,
                  WEMWBS_TOT,
                  ISMI_TOT,
                  IS_TOT
                  ), ~as.character(.x)))
  
  # mutate(Age = case_when(Age <=35 ~ as.character(0),
  #                       Age >35 ~ as.character(1)),
  #        GAF = case_when(GAF <=60 ~ as.character(0),
  #                        GAF>60 ~ as.character(1)),
  #        CGI = case_when(CGI<4 ~ as.character(0),
  #                        CGI>=4 ~ as.character(1)),
  #        across(c(SQoL18_SEL:SQoL18_TOT), ~ case_when(.x<=50 ~ as.character(0),
  #                                                     .x>50 ~ as.character(1))),
  #        WEMWBS_TOT =case_when(WEMWBS_TOT<=42 ~ as.character(0),
  #                              WEMWBS_TOT>42 ~ as.character(1)),
  #        ISMI_TOT = case_when(ISMI_TOT<=2.5 ~ as.character(0),
  #                             ISMI_TOT>2.5 ~ as.character(1)),
  #        IS_TOT= case_when(IS_TOT<=9 ~ as.character(0),
  #                          IS_TOT>9 ~ as.character(1))
  #        ) %>%
# mutate(across(c(Age,GAF,CGI,
#                 SQoL18_SEL:SQoL18_TOT,
#                 WEMWBS_TOT,
#                 ISMI_TOT,
#                 IS_TOT
# ), ~as.character(.x)))




mat_A_W_rd_PEC_6dx_MHC_alldum_nodum <- dplyr::select(mat_A_W_rd_PEC_6dx_MHC_alldum_pre,
                                       c(StudySubjectID,
                                         #PSR,N_PEC
                                         CBT,SOC_ASP,COG_REM,EDUC
                                         ))

#function to find the mode
find_mode <- function(x) {
  u <- unique(x[!is.na(x)])
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

mat_A_W_rd_PEC_6dx_MHC_alldum_dum_NA <- dummy_cols(
    .data=  dplyr::select(mat_A_W_rd_PEC_6dx_MHC_alldum_pre,Age:STORI),
    select_columns = NULL,
    remove_first_dummy = TRUE,#to avoid multicol
    remove_most_frequent_dummy = FALSE,
    ignore_na = FALSE,
    split = NULL,
    remove_selected_columns = TRUE#removes the columns used to generate the dummy columns
  ) %>%
  mutate(across(everything(.),
                ~ replace(., is.na(.), find_mode(.))
                ))

mat_A_W_rd_PEC_6dx_MHC_alldum_dum_cc <- dummy_cols(
  .data=dplyr::select(mat_A_W_rd_PEC_6dx_MHC_alldum_pre,Age:STORI),
  select_columns = NULL,
  remove_first_dummy = TRUE,#to avoid multicol
  remove_most_frequent_dummy = FALSE,
  ignore_na = TRUE,
  split = NULL,
  remove_selected_columns = TRUE#removes the columns used to generate the dummy columns
) 

mat_A_W_rd_PEC_6dx_MHC_alldum_cc=cbind(mat_A_W_rd_PEC_6dx_MHC_alldum_nodum,
                             mat_A_W_rd_PEC_6dx_MHC_alldum_dum_cc)

mat_A_W_rd_PEC_6dx_MHC_alldum_NA=cbind(mat_A_W_rd_PEC_6dx_MHC_alldum_nodum,
                                       mat_A_W_rd_PEC_6dx_MHC_alldum_dum_NA)


################################
################################


########################
########################
#prepare matrix for mice

#You'll impute missing cases from the DOC complete cases matrix
# tmp <- mat_A_W_rd_PEC_6dx_MHC[complete.cases(mat_A_W_rd_PEC_6dx_DOC),]
# colSums(is.na(tmp))
# 
# matrix_mice <- tmp
# 
# #transform char to factor
# # all_categ <- matrix_mice %>% 
# #   dplyr::select(where(is.character))  %>%
# #   lapply(factor) %>%
# #   as.data.frame() 
# # 
# # all_cont <- matrix_mice %>%
# #   dplyr::select(where(is.numeric)) 
# # 
# # matrix_mice <- all_categ %>% cbind(all_cont)
# 
# #impute with mice
# 
# system.time(
#   impute <- parlmice(matrix_mice
#                    ,m=5 
#                    ,nnet.MaxNWts = 4500
#                    ,n.core = 4
#                    #,n.imp.core = 150
#                    )
#   )



###############################       
###############################


##########
#SAVE
##########
save(mat_A_W_rd_PEC,
	 mat_A_W_rd_PEC_6dx ,
	 #mat_A_W_rd_PEC_6dx_MHC_linear_cc,
	 #mat_A_W_rd_PEC_6dx_MHC_alldum_cc,
	 mat_A_W_rd_PEC_6dx_MHC_alldum_NA,
	 
	 file="mat_A_W_rd_PEC7bis.RData")

# save(impute, file="imputed.RData")
##########