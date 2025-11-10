rm(list=ls())
library(rstudioapi)
current_path<-getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

# Caricamento dati
data <- read_excel("Marketing_Campaign.xlsx", sheet = "Features")
targets <- read_excel("Marketing_Campaign.xlsx", sheet = "Targets")
data <- data.frame(data,targets)
data <- data[,-21]
######## DATA QUALITY ###########
# somma per colonna tutti gli na di data:
colSums(is.na(data))

# Vediamo se ci sono righe duplicate
duplicated(data)
# andiamo a vedere quale riga è duplicata
which(duplicated(data))
# Non ci sono righe duplicate

summary(data)
# Sostituiamo 999 della variabile pdays con -1
data$pdays[which(data$pdays==999)] = -1

## Trasformare in dummy o categoriche le variabili che sono tali
data$House.Ownershi<-ifelse(data$House.Ownershi == "yes", 1, 0)
data$House.Ownershi<- as.factor(data$House.Ownershi)
data$Existing.Loans<-ifelse(data$Existing.Loans == "yes", 1, 0)
data$Existing.Loans<- as.factor(data$Existing.Loans)
data$Marital.Status<-as.factor(data$Marital.Status)
data$Previous.Default<-ifelse(data$Previous.Default == "no", 0, ifelse(data$Previous.Default == "yes", 1, -1 ))
data$Previous.Default<-as.factor(data$Previous.Default)
data$Contact.Channel <- as.factor(data$Contact.Channel)
data$poutcome<-as.factor(data$poutcome)
data$Target<-ifelse(data$Target == 1, 1, 0)
data$Target<-as.factor(data$Target)
data$Month<-as.factor(data$Month)
data$Day.of.Week<-as.factor(data$Day.of.Week)
data$Marital.Status <- as.factor(data$Marital.Status)
data$Job <- as.factor(data$Job)
data$Education <- as.factor(data$Education)

str(data)

# BOXPLOT --- vediamo se ci sono outliers
dev.new()
par(mfrow=c(1,2))
boxplot(data$Age, col="lightgreen", main="Age")
boxplot(data$Call.Duration, col="lightgreen", main="Call Duration") 
dev.new()
par(mfrow=c(1,2))
boxplot(data$pdays, col="lightgreen", main="pdays")
boxplot(data$previous, col="lightgreen", main="previous")
dev.new()
par(mfrow=c(1,2))
boxplot(data$emp_var_rate, col="lightgreen", main="emp_var_rate")
boxplot(data$cons_price_idx, col="lightgreen", main="cons_price_idx")
par(mfrow=c(1,3))
boxplot(data$cons_conf_idx, col="lightgreen", main="cons_conf_idx")
boxplot(data$euribor3m, col="lightgreen", main="euribor3m")
boxplot(data$Target, col="lightgreen", main="Target")


################## ANALISI ESPLORATIVA ###################

# Vediamo la frequenza assoluta di "Target"
tab<-data$Target%>%table()
tab
# Vediamo la frequenza relativa di "Target"
percentages<-tab%>%prop.table()%>%round(3)*100
percentages ## 88.7% --> 0 e 11.3% --> 1

# Raprresentiamo la frequenza relativa di Target con un grafico a torta
dev.new()
txt<-paste0(names(tab), '\n',percentages, '%')
txt
colors <- c("violet","orange")
pie(tab, labels = txt, col = colors)
legend("topright", legend = names(tab), fill = colors, title = "Target")


#### CORRELAZIONE TRA LE VARIABILI
data_aux <- data[,-c(1,3:11,16,21:24)]
dev.new()
matrcorr=cor(data_aux)
library(ggcorrplot)
#ggcorrplot(matrcorr)
library(corrplot)
corrplot(matrcorr, method = "color", type ="lower",
         addCoef.col = "black", # colore dei coefficienti
         number.cex = 0.7)

new_names <- c("Age", "Call Duration", "Current Contacts", "Days since last contact", "Previous Contacts", "Employment Variation Rate", "Consumer Price Index", "Consumer Confidence Index", "Euribor 3-month")  # Personalizza i nomi
colnames(matrcorr) <- new_names
rownames(matrcorr) <- new_names
# Plotta la heatmap della correlazione
ggcorrplot(matrcorr, 
           method = "square",          # Visualizzazione a quadrati colorati
           type = "lower",             # Mostra solo la parte inferiore per pulizia
           lab = TRUE,                 # Mostra i valori di correlazione
           lab_size = 5,                # Dimensione dei numeri sulla mappa
           colors = c("darkred", "white", "royalblue"),  # Scala di colori viola/lilla
           outline.color = "white",    # Rende la griglia più chiara
           title = "Matrice di Correlazione delle Variabili Numeriche",
           legend.title = "Correlazione",
           show.diag = TRUE) +
  theme(
    # Modifica i numeri in grassetto
    element_text(face = "bold")
  )
library(ggplot2)


# Creiamo le fasce d'età
data$Age_Group <- cut(data$Age, 
                      breaks = c(0, 30, 40, 50, 60, 100), 
                      labels = c("<=30", "31-40", "41-50", "51-60", "60+"), 
                      right = TRUE)

# Controlliamo il risultato
table(data$Age_Group)

# Seleziona alcune variabili categoriche chiave
cat_vars <- c("Job", "Education", "Marital.Status", "Age_Group", "Previous.Default", "House.Ownershi", "Existing.Loans")

# Converti in formato lungo per ggplot
df_long <- reshape2::melt(data, id.vars = "Target", measure.vars = cat_vars)

# Plotta il grafico con più barplot affiancati
ggplot(df_long, aes(x = value, fill = as.factor(Target))) +
  geom_bar(position = "fill") +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Distribuzione della Target per Variabili Categoriche", y = "Proporzione", x = "Categorie") +
  theme_minimal() +
  scale_fill_manual(values = c("red", "green"), name = "Target", labels = c("No", "Yes")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

cliente1 <- data.frame(Marital.Status = character(), stringsAsFactors = FALSE)
cliente1 <- rbind(cliente1, data.frame(Marital.Status = "neutral"))
cliente1$Age <- "60+"
cliente1$Marital.Status <- "neutral"
cliente1$Education <- "illiterate"
cliente1$Job <- "retired"
cliente1$Previous.Default <- 0
cliente1$House.Ownershi <- "neutral"
cliente1$Existing.Loans <-"neutral"


library(writexl)
data$Month <- ifelse(data$Month == "jan", 1, ifelse(data$Month == "feb", 2, ifelse(data$Month == "mar", 3, ifelse(data$Month == "apr", 4, ifelse(data$Month == "may", 5, ifelse(data$Month == "jun", 6,ifelse(data$Month == "jul", 7,ifelse(data$Month == "aug", 8, ifelse(data$Month == "sep", 9, ifelse(data$Month == "oct", 10, ifelse(data$Month == "nov", 11, 12)))))))))))


library(dplyr)

# Creazione della colonna con le macro categorie
data <- data %>%
  mutate(job_category = case_when(
    Job %in% c("blue-collar", "housemaid", "services") ~ "Lavori Manuali e Operativi",
    Job %in% c("technician", "admin.") ~ "Professioni Tecnico-Amministrative",
    Job %in% c("management", "entrepreneur", "self-employed") ~ "Ruoli Dirigenziali e Imprenditoriali",
    Job %in% c("retired", "unemployed", "student", "unknown") ~ "Non Occupati e Altro",
    TRUE ~ "Altro" # Per gestire eventuali nuovi valori
  ))

# visualizzo tutte le categorie
unique(data$job_category)
# Creazione del mapping manuale delle categorie a numeri
data$job_category <- factor(data$job_category, 
                            levels = c("Lavori Manuali e Operativi", 
                                       "Professioni Tecnico-Amministrative", 
                                       "Ruoli Dirigenziali e Imprenditoriali", 
                                       "Non Occupati e Altro"))


data <- data %>%
  mutate(education_category = case_when(
    Education %in% c("illiterate", "basic.4y", "basic.6y") ~ "Bassa Istruzione",
    Education %in% c("basic.9y", "high.school") ~ "Istruzione Media",
    Education %in% c("professional.course", "university.degree") ~ "Istruzione Superiore",
    Education == "unknown" ~ "Sconosciuto",
    TRUE ~ "Altro"
  ))

# Creazione del mapping manuale delle categorie a numeri
data$education_category <- factor(data$education_category, 
                                  levels = c("Bassa Istruzione", 
                                             "Istruzione Media", 
                                             "Istruzione Superiore", 
                                             "Sconosciuto"))

# Controllo della distribuzione
table(data$education_category)

write_xlsx(data, "dataset_ass3.xlsx")

# Call duration maggiore porta ad una maggiore probabilità di dire yes
# House Ownership non influenza il valore della variabile target

#### Facciamo UNDERSAPMPLING DELLA CLASSE Target = 0
library(caret)
library(e1071)
library(ROSE)

# Applichiamo l'undersampling stratificato con downSample()
set.seed(123)  # Per riproducibilità
new_data <- downSample(x = data[, -which(names(data) == "Target")],  
                       y = as.factor(data$Target))

# Rinominiamo la variabile target
colnames(new_data)[ncol(new_data)] <- "Target"

# Controlliamo il nuovo bilanciamento
table(new_data$Target)


########### REGRESSIONE LOGISTICA #############

# Dividiamo il set di dati in training e test set
library(caTools)
set.seed(123)
sample_data<-sample.split(new_data, SplitRatio = 0.60)
train_data<-subset(new_data, sample_data==TRUE)
test_data<-subset(new_data, sample_data==FALSE)
prop.table(table(train_data$Target))
prop.table(table(test_data$Target))

train_data1<-train_data[-1642,]

# Calcola l'IQR per la variabile Call.Duration
Q1 <- quantile(train_data1$Call.Duration, 0.25)
Q3 <- quantile(train_data1$Call.Duration, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 5 * IQR_value
upper_bound <- Q3 + 5 * IQR_value

# Filtra il dataset rimuovendo gli outlier
train_data2 <- subset(train_data1, Call.Duration > lower_bound & Call.Duration < upper_bound)
View(train_data2)
boxplot(train_data2$Call.Duration, col="lightgreen", main="Call.Duration")


# Calcola l'IQR per la variabile Call.Duration
Q1 <- quantile(train_data2$campaign, 0.25)
Q3 <- quantile(train_data2$campaign, 0.75)
IQR_value <- Q3 - Q1

# Identifica gli outlier
lower_bound <- Q1 - 5 * IQR_value
upper_bound <- Q3 + 5 * IQR_value

# Filtra il dataset rimuovendo gli outlier
train_data3 <- subset(train_data2, campaign > lower_bound & campaign < upper_bound)
View(train_data3)
boxplot(train_data3$campaign, col="lightgreen", main="campaign")

#train_data3_scaled <- scale(train_data3[,-c(1,3:11,16,21:22)])
#train_data_reg<-data.frame(train_data3[,1],train_data3_scaled[,1], train_data3[,3:11],train_data3_scaled[,2:5], train_data3[,16], train_data3_scaled[,6:9], train_data3[,21], train_data3$Target)
#names(train_data_reg) <- names(train_data3)

train_data3$Marital.Status <- relevel(train_data3$Marital.Status, ref = "single")

library(car)
library(lmtest)
mod0=glm( Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Existing.Loans + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + euribor3m + job_category + education_category, family = binomial( link = logit ) , data = train_data3)
summary(mod0)
vif(mod0)

# Toglo euribor
mod1=glm(Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Existing.Loans + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category + education_category, family = binomial( link = logit ) , data = train_data3)
summary(mod1)
vif(mod1)
lrtest(mod0,mod1) # accettiamo H0 ossia il modello con meno regressori (modello 1)

# Tolgo education category
mod2=glm(Target ~ Age + Marital.Status + Previous.Default + House.Ownershi+ Existing.Loans + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod2)
vif(mod2)
lrtest(mod1,mod2) # accettiamo H0 (modello 2 è più significativo)

# Tolgo Existing Loans
mod3=glm( Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Month + Call.Duration + campaign + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod3)
vif(mod3)
lrtest(mod2,mod3) # accettiamo H0 (il modello 3 è più significativo)

# Tolgo campaign
mod4=glm( Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Month + Call.Duration + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod4)
vif(mod4)
lrtest(mod3,mod4) # accettiamo H0 (il modello 4 è più significativo)

# Tolgo Month
mod5=glm( Target ~ Age + Marital.Status + Previous.Default + House.Ownershi + Call.Duration + pdays + previous + emp_var_rate + cons_conf_idx + cons_price_idx + job_category, family = binomial( link = logit ) , data = train_data3)
summary(mod5)
vif(mod5)
lrtest(mod4,mod5) # accettiamo H0 (il modello 5 è più significativo)

#### MODELLO 5 DEFINITIVO 

#### IPOTESI DA VERIFICARE ####

# Calcola i residui Pearson dal modello logit
residuals_pearson <- residuals(mod5, type = "pearson")

# Esegue il test di Durbin-Watson sui residui
durbinWatsonTest(residuals_pearson)

library(stats)

# Test di Ljung-Box per autocorrelazione sui residui Pearson
Box.test(residuals_pearson, type = "Ljung-Box")


## influential values
dev.new()
plot(mod5, which=4, id.n = 10)

library(car)
dev.new()
influencePlot(mod5)

# Vediamo se c'è overfitting
# Accuracy del training
# Accuracy del testing
predictions<-predict(mod5, train_data3, type="response")

predictions.hat<-ifelse(predictions>0.5,1,0)

accuracy_test<-table(predictions.hat, train_data3$Target)
accuracy_test
sum(diag(accuracy_test))/sum(accuracy_test)*100 # accuracy=85.25%


# Accuracy del testing
predictions<-predict(mod5, test_data, type="response")

predictions.hat<-ifelse(predictions>0.5,1,0)

accuracy_test<-table(predictions.hat, test_data$Target)
accuracy_test
sum(diag(accuracy_test))/sum(accuracy_test)*100 # accuracy=85.90%

library(pscl)
pR2(mod5) ## 0.53 quindi molto buono

library(pROC)
# Calcola la curva ROC
roc_curve <- roc(test_data$Target, predictions.hat)

# Plotta la curva ROC
plot(roc_curve, col = "blue", main = "Curva ROC")

# Stampa l'AUC
auc(roc_curve)

################### ANALISI DELLE CATEGORIE PIU' PROPENSE #################

# Analizzare chi accetta più frequentemente (media della variabile Target per gruppo)

data %>% group_by(job_category) %>% summarise(AcceptanceRate = mean(Target)) %>% arrange(desc(AcceptanceRate))


data %>% 
  group_by(education_category) %>% 
  summarise(AcceptanceRate = mean(Target, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(AcceptanceRate))


data %>% 
  group_by(Age_Group) %>% 
  summarise(AcceptanceRate = mean(Target, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(AcceptanceRate))


data %>% 
  group_by(Marital.Status) %>% 
  summarise(AcceptanceRate = mean(Target, na.rm = TRUE), .groups = "drop") %>% 
  arrange(desc(AcceptanceRate))


