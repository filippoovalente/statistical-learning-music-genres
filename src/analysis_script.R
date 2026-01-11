
# LIBRERIE ----------------------------------------------------------------

library(mclust)
library(tidyverse)
library(spotifyr)
library(httr)
library(jsonlite)
library(caret)
library(e1071)
library(GGally)
library(ggcorrplot)
library(Rmixmod)

# ACCESS TOKEN SPOTIFY -----------------------------------------------------

Sys.setenv(SPOTIFY_CLIENT_ID = '0856fbc1255049cdbd2e45a40f3e244d')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'f35ea6d6ae18419e963806afbaa188a3')
access_token <- get_spotify_access_token()


# CODICE OPEN-SOURCE RICAVATO DA GITHUB -----------------------------------

get_tracklist <- function(artist_id) {
  # Get all of the album names from a particular artist
  artist_albums <- get_artist_albums(id = artist_id)
  
  message("Albums found.")
  
  # Loop through all albums to get track ids
  artist_album_ids <- artist_albums$id
  
  # Get the first album to create the structure of the dataset
  artist_tracks <- spotifyr::get_album_tracks(id = artist_albums$id[1]) %>% 
    mutate(album = artist_albums$name[1],
           release_date = artist_albums$release_date[1]) # add name and release of album to tracks tibble
  
  # Remaining tracks
  for (i in 2:length(artist_album_ids)) {
    
    add_tracks <- get_album_tracks(id = artist_albums$id[i]) %>% 
      mutate(album = artist_albums$name[i],
             release_date = artist_albums$release_date[i])
    artist_tracks <- bind_rows(artist_tracks, add_tracks) # bind new tracks to old
    Sys.sleep(1) # rate limiter
    
  }
  
  message("Tracklist downloaded.")
  return(artist_tracks)
}


batch_requests <- function (tracks) {
  
  # Use ReccoBeats API to fetch audio features (acousticness, loudness, danceability, etc.)
  
  # API accepts comma separated track IDs from Spotify IDs or ReccoBeats IDS
  # https://reccobeats.com/docs/documentation/request-and-response 
  
  # Currently, the API can only return up to 40 tracks per request
  
  # This function batches a dataset into chunks of 40 tracks
  
  request_range <- seq(0, 400, 40)
  request_tracks <- list()
  
  for (i in 1:11) {
    
    # Split request range up into chunks of 40 
    request_tracks[[i]] <- tibble(tracks)[(request_range[i]+1):(request_range[i+1]), ]
    request_tracks[[i]] <- request_tracks[[i]] %>% drop_na(id)
    
    # break the loop when all of the request subsets have been allocated
    if (nrow(request_tracks[[i]])==0) break
    
  }
  
  # Remove the excess empty list item
  request_tracks[[length(request_tracks)]] <- NULL
  
  return(request_tracks)
  
}

recco_requests <- function(batches) {
  
  # Make requests one batch at a time to reccobeats
  
  headers = c(
    'Accept' = 'application/json'
  )
  
  recco_results <- list()
  
  for (i in 1:length(batches)) {
    
    url <- paste0("https://api.reccobeats.com/v1/audio-features?ids=", 
                  paste0(batches[[i]]$id, collapse = ","))
    
    res <- VERB("GET", 
                url = url,
                add_headers(headers))
    
    # convert results into a tibble and save it to the results list
    recco_results[[i]] <- as_tibble(fromJSON(content(res, "text"))$content)
    
    # rate limiter
    Sys.sleep(2)
    
  }
  
  return(recco_results)
  
}


recco_popularity <- function(results) {
  
  # Use a results from Recco list to fetch the popularity of tracks 
  
  headers = c(
    'Accept' = 'application/json'
  )
  
  recco_popularity <- list()
  
  for (i in 1:length(results)) {
    
    url <- paste0("https://api.reccobeats.com/v1/track?ids=", paste0(results[[i]]$id, collapse = ","))
    
    res <- VERB("GET", 
                url = url,
                add_headers(headers))
    
    # convert results into a tibble and save it to the results list
    recco_popularity[[i]] <- as_tibble(fromJSON(content(res, "text"))$content) %>%
      select(
        id, popularity
      )
    
    # rate limiter
    Sys.sleep(2)
    
  }
  
  return(recco_popularity)
  
}

get_recco_artist_track_features <- function(artist_name, 
                                            artist_id) {
  
  # Get the complete list of track names from all albums
  # by the artist
  artist_tracks <- get_tracklist(artist_id)
  
  # Group the tracks into sets of 40 for making requests to ReccoBeats
  artist_batches <- batch_requests(artist_tracks)
  
  # Make calls to ReccoBeats API to download track audio features
  artist_results <- recco_requests(artist_batches)
  message("Recco audio features downloaded.")
  
  artist_track_popularity <- recco_popularity(artist_results)
  message("Recco track popularity downloaded.")
  
  # Now all lists can be flattened, tidied, and joined
  track_info <- bind_rows(artist_batches) %>% mutate(artist = artist_name, .before = artists)
  track_features <- bind_rows(artist_results) %>% 
    rename(recco_id = id) %>% 
    mutate(
      # Create spotify ID from the url link to the track
      spotify_id = str_remove(href, "https://open.spotify.com/track/")
    ) %>%
    select(-href)
  track_popularity <- bind_rows(artist_track_popularity) %>%
    rename(recco_id = id)
  
  artist_data <- left_join(track_info, 
                           track_features, 
                           by = c("id" = "spotify_id")) %>%
    left_join(., track_popularity, by = "recco_id")
  
  message("Complete.")
  return(artist_data)
  
}


# COSTRUZIONE AUTOMATICA DEL DATASET --------------------------------------

artist_name <- c("Marc Arcadipane", "Usher", "Ludovico Einaudi",
                 "Ólafur Arnalds","Michael Jackson", "The DJ Producer",
                 "Fantasm")
artist_id <- c("2hyRTXUyfd56j4siLF4zJx", "23zg3TcAtWQy7J6upgbUnj",
               "2uFUBdaVGtyMqckSeCl0Qj","7E3BRXV9ZbCt5lQTCXMTia",
               "3fMbdgg4jU18AjLCKBhRSm","5K4DYOZmv58mpArt4NCumc",
               "0copVQkrcbfv5CzOyXuLKy")
df <- data.frame()
for(k in seq_along(artist_name)) {
  data <- get_recco_artist_track_features(
    artist_name = artist_name[k],
    artist_id   = artist_id[k]
  )
  df <- bind_rows(df, data)
}

# COSTRUZIONE DATASET -----------------------------------------------------

df$release_date <- as.Date(df$release_date)

df <- df %>% 
  arrange(release_date)%>%
  distinct(name,.keep_all = T)

canzoni <- df %>% 
  select(name, artist, popularity, acousticness, danceability, 
         energy,liveness, loudness, tempo, valence, 
         duration_ms) %>%
  na.omit

canzoni <- canzoni %>%
  mutate(duration_min=duration_ms/60000)

canzoni <- canzoni[,-11]

canzoni$label[canzoni$artist %in% c("Marc Arcadipane", "The DJ Producer","Fantasm")] <- "Elettronico"
canzoni$label[canzoni$artist %in% c("Usher", "Michael Jackson")] <- "Pop"
canzoni$label[canzoni$artist %in% c("Ludovico Einaudi","Ólafur Arnalds")] <- "Neoclassico"


# 1. LIBRERIE
library(tidyverse)
library(mclust)
library(caret)
library(e1071)
library(GGally)
library(ggcorrplot)

# STANDARDIZZAZIONE -------------------------------------------------------

vars_to_scale <- c(
  "acousticness", "danceability", "energy", "liveness", 
  "loudness", "tempo", "valence", "duration_min", "popularity"
)

canzoni_scaled <- canzoni %>%
  mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.x))))
summary(canzoni_scaled)


# PREPARAZIONE DATI -------------------------------------------------------

canzoni_label   <- canzoni_scaled %>% select(-c(1, 2))
canzoni_no_label <- canzoni_scaled %>% select(-c(1, 2, 12))


ggplot(canzoni_label, aes(x = label, y = acousticness, fill = label)) +
  geom_boxplot(outlier.alpha = 0.4) +
  labs(
    x = "Genere musicale",
    y = "acousticness"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 11)
  )

# SELEZIONE VARIABILI ---------------------------------------------

corr <- round(cor(canzoni_no_label, use="pairwise.complete.obs"),3)
ggcorrplot(corr)

pca <- princomp(canzoni_no_label, cor = TRUE)
sum((pca$sdev[1:6])^2) / ncol(canzoni_no_label)
pca$loadings[ , 1:6]
apply(pca$loadings[,1:6], 2,function(x) names(canzoni_no_label)[which.max(x^2)])


features <- c("acousticness", "popularity", "duration_min", "tempo", "valence")
canzoni_label <- canzoni_label %>% select(all_of(features), label)
canzoni_no_label <- canzoni_no_label %>% select(all_of(features))


# MODEL BASED CLUSTERING --------------------------------------------------

model <- Mclust(canzoni_no_label, G = 3)
summary(model)
str(model)
model$BIC
mclustICL(canzoni_no_label, G = 3)

cluster_raw <- model$classification
true_label <- factor(canzoni_label$label,
                     levels = c("Elettronico", "Pop", "Neoclassico"))

# Errore "grezzo"
CER <- classError(cluster_raw, true_label)


# DISTANZA DI KULLBACK-LEIBLER --------------------------------------------

KL_sym_prof <- function(mu_i, Sigma_i, mu_j, Sigma_j) {
  d <- length(mu_i)
  
  diff <- mu_i - mu_j
  
  term_mean <- 0.5 * t(diff) %*%
    (solve(Sigma_i) + solve(Sigma_j)) %*%
    diff
  
  term_cov <- 0.5 * sum(diag(
    Sigma_i %*% solve(Sigma_j) +
      Sigma_j %*% solve(Sigma_i)
  ))
  
  as.numeric(term_mean + term_cov - d)
}

mus    <- model$parameters$mean          # dimensione: d × 3
Sigmas <- model$parameters$variance$sigma # dimensione: d × d × 3


G <- 3
KL_matrix <- matrix(0, nrow = G, ncol = G)

for (i in 1:G) {
  for (j in 1:G) {
    KL_matrix[i, j] <-
      KL_sym_prof(
        mus[, i], Sigmas[, , i],
        mus[, j], Sigmas[, , j]
      )
  }
}

rownames(KL_matrix) <- colnames(KL_matrix) <- c("Pop","Neoclassico", "Elettronico")
KL_matrix

# UNCERTAINTY -------------------------------------------------------------

summary(model$uncertainty)

# ENTROPIA ----------------------------------------------------------------

z <- model$z
entropy_total <- -sum(z[z > 0] * log(z[z > 0]))
entropy_total



# VISUALIZZAZIONE GRAFICA -------------------------------------------------


plot(model, what = "classification")

#pdf("confronto.pdf", width = 14, height = 8)
par(mfrow = c(1,2))

#par(
#  bg = "black",
#  fg = "white",
#  col.axis = "white",
#  col.lab = "white",
#  col.main = "white",
#  col.sub = "white"
#)

coordProj(
  data = as.data.frame(canzoni_no_label),
  dimens = c(1,4),
  what = "classification",
  classification = true_label,
  color = c("lightsalmon3", "mediumpurple", "khaki3"),
  symbol = c(16,16,16),
  sub = "(a) True classification"
)


coordProj(
  data = as.data.frame(canzoni_no_label),
  dimens = c(1,4),
  what = "classification",
  classification = cluster_raw,
  col = c("mediumpurple", "khaki3","lightsalmon3"),
  symbols = c(16,16,16),
  sub = "(b) Model-based Clustering"
)

misclass <- classError(cluster_raw, true_label)$misclassified
points(canzoni_no_label[misclass, c(1,4)], pch = 20, col = "black")

# dev.off()

#par(
#  bg = "white",
#  fg = "black",
#  col.axis = "black",
#  col.lab = "black",
#  col.main = "black",
#  col.sub = "black"
#)




# CORREZIONE LABEL SWITCHING ----------------------------------------------


tab <- table(true_label, cluster_raw)
tab

# Per ogni cluster (colonna) trovo il genere prevalente
mapping <- apply(tab, 2, function(col) names(which.max(col)))
mapping

# Applico la mappatura
cluster_corr <- mapping[as.character(cluster_raw)]

# Converto in factor con gli stessi livelli delle etichette vere
cluster_factor <- factor(cluster_corr,
                         levels = levels(true_label))


# CONFUSION MATRIX --------------------------------------------------------



confusionMatrix(cluster_factor, true_label)
adjustedRandIndex (cluster_raw, true_label)





# -------------------------------------------------------------------------
# SUPERVISED CLASSIFICATION
# -------------------------------------------------------------------------

# STANDARDIZZAZIONE -------------------------------------------------------

vars_to_scale <- c(
  "acousticness", "danceability", "energy", "liveness", 
  "loudness", "tempo", "valence", "duration_min", "popularity"
)

dataset<-canzoni

dataset <- dataset %>%
  mutate(across(all_of(vars_to_scale), ~ as.numeric(scale(.x))))

summary(dataset)

# INIZIO ------------------------------------------------------------------

# Matrice numerica dataset
X<-dataset%>%
  select(where(is.numeric))

# Classi
dataset.class<-as.factor(unlist(dataset[12]))
summary(dataset.class)

# Divisione del dataset
n<-nrow(X)
test.set.labels<-sample(1:n,248)
test.set<-X[test.set.labels,]
training.set<-X[-test.set.labels,]  

test.set.class<-dataset.class[test.set.labels]
training.set.class<-dataset.class[-test.set.labels]

# Simulazione iterativa per identificare il miglior modello Rmixmod
prove<- data.frame(Modello=character(), CV_Error=double())

for(i in 1:100){
  
  pr<-mixmodLearn(training.set, training.set.class,    
                  models=mixmodGaussianModel(family='all',equal.proportions=FALSE),
                  criterion=c('CV','BIC'))
  best_model_name <- pr@bestResult@model
  best_cv_value   <- pr@bestResult@criterionValue[1] 
  prove <- rbind(prove, 
                 data.frame(
                   Modello=best_model_name, 
                   CV_Error=best_cv_value))
}

summ.prove <- prove %>%
  mutate(Accuratezza = 1 - CV_Error) %>% 
  group_by(Modello) %>%
  summarise(
    Vittorie = n(),                     
    Avg_Accuracy = mean(Accuratezza),   
    Max_Accuracy = max(Accuratezza),      
    Min_Accuracy = min(Accuratezza),       
    Std_Dev = sd(Accuratezza)             
  ) %>%
  arrange(desc(Vittorie)) 

print(summ.prove)



# V-fold CV Montecarlo ----------------------------------------------------

accuracy_dist <- c()

for(j in 1:100){
  V <- sample(rep(1:10, length.out = nrow(training.set)))
  accuratezzaV <- c()
  for(k in 1:10){
    
    ind <- which(V == k)
    x_train <- training.set[-ind,]
    y_train <- training.set.class[-ind]
    x_value <- training.set[ind,]
    y_value <- training.set.class[ind]
    mod <- mixmodLearn(x_train, y_train, 
                       models=mixmodGaussianModel(listModels="Gaussian_pk_L_C"))
    pred <- mixmodPredict(x_value, classificationRule=mod["bestResult"])
    accuratezzaV[k] <- mean(pred@partition == as.integer(y_value))
  }
  accuracy_dist[j] <- mean(accuratezzaV)
}


# Statistiche Monte Carlo CV

mc_stats <- data.frame(
  Mean = mean(accuracy_dist),
  SD   = sd(accuracy_dist),
  Min  = min(accuracy_dist),
  Max  = max(accuracy_dist)
)
print(mc_stats)

df_acc <- data.frame(Accuracy = accuracy_dist)

ggplot(df_acc, aes(x=Accuracy)) +
  
  geom_density(fill="gold", alpha=0.5) +
  geom_vline(xintercept=mean(accuracy_dist), color="red", linetype="dashed") +
  labs(title="Validazione Monte Carlo (100 iterazioni)",
       
       subtitle=paste("Media Accuratezza:", round(mean(accuracy_dist), 4))) +
  
  theme_minimal()
ggsave("montecarlo_validazione.pdf",width = 7, height = 4)

# Addestramento del modello selezionato sul training set ------------------

end <- mixmodLearn(
  training.set, 
  training.set.class,
  models = mixmodGaussianModel(listModels="Gaussian_pk_L_C")
)

# Estrazione del modello selezionato e del valore di CV corrispondente
Test_model_name <- end@bestResult@model # anche se ho fatto mixmodGaussianModel(listModels="Gaussian_pk_L_C")
Test_cv_value   <- end@bestResult@criterionValue[1]

# Predizione sul test set 

prede <- mixmodPredict(
  test.set, 
  classificationRule = end["bestResult"]
)

# Calcolo dell'accuratezza finale

mean(prede@partition == as.integer(test.set.class))

#  ANALISI ERRORI ---------------------------------------------------------

# Matrice di confusione
conf_matrix <- table(Predetto = prede@partition, Reale = test.set.class)
print(conf_matrix)

precision_per_class <- diag(conf_matrix) / colSums(conf_matrix)
print(round(precision_per_class, 3))


# Visualizzazione degli errori
df_test_plot <- as.data.frame(test.set) %>%
  select(popularity, energy) %>% 
  mutate(
    Genere = test.set.class,
    Predetto = factor(prede@partition, labels = levels(test.set.class)),
    Corretto = ifelse(Genere == Predetto, "Sì", "NO (Errore)")
  )

ggplot(df_test_plot, aes(x = popularity, y = energy, color = Genere)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_point(data = subset(df_test_plot, Corretto == "NO (Errore)"), 
             aes(x = popularity, y = energy,color=Predetto),
             shape = 1, size = 4, stroke = 1.5) +
  labs(title = "Classificazione Test Set",
       subtitle = "I cerchi indicano le canzoni classificate male",
       caption = paste("Accuratezza Finale:", round(mean(prede@partition == as.integer(test.set.class)), 4))) +
  theme_minimal()
ggsave("errori_test_set.pdf", width = 8, height = 5)

#  ERRORI -----------------------------------------------------------------

# Identifico le posizioni dove il modello ha sbagliato
indici_errori_test <- which(prede@partition != as.integer(test.set.class))

indici_originali <- test.set.labels[indici_errori_test]

# Estraggo i nomi e i dettagli dal dataset originale

canzoni_sbagliate <- dataset[indici_originali, ] %>%
  select(name, artist) %>%  
  mutate(
    Genere_Reale = test.set.class[indici_errori_test],
    Genere_Predetto = factor(prede@partition[indici_errori_test], 
                             levels = 1:3, 
                             labels = levels(dataset.class)) 
  )

# Visualizzo la lista 
print("Canzoni che hanno ingannato il modello:")
print(canzoni_sbagliate)

# grafico usato per le slide -------------------------------------------------

if (!require("fmsb")) install.packages("fmsb")
library(fmsb)

# Preparazione dei dati

X.class<-X
X.class$class<-as.factor(dataset$label)

media_per_classe <- X.class %>%
  group_by(class) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

print(media_per_classe)

# Etichette 
attributi <- c(names(training.set))


# Limiti max e min
max<-c()
for(i in 1:9){
  max[i]<-max(X[,i])
}
min<-c()
for(i in 1:9){
  min[i]<-min(X[,i])
}
dfE <- rbind(max, min, data.frame(media_per_classe[1,-1]))
dfN <- rbind(max, min, data.frame(media_per_classe[2,-1]))
dfP <- rbind(max, min, data.frame(media_per_classe[3,-1]))
dftest<-rbind(max, min, data.frame(X[92,]))
colnames(dfE) <- c("POP","ACU","DAN","ENE","LIV","LOU","TEM","VAL","DUR")

col_sfondo <- "#D15EFF"    
col_griglia<- "#5C4B75"   
col_testo  <- "#FFFFFF"   

# Grafico
par(mfrow = c(1, 3), bg = col_sfondo, mar = c(1, 1, 3, 1), 
    col.main = "white", col.lab = "white",col.axis = "white", 
    col = "white",cex.main=2.5)


radarchart(dfE,
           axistype = 0,
           pcol = "red",
           pfcol = "#FF00004D",
           plwd = 3,
           cglcol = col_griglia,
           cglwd = 2,
           vlcex = 1.1,
           vlabels = colnames(dfE),
           title = "Elettronico")

par(new = TRUE)   
radarchart(dftest,
           axistype = 0,
           pcol = "white",
           pfcol = "#0000004D",
           vlabels = colnames(dfE),
           vlcex = 1.1,
           plwd = 3,
           add = TRUE)

radarchart(dfN,
           axistype = 0,
           pcol = "green",
           pfcol = "#00FF004D",
           plwd = 3,
           cglcol = col_griglia,
           cglwd = 2,
           vlcex = 1.1,
           vlabels = colnames(dfE),
           title = "Neoclassico")

par(new = TRUE)
radarchart(dftest,
           axistype = 0,
           pcol = "white",
           pfcol = "#0000004D",
           vlabels = colnames(dfE),
           vlcex = 1.1,
           plwd = 3,
           add = TRUE)

radarchart(dfP,
           axistype = 0,
           pcol = "blue",
           pfcol = "#0000FF4D",
           plwd = 3,
           cglcol = col_griglia,
           cglwd = 2,
           vlcex = 1.1,
           vlabels = colnames(dfE),
           title = "Pop")

par(new = TRUE)
radarchart(dftest,
           axistype = 0,
           pcol = "white",
           pfcol = "#0000004D",
           vlabels = colnames(dfE),
           vlcex = 1.1,
           plwd = 3,
           add = TRUE)
#dev.off()

