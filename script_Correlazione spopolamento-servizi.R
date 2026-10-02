# ==============================================================================
# FASE 8: ANALISI STATISTICA E INTERPRETAZIONE DEI RISULTATI
# ==============================================================================

# Carichiamo pacchetti utili per la statistica e i grafici statici da report
if(!require(broom)) install.packages("broom")
library(broom)
library(ggplot2)
library(cluster)
library(dplyr)
library(tidyr)
library(sf)

#Correlazione di Pearson distanza-spopolamento

cor.test(comuni_pop$distanza_stazione_m, comuni_pop$var_pop_perc)

# Rimuoviamo eventuali valori mancanti per evitare errori nel grafico
dati_plot <- comuni_pop %>% 
  filter(!is.na(distanza_stazione_m) & !is.na(var_pop_perc))

# Creazione del grafico della correlazione
grafico_correlazione <- ggplot(dati_plot, aes(x = distanza_stazione_m / 1000, y = var_pop_perc)) +
  
  # Aggiungiamo i punti (ogni punto è un comune). 
  # alpha = 0.2 rende i punti semitrasparenti per gestire la sovrapposizione
  geom_point(alpha = 0.2, color = "#004a99") +
  
  # Aggiungiamo la retta di tendenza lineare (lm) in rosso
  #geom_smooth(method = "lm", color = "red", fill = "pink", linewidth = 1.2, se = TRUE) +
  
  # Formattazione di titoli e assi
  labs(
    title = "Impatto della Distanza dalle Stazioni Ferroviarie sulla Demografia (2001-2021)",
    subtitle = "Correlazione tra distanza dalle stazioni e variazione di popolazione (Italia, 2001-2021)",
    caption = "Elaborazione SBL Consultancy su dati ISTAT e OSM",
    x = "Distanza dalla Stazione più vicina (km)",
    y = "Variazione Popolazione (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "darkgray"),
    axis.title = element_text(face = "bold")) +
  coord_cartesian(ylim = c(-50, 100))

# Mostra il grafico nel pannello "Plots" di RStudio
print(grafico_correlazione)


#Correlazione Numero servizi - Spopolamento

cor.test(comuni_pop$num_servizi, comuni_pop$var_pop_perc)

# Rimuoviamo eventuali valori mancanti per evitare errori nel grafico
dati_plot_servizi <- comuni_pop %>% 
  filter(!is.na(num_servizi) & !is.na(var_pop_perc))

# Creazione del grafico della correlazione
grafico_servizi <- ggplot(dati_plot_servizi, aes(x = num_servizi, y = var_pop_perc)) +
  
  # Aggiungiamo i punti. 
  # alpha = 0.2 rende i punti semitrasparenti per gestire la densità
  geom_point(alpha = 0.2, color = "#1a9850") + # Verde scuro
  
  # Aggiungiamo la retta di tendenza lineare in blu scuro
  #geom_smooth(method = "lm", color = "#004a99", fill = "lightblue", linewidth = 1.2, se = TRUE) +
  
  
  # Formattazione di titoli e assi
  labs(
    title = "Impatto dell'Offerta Turistico-Culturale sulla Demografia",
    subtitle = "Correlazione tra numero di servizi e variazione di popolazione (Italia, 2001-2021)",
    caption = "Elaborazione SBL Consultancy su dati ISTAT e OSM",
    x = "Numero di Servizi (Strutture ricettive, musei, teatri, ecc.)",
    y = "Variazione Popolazione (%)"
  ) +
  
  # Stile minimalista
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "darkgray"),
    axis.title = element_text(face = "bold")
  ) +
  
  # Tagliamo l'asse Y per escludere outlier estremi e rendere la retta visibile.
  # Nota: Se nel grafico i punti sono tutti schiacciati a sinistra a causa di grandi 
  # città (come Roma o Milano che hanno migliaia di servizi), potresti voler limitare 
  # anche l'asse X aggiungendo xlim = c(0, 100) dentro coord_cartesian.
  coord_cartesian(ylim = c(-60, 100))

# Mostra il grafico nel pannello "Plots" di RStudio
print(grafico_servizi)

# ------------------------------------------------------------------------------
# 1. REGRESSIONE LINEARE MULTIPLA
# ------------------------------------------------------------------------------
cat("\n--- ESECUZIONE MODELLO DI REGRESSIONE ---\n")

# Il modello verifica come la variazione di popolazione (Y) dipenda 
# dalla distanza dalla stazione (X1) e dal numero di servizi (X2)
modello_regressione <- lm(
  var_pop_perc ~ distanza_stazione_m + num_servizi, 
  data = comuni_pop
)

# Stampiamo un riassunto pulito dei risultati
risultati_modello <- tidy(modello_regressione)
print(risultati_modello)

# Interpretazione automatizzata basata sui p-value
p_value_distanza <- risultati_modello %>% filter(term == "distanza_stazione_m") %>% pull(p.value)
if(p_value_distanza < 0.05) {
  cat("\n=> INSIGHT: La distanza dalla stazione ha un impatto STATISTICAMENTE SIGNIFICATIVO sullo spopolamento.\n")
} else {
  cat("\n=> INSIGHT: La distanza dalla stazione NON risulta avere un impatto statisticamente significativo nel modello.\n")
}

glance(modello_regressione)




#Simulatore Demografico SBL - Proiezione 2001-2021

library(shiny)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("Simulatore Demografico SBL - Proiezione 2001-2021"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Trascina i cursori per simulare il destino demografico di un comune."),
      sliderInput("km", "Distanza dalla Stazione (km):", min = 0, max = 60, value = 10, step = 0.5),
      sliderInput("serv", "Numero di Servizi Turistico-Culturali:", min = 0, max = 500, value = 5, step = 1),
      hr(),
      p("Basato sul Modello di Regressione Lineare Multipla SBL Consultancy.")
    ),
    
    mainPanel(
      card(
        card_header("Variazione di Popolazione Prevista"),
        card_body(
          h2(textOutput("risultato"), align = "center"),
          plotOutput("gauge", height = "200px")
        )
      )
    )
  )
)

server <- function(input, output) {
  output$risultato <- renderText({
    # Formula estratta dal tuo modello: Y = Intercetta + (Dist * Coeff_Dist) + (Serv * Coeff_Serv)
    valore <- 4.4361274 + (input$km * 1000 * -0.0007325) + (input$serv * 0.0285913)
    paste0(round(valore, 2), " %")
  })
}

shinyApp(ui = ui, server = server)







# ------------------------------------------------------------------------------
# 2. ANALISI DEI CLUSTER (K-MEANS) PER PROFILAZIONE TERRITORIALE
# ------------------------------------------------------------------------------
cat("\n--- CREAZIONE PROFILI TERRITORIALI (CLUSTERING) ---\n")

# Prepariamo un dataframe pulito (senza geometrie e senza valori mancanti)
dati_cluster <- comuni_pop %>%
  st_drop_geometry() %>%
  select(PRO_COM, Territorio, var_pop_perc, distanza_stazione_m, num_servizi) %>%
  drop_na()

# Standardizziamo le variabili (fondamentale perché i metri e i servizi hanno scale diverse)
dati_standard <- scale(dati_cluster %>% select(-PRO_COM, -Territorio))

# Applichiamo l'algoritmo K-Means per trovare 3 gruppi naturali di comuni
set.seed(42) # Fissiamo il seed per avere risultati riproducibili
kmeans_risultato <- kmeans(dati_standard, centers = 3, nstart = 25)

# Assegniamo il numero del cluster al nostro dataset
dati_cluster$profilo_cluster <- as.factor(kmeans_risultato$cluster)

# Calcoliamo le medie per capire cosa rappresenta ogni cluster
sintesi_cluster <- dati_cluster %>%
  group_by(profilo_cluster) %>%
  summarise(
    Comuni_Totali = n(),
    Variazione_Pop_Media = round(mean(var_pop_perc), 1),
    Distanza_Stazione_Media_km = round(mean(distanza_stazione_m) / 1000, 1),
    Servizi_Medi = round(mean(num_servizi), 1)
  )

print(sintesi_cluster)

#GRAFICO CLUSTER

dati_plot_cluster <- dati_cluster %>%
  mutate(
    Nome_Cluster = case_when(
      profilo_cluster == "1" ~ "Provincia Connessa",
      profilo_cluster == "2" ~ "Poli Attrattori (Grandi Città)",
      profilo_cluster == "3" ~ "Aree Interne Marginalizzate"
    )
  )

# Creiamo il Grafico a Bolle
grafico_cluster <- ggplot(dati_plot_cluster, aes(
  x = distanza_stazione_m / 1000, 
  y = var_pop_perc, 
  color = Nome_Cluster, 
  size = num_servizi # La grandezza del punto dipenderà dai servizi
)) +
  
  # Aggiungiamo i punti semitrasparenti
  geom_point(alpha = 0.6) +
  
  # Assegniamo colori strategici ai cluster
  scale_color_manual(values = c(
    "Provincia Connessa" = "#377eb8",             # Blu
    "Poli Attrattori (Grandi Città)" = "#4daf4a", # Verde
    "Aree Interne Marginalizzate" = "#e41a1c"     # Rosso allarme
  )) +
  
  # Regoliamo la scala delle bolle per evitare che le grandi città coprano tutto
  scale_size_continuous(range = c(1, 15), guide = "none") + # "guide=none" nasconde la legenda delle dimensioni per pulizia
  
  # Etichette e Formattazione
  labs(
    title = "Mappatura dei Profili Territoriali (K-Means Clustering)",
    subtitle = "Dimensione bolla: N. Servizi | Colore: Profilo Territoriale",
    x = "Distanza dalla Stazione (km)",
    y = "Variazione Popolazione (%)",
    color = "Profilo Territoriale:"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  ) +
  
  # Tagliamo gli estremi per una visuale pulita 
  coord_cartesian(ylim = c(-50, 100), xlim = c(0, 40))

grafico_cluster


