# ==============================================================================
# PROGETTO SBL CONSULTANCY: 
# Spopolamento, Accessibilità Ferroviaria e Servizi in Italia (2001-2021)
# SCRIPT MASTER: Fasi 1 - 7
# ==============================================================================

# ==============================================================================
# FASE 2: IMPORTAZIONE E PRE-PROCESSING DEI DATI
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PREPARAZIONE DELL'AMBIENTE
# ------------------------------------------------------------------------------
# Caricamento delle librerie necessarie
library(sf)        # Per la gestione dei dati spaziali
library(tidyverse) # Include dplyr, tidyr, readr, stringr per manipolazione dati

# ------------------------------------------------------------------------------
# 2. IMPORTAZIONE DEI DATASET
# ------------------------------------------------------------------------------
# Importazione confini comunali (basta puntare al file .shp)
comuni_sf <- st_read("Com2021_g.shp")

# Importazione dati demografici ISTAT (CSV con separatore punto e virgola)
popolazione_raw <- read_delim("Popolazione residente - Serie storica (IT1,DF_DCSS_POPRES_SERIES_TV_1,1.0).csv", delim = ";")

# Importazione dataset spaziali puntuali (GeoJSON)
stazioni_sf <- st_read("Stazioni ferroviarie Italia.geojson")
servizi_sf  <- st_read("Servizi Italia.geojson")
#mezzi_sf    <- st_read("Mezzi pubblici Italia.geojson")

# ------------------------------------------------------------------------------
# 3 e 4. TRASFORMAZIONE, PULIZIA E UNIFORMAZIONE DATI DEMOGRAFICI
# ------------------------------------------------------------------------------
popolazione_pulita <- popolazione_raw %>%
  # Selezione delle colonne utili
  select(REF_AREA, Territorio, TIME_PERIOD, OBS_VALUE) %>%
  
  # Pad degli zeri: assicuriamo che il codice ISTAT sia sempre di 6 caratteri
  mutate(REF_AREA = str_pad(as.character(REF_AREA), width = 6, side = "left", pad = "0")) %>%
  
  # Pivot: trasformiamo l'anno in colonna e i valori della popolazione nel contenuto
  pivot_wider(
    names_from = TIME_PERIOD, 
    values_from = OBS_VALUE, 
    names_prefix = "pop_"
  ) %>%
  
  # Uniformazione: rinominiamo la colonna del codice per il join con lo shapefile
  rename(PRO_COM = REF_AREA)

# ------------------------------------------------------------------------------
# 5. PULIZIA SPAZIALE E CONTROLLO COORDINATE
# ------------------------------------------------------------------------------
# Comuni: rimuoviamo geometrie vuote e correggiamo eventuali errori topologici
comuni_sf <- comuni_sf %>%
  filter(!st_is_empty(.)) %>%
  st_make_valid()

# Punti (Stazioni, Servizi, Mezzi): teniamo solo le geometrie non vuote
stazioni_sf <- stazioni_sf %>% filter(!st_is_empty(.))
servizi_sf  <- servizi_sf %>% filter(!st_is_empty(.))
#mezzi_sf    <- mezzi_sf %>% filter(!st_is_empty(.))

# Controllo dei Sistemi di Riferimento (CRS) stampato in console
cat("CRS Comuni:", st_crs(comuni_sf)$epsg, "\n")
cat("CRS Stazioni:", st_crs(stazioni_sf)$epsg, "\n")

# ------------------------------------------------------------------------------
# 6. ESECUZIONE DEL JOIN E GESTIONE VALORI MANCANTI (NA)
# ------------------------------------------------------------------------------

# Sistemiamo la colonna PRO_COM dello shapefile forzandola a testo a 6 caratteri
comuni_sf <- comuni_sf %>%
  mutate(PRO_COM = str_pad(as.character(PRO_COM), width = 6, side = "left", pad = "0"))

# Ora il join funzionerà perfettamente perché entrambi sono <character> a 6 cifre
comuni_pop <- comuni_sf %>%
  left_join(popolazione_pulita, by = "PRO_COM")

# Controllo preliminare: conteggio dei comuni rimasti senza dati demografici
comuni_senza_dati <- sum(is.na(comuni_pop$pop_2021))
cat("Comuni con dati demografici mancanti:", comuni_senza_dati, "\n")

# Opzionale: rimuovere il commento (#) dalla riga seguente per eliminare i comuni senza dati
# comuni_pop <- comuni_pop %>% drop_na(pop_2021)


# ==============================================================================
# FASE 3: COSTRUZIONE INDICATORI DI SPOPOLAMENTO
# ==============================================================================

comuni_pop <- comuni_pop %>%
  mutate(
    # 1. Calcolo della variazione percentuale (2001-2021)
    var_pop_perc = ((pop_2021 - pop_2001) / pop_2001) * 100,
    
    # 2. Classificazione in tre categorie basate sulle soglie del progetto
    classe_spopolamento = case_when(
      var_pop_perc < -20 ~ "Alto spopolamento",
      var_pop_perc >= -20 & var_pop_perc < -5 ~ "Medio spopolamento",
      var_pop_perc >= -5 ~ "Stabile o in crescita",
      TRUE ~ NA_character_ # Assegna NA nel caso manchino i dati della popolazione
    ),
    
    # 3. Trasformazione in factor ordinato per le future legende
    classe_spopolamento = factor(
      classe_spopolamento,
      levels = c("Alto spopolamento", "Medio spopolamento", "Stabile o in crescita")
    )
  )

# ------------------------------------------------------------------------------
# CONTROLLO DEI RISULTATI
# ------------------------------------------------------------------------------
# Stampiamo un riepilogo per vedere quanti comuni ricadono in ogni categoria
cat("Distribuzione dei comuni per classe di spopolamento:\n")
print(table(comuni_pop$classe_spopolamento, useNA = "ifany"))

# Vediamo le prime righe per assicurarci che i calcoli siano corretti
head(comuni_pop %>% select(PRO_COM, pop_2001, pop_2021, var_pop_perc, classe_spopolamento))

# ==============================================================================
# FASE 4: SETUP SPAZIALE E RIPROIEZIONE IN SISTEMA METRICO
# ==============================================================================

# Impostiamo il codice EPSG 32632 (WGS 84 / UTM zone 32N), standard metrico per l'Italia
crs_metrico <- 32632

# Riproiezione di tutti i dataset (comuni, stazioni e servizi)
comuni_pop  <- st_transform(comuni_pop, crs = crs_metrico)
stazioni_sf <- st_transform(stazioni_sf, crs = crs_metrico)
servizi_sf  <- st_transform(servizi_sf, crs = crs_metrico)

# ------------------------------------------------------------------------------
# CONTROLLO DEI RISULTATI
# ------------------------------------------------------------------------------
# Verifichiamo che il sistema sia identico per tutti e misurato in metri
cat("CRS Comuni in metri?", st_crs(comuni_pop)$units_gdal == "metre", "\n")
cat("CRS Stazioni in metri?", st_crs(stazioni_sf)$units_gdal == "metre", "\n")
cat("CRS Servizi in metri?", st_crs(servizi_sf)$units_gdal == "metre", "\n")


# ==============================================================================
# FASE 5: ANALISI DI PROSSIMITÀ FERROVIARIA E CREAZIONE BUFFER (AGGIORNATA)
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Calcolo della distanza dalla stazione più vicina (Metodo dei Centroidi)
# ------------------------------------------------------------------------------
# Calcoliamo il centro geometrico (centroide) di ogni comune per misurare una 
# distanza realistica, evitando che i comuni con la stazione all'interno risultino a "0 metri".
# (of_largest_polygon = TRUE assicura che per i comuni con isole venga preso il pezzo principale)
comuni_centroidi <- st_centroid(comuni_pop, of_largest_polygon = TRUE)

# Troviamo l'indice della stazione più vicina a ciascun centroide
indici_stazioni_vicine <- st_nearest_feature(comuni_centroidi, stazioni_sf)

# Calcoliamo la distanza lineare esatta in metri (Centroide -> Stazione)
distanze_minime <- st_distance(
  comuni_centroidi, 
  stazioni_sf[indici_stazioni_vicine, ], 
  by_element = TRUE
)

# Aggiungiamo il risultato come nuova variabile al dataset principale dei poligoni
# as.numeric() serve a rimuovere l'etichetta "[m]" e tenere solo il numero puro
comuni_pop <- comuni_pop %>%
  mutate(distanza_stazione_m = as.numeric(distanze_minime))

# ------------------------------------------------------------------------------
# 2. Costruzione dei Buffer e Unione (Dissolve)
# ------------------------------------------------------------------------------
# Creiamo le aree di copertura a 5 km, 10 km e 20 km attorno alle stazioni.
# st_union fonde tutti i cerchi sovrapposti in un'unica "macchia" continua.
buffer_5km  <- st_union(st_buffer(stazioni_sf, dist = 5000))
buffer_10km <- st_union(st_buffer(stazioni_sf, dist = 10000))
buffer_20km <- st_union(st_buffer(stazioni_sf, dist = 20000))

# ------------------------------------------------------------------------------
# CONTROLLO DEI RISULTATI
# ------------------------------------------------------------------------------
# Osserviamo una sintesi delle distanze per assicurarci che il minimo sia > 0
cat("Statistiche descrittive della distanza (in metri) dalle stazioni:\n")
print(summary(comuni_pop$distanza_stazione_m))

# ------------------------------------------------------------------------------
# FASE 6: JOIN SPAZIALE E ARRICCHIMENTO
# ------------------------------------------------------------------------------
comuni_pop <- comuni_pop %>%
  mutate(
    num_servizi = lengths(st_intersects(geometry, servizi_sf)),
    copertura_5km  = as.integer(lengths(st_intersects(geometry, buffer_5km)) > 0),
    copertura_10km = as.integer(lengths(st_intersects(geometry, buffer_10km)) > 0),
    copertura_20km = as.integer(lengths(st_intersects(geometry, buffer_20km)) > 0)
  )

# Salvataggio dataset pulito (Esportazione)
# st_write(comuni_pop, "Dataset_Finale.geojson", delete_dsn = TRUE)

# ------------------------------------------------------------------------------
# FASE 7: CRUSCOTTO INTERATTIVO LEAFLET SBL
# ------------------------------------------------------------------------------
library(leaflet)          # Il motore principale per creare la mappa
library(leaflet.extras)   # Serve per l'estensione addHeatmap()
library(sf)               # Serve per manipolare i dati spaziali (st_transform, st_coordinates)
library(dplyr)            # Serve per manipolare i dati tabellari e per il pipe %>%
library(htmltools)        # Serve per i tag HTML personalizzati (titolo, logo, nota metodologica)
library(htmlwidgets)      # CRITICA: Serve per la funzione onRender() e il codice JavaScript

# Riproiezione in WGS84 per Leaflet
comuni_map      <- st_transform(comuni_pop, crs = 4326)
stazioni_map    <- st_transform(stazioni_sf, crs = 4326)
buffer_5km_map  <- st_transform(buffer_5km, crs = 4326)
buffer_10km_map <- st_transform(buffer_10km, crs = 4326)
buffer_20km_map <- st_transform(buffer_20km, crs = 4326)
servizi_map     <- st_transform(servizi_sf, crs = 4326)

comuni_map <- st_simplify(comuni_map, dTolerance = 0.001)
comuni_map$distanza_km <- comuni_map$distanza_stazione_m / 1000

# ------------------------------------------------------------------------------
# 1. PREPARAZIONE PALETTE A CLASSI
# ------------------------------------------------------------------------------
colori_spopolamento <- colorFactor(
  palette = c("#d73027", "#fdae61", "#1a9850"), 
  domain = comuni_map$classe_spopolamento,
  na.color = "transparent"
)

pal_distanza <- colorBin(
  palette = "YlOrRd", 
  domain = comuni_map$distanza_km,
  bins = c(0, 5, 10, 20, 30, 60, 215) 
)

# ------------------------------------------------------------------------------
# 2. GLI ELEMENTI HTML (Titolo, Logo SBL, Nota Metodologica)
# ------------------------------------------------------------------------------
titolo_html <- tags$div(
  style = "padding: 10px; background-color: rgba(255,255,255,0.9); border-radius: 5px; font-family: Arial, sans-serif; font-weight: bold; font-size: 15px; border: 2px solid #004a99; color: #333; text-align: center; box-shadow: 3px 3px 5px grey;",
  "Analisi dei comuni in spopolamento, accessibilità ferroviaria e offerta culturale/ricettiva in Italia (2001-2021)"
)

logo_sbl_html <- HTML('
<div>
  <a href="https://sblconsultancy.it/" target="_blank">
    <img src="https://portiamovalore.uniba.it/uploads/loghi/LinkedIn%20Logo_1639558059.png" style="height:60px; opacity: 0.95; cursor: pointer;">
  </a>
</div>
')

nota_metodologica_html <- tags$div(
  style = "
    background: rgba(255, 255, 255, 0.98); 
    padding: 20px; 
    border-radius: 10px; 
    border: 1px solid #004a99; 
    max-width: 320px; 
    max-height: 250px; 
    overflow-y: auto; 
    font-family: 'Segoe UI', Arial, sans-serif; 
    font-size: 11.5px; 
    color: #333; 
    line-height: 1.6;
    box-shadow: 0 4px 25px rgba(0,0,0,0.2);
  ",
  tags$h3(style = "color: #004a99; margin-top: 0; border-bottom: 2px solid #004a99; padding-bottom: 10px; font-size: 16px;", 
          "Nota Metodologica"),
  
  tags$b("1. Disegno di Ricerca e Obiettivi"),
  tags$p("Il presente studio analizza le dinamiche di spopolamento dei comuni italiani nel ventennio 2001-2021, indagando il ruolo di due specifiche determinanti spaziali: l'accessibilità alle reti di trasporto su ferro (distanza dalle stazioni ferroviarie) e la dotazione di capitale turistico-culturale (numero di servizi ricettivi e culturali). L'approccio metodologico integra l'analisi spaziale (GIS) con la statistica inferenziale e tecniche di apprendimento automatico (Machine Learning) non supervisionato."),
  
  tags$b("2. Fonti Dati e Pre-processing"),
  tags$p("L'analisi si basa sull'integrazione di dati unificati in un unico database georeferenziato:"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Dati Demografici e Confini (ISTAT):"), " Serie storiche 2001-2021 normalizzate per il calcolo della Variazione Percentuale. Join spaziale eseguito sulle basi territoriali ISTAT 2021."),
          tags$li(tags$b("Dati Geospaziali (OpenStreetMap):"), " Estrazioni GeoJSON per nodi ferroviari (railway=station) e POI a vocazione turistico-culturale (categorie tourism, amenity e agritourism).")
  ),
  
  # 3. NUOVO PUNTO: Classificazione Demografica
  tags$b("3. Classificazione della Dinamica Demografica"),
  tags$p("La variazione percentuale della popolazione è stata segmentata in tre livelli per distinguere i fenomeni di declino strutturale dalla resilienza locale:"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Alto spopolamento:"), " variazione < -20%."),
          tags$li(tags$b("Medio spopolamento:"), " variazione compresa tra il -5% e il -20%."),
          tags$li(tags$b("Stabile o in crescita:"), " variazioni superiori al -5% (inclusa la lieve flessione fisiologica).")
          
  ),
  
  tags$b("4. Analisi Geospaziale (GIS)"),
  tags$p("Proiezione cartografica applicata: ", tags$b("UTM Zona 32N (EPSG:32632)"), " per misurazioni in metri reali. Operazioni eseguite:"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Centroidi:"), " Estrazione del centro geometrico di ogni poligono comunale."),
          tags$li(tags$b("Prossimità (Nearest Neighbor):"), " Distanza euclidea minima tra centroide e stazione."),
          tags$li(tags$b("Aggregazione (Point-in-Polygon):"), " Conteggio servizi entro i confini comunali."),
          tags$li(tags$b("Buffer:"), " Generazione di isocrone di rispetto (5, 10, 20 km)."),
          tags$li(tags$b("Heatmap:"), " Analisi di densità per l'individuazione dei cluster di servizi.")
  ),
  
  tags$b("5. Analisi Statistica e Modellizzazione"),
  tags$ul(style = "padding-left: 18px;",
          tags$li(tags$b("Correlazione (Pearson):"), " Test di dipendenza tra variazione popolazione e variabili indipendenti (significatività p < 0.05)."),
          tags$li(tags$b("Regressione Lineare Multipla:"), " Modello per isolare l'effetto netto di ciascuna variabile e calcolare il tasso di compensazione tra isolamento e resilienza turistica.")
  ),
  
  tags$b("6. Profilazione Territoriale (K-Means)"),
  tags$p("Segmentazione in 3 profili tramite Machine Learning su variabili standardizzate (Z-score). Eseguite 25 iterazioni (nstart=25) per garantire la stabilità dei centroidi, identificando: 'Aree Interne Marginalizzate', 'Provincia Connessa' e 'Poli Attrattori'."),
  
  tags$b("7. Strumenti Software"),
  tags$p("Pipeline sviluppata interamente in ", tags$b("R"), " tramite i pacchetti: sf, tidyverse, leaflet, broom. ")
)

# ------------------------------------------------------------------------------
# 3. COSTRUZIONE MAPPA LEAFLET (FIX HEATMAP E Z-INDEX)
# ------------------------------------------------------------------------------

# Estrazione sicura delle coordinate per la heatmap
coords_servizi <- st_coordinates(st_centroid(st_geometry(servizi_map)))

mappa_sbl_final <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  
  # A. LIVELLI DI PROFONDITÀ (Z-INDEX)
  addMapPane("sfondo", zIndex = 210) %>%
  addMapPane("poligoni", zIndex = 220) %>%
  addMapPane("buffer", zIndex = 230) %>%
  addMapPane("punti", zIndex = 430) %>%
  addMapPane("etichettetesto", zIndex = 450) %>%
  
  # B. MAPPA DI SFONDO
  addProviderTiles(
    providers$Esri.WorldGrayCanvas,
    options = providerTileOptions(pane = "sfondo")
  ) %>%
  
  # LAYER 1: Spopolamento 
  addPolygons(
    data = comuni_map,
    fillColor = ~colori_spopolamento(classe_spopolamento),
    weight = 0.5, opacity = 1, color = "white", fillOpacity = 0.7,
    group = "Spopolamento (Comuni)",
    options = pathOptions(pane = "poligoni"),
    popup = ~paste0(
      "<div style='font-family:Arial; font-size:12px;'>",
      "<b style='color:#004a99;'>Comune di ", Territorio, "</b><br>",
      "<hr style='margin:5px 0;'>",
      "<b>Dinamica demografica:</b> ", classe_spopolamento, "<br>",
      "<b>Variazione 2001-2021:</b> ", round(var_pop_perc, 1), "%<br>",
      "<b>Popolazione 2021:</b> ", pop_2021, " ab.<br>",
      "<b>Servizi presenti:</b> ", num_servizi, "</div>"
    )
  ) %>%
  
  # LAYER 2: Prossimità Stazioni 
  addPolygons(
    data = comuni_map,
    fillColor = ~pal_distanza(distanza_km),
    weight = 0.2, opacity = 1, color = "white", fillOpacity = 0.8,
    group = "Analisi Distanza Stazioni",
    options = pathOptions(pane = "poligoni"),
    popup = ~paste0(
      "<div style='font-family:Arial; font-size:12px;'>",
      "<b>Comune:</b> ", Territorio, "<br>",
      "<b>Distanza stazione:</b> ", round(distanza_km, 1), " km</div>"
    )
  ) %>%
  
  # LAYER BUFFER
  addPolygons(
    data = buffer_20km_map,
    fillColor = "black",
    color = "black",
    weight = 1,
    fillOpacity = 0.08,
    group = "Buffer 20km",
    options = pathOptions(pane = "buffer", interactive = FALSE)
  ) %>%
  
  addPolygons(
    data = buffer_10km_map,
    fillColor = "darkblue",
    color = "darkblue",
    weight = 1,
    fillOpacity = 0.12,
    group = "Buffer 10km",
    options = pathOptions(pane = "buffer", interactive = FALSE)
  ) %>%
  
  addPolygons(
    data = buffer_5km_map,
    fillColor = "blue",
    color = "blue",
    weight = 1,
    fillOpacity = 0.15,
    group = "Buffer 5km",
    options = pathOptions(pane = "buffer", interactive = FALSE)
  ) %>%
  
  # LAYER PUNTI: Stazioni e Servizi 
  addCircleMarkers(
    data = stazioni_map,
    radius = 3,
    color = "purple",
    group = "Stazioni",
    options = pathOptions(pane = "punti"),
    popup = ~paste0(
      "<div style='font-family:Arial; font-size:11px;'>",
      "<b style='color:purple;'>STAZIONE FERROVIARIA</b><br>",
      "<b>Nome:</b> ", coalesce(name, "N.D."), "<br>",
      "<b>Rete:</b> ", coalesce(operator, "RFI"),
      "</div>"
    )
  ) %>%
  
  addCircleMarkers(
    data = servizi_map,
    radius = 2,
    color = "red",
    group = "Servizi Cultura/Turismo", 
    clusterOptions = markerClusterOptions(),
    popup = ~paste0(
      "<div style='font-family:Arial; font-size:11px;'>",
      "<b style='color:red;'>SERVIZIO / STRUTTURA</b><br>",
      "<b>Nome:</b> ", coalesce(name, "Informazione non disponibile"), "<br>",
      "<b>Categoria:</b> ", coalesce(tourism, amenity, agritourism),
      "</div>"
    )
  ) %>%
  
  # HEATMAP
  addHeatmap(
    lng = coords_servizi[,1], 
    lat = coords_servizi[,2], 
    radius = 10,       
    blur = 15,         
    max = 0.08,        
    gradient = c(
      "0.2" = "yellow",
      "0.5" = "orange",
      "1" = "red"
    ), 
    group = "Densità Spaziale Servizi (Heatmap)"
  ) %>%
  
  # POSIZIONAMENTO BOX
  addControl(titolo_html, position = "topright") %>%
  addControl(nota_metodologica_html, position = "bottomleft") %>%
  addControl(logo_sbl_html, position = "bottomright") %>%
  
  # LEGENDE DINAMICHE
  addLegend(
    data = comuni_map,
    pal = colori_spopolamento,
    values = ~classe_spopolamento, 
    title = "Classe Spopolamento",
    position = "topright",
    group = "Spopolamento (Comuni)" 
  ) %>%
  
  addLegend(
    pal = pal_distanza,
    values = comuni_map$distanza_km, 
    title = "Distanza Stazione (km)",
    position = "topright",
    group = "Analisi Distanza Stazioni" 
  ) %>%
  
  addLegend(
    colors = c("yellow", "orange", "red"),
    labels = c("Bassa", "Media", "Alta"),
    title = "Intensità Servizi", 
    position = "topright",
    group = "Densità Spaziale Servizi (Heatmap)"
  ) %>%
  
  # CONTROLLO LAYER 
  addLayersControl(
    overlayGroups = c(
      "Spopolamento (Comuni)", 
      "Analisi Distanza Stazioni",
      "Densità Spaziale Servizi (Heatmap)",
      "Buffer 5km", 
      "Buffer 10km", 
      "Buffer 20km", 
      "Stazioni", 
      "Servizi Cultura/Turismo"
    ),
    options = layersControlOptions(collapsed = FALSE),
    position = "topleft"
  ) %>%
  
  # SPEGNIMENTO LAYER ALL'AVVIO
  hideGroup(c(
    "Analisi Distanza Stazioni",
    "Densità Spaziale Servizi (Heatmap)",
    "Buffer 5km", 
    "Buffer 10km", 
    "Buffer 20km", 
    "Stazioni", 
    "Servizi Cultura/Turismo"
  )) %>%
  
  # INIEZIONE JAVASCRIPT
  onRender("
    function(el, x) {
      L.control.zoom({position: 'bottomright'}).addTo(this);
      
      var labels = document.querySelectorAll(
        '.leaflet-control-layers-overlays label'
      );
      
      for (var i = 0; i < labels.length; i++) {
        if (labels[i].innerText.includes(
          'Densità Spaziale Servizi (Heatmap)'
        )) {
          labels[i].style.borderBottom = '1px solid #004a99'; 
          labels[i].style.paddingBottom = '8px';
          labels[i].style.marginBottom = '8px';
        }
      }
    }
  ")

# Visualizza la mappa
mappa_sbl_final



# Carica la libreria per l'esportazione in Excel
library(writexl)

# Esportazione pulita senza geometrie
comuni_pop %>% 
  # 1. Rimuoviamo la colonna spaziale (fondamentale per i formati tabellari)
  st_drop_geometry() %>% 
  # 2. Esportiamo direttamente in formato .xlsx
  write_xlsx("Dataset Comuni - Spopolamento e Servizi Italia.xlsx")

