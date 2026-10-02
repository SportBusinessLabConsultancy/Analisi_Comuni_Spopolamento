# 🏘️ Analisi dei Comuni in Spopolamento

Analisi spaziale completa dei comuni italiani per comprendere la relazione tra spopolamento, accessibilità ferroviaria e distribuzione dei servizi culturali e turistici, sviluppata in R.

---

## 🗺️ Mappa interattiva

👉 [Visualizza la mappa](https://sportbusinesslabconsultancy.github.io/Analisi_Comuni_Spopolamento/Mappa%20spopolamento%20-%20mobilit%C3%A0.html)

---

## 📌 Descrizione

Il progetto sviluppa un'analisi spaziale dei comuni italiani finalizzata a comprendere le relazioni tra spopolamento demografico, accessibilità alle infrastrutture ferroviarie e presenza di servizi culturali e turistici.

Integrando dati ISTAT sulle variazioni di popolazione (2001–2021) con dati geografici delle stazioni ferroviarie (RFI) e dei servizi culturali e turistici estratti da OpenStreetMap, il progetto costruisce un dataset analitico multi-variato a livello comunale che consente di esplorare i pattern territoriali dello spopolamento e i fattori associati alla resilienza o al declino demografico.

L'output principale è una mappa Leaflet interattiva multi-layer che visualizza la distribuzione dello spopolamento, la copertura ferroviaria e la dotazione di servizi per ciascun comune italiano.

---

## 🎯 Obiettivi

- Costruire un indicatore di spopolamento comunale basato sulla variazione percentuale della popolazione ISTAT tra il 2001 e il 2021, classificando i comuni in tre categorie (alto spopolamento, medio, stabile/crescita)
- Analizzare l'accessibilità ferroviaria calcolando la distanza di ciascun comune dalla stazione più vicina e costruendo buffer di copertura a 5, 10 e 20 km
- Associare a ciascun comune il numero di strutture culturali (musei, teatri, biblioteche) e turistiche (hotel, B&B, agriturismi) presenti nel territorio o nelle vicinanze
- Esplorare le correlazioni tra spopolamento, accessibilità ferroviaria e dotazione di servizi tramite analisi di regressione, clustering territoriale e analisi di densità spaziale
- Produrre una mappa interattiva Leaflet con layer tematici per l'esplorazione visuale delle relazioni tra variabili

---

## 🔬 Metodologia

**Raccolta e preparazione dati**
I dati di base comprendono gli shapefile dei confini comunali ISTAT, le serie storiche della popolazione (2001–2021), i dati geografici delle stazioni ferroviarie RFI e i punti di interesse culturali e turistici estratti da OpenStreetMap. Tutti i dataset sono stati importati in R tramite il pacchetto `sf` e sottoposti a una fase di pulizia che include la gestione dei valori mancanti, il controllo delle coordinate e l'uniformazione dei nomi comunali per consentire il join tra dati geografici e demografici.

**Costruzione dell'indicatore di spopolamento**
Per ciascun comune è stata calcolata la variazione percentuale della popolazione tra il 2001 e il 2021, classificando i comuni in: alto spopolamento (< -20%), medio spopolamento (-20% / -5%), stabile o in crescita (> -5%).

**Analisi dell'accessibilità ferroviaria**
Tutti i dataset sono stati proiettati in un sistema di riferimento metrico per il calcolo corretto delle distanze. Per ogni comune è stata calcolata la distanza dalla stazione più vicina; sono stati costruiti buffer di 5, 10 e 20 km attorno alle stazioni per definire le aree di copertura ferroviaria, associando a ciascun comune una variabile binaria di inclusione nei buffer.

**Analisi spaziale dei servizi**
Tramite operazioni di join spaziale sono stati associati a ciascun comune il numero di strutture culturali e turistiche presenti nel territorio, costruendo così un dataset finale con indicatori demografici, di accessibilità e di dotazione di servizi.

**Analisi interpretativa**
I risultati sono stati interpretati attraverso modelli di regressione (spopolamento in funzione della distanza e dei servizi), clustering dei comuni in tipologie territoriali (comuni isolati, turistici, resilienti) e analisi di densità spaziale per identificare le aree di maggiore concentrazione dei servizi.

---

## 📊 Risultati principali

- Mappata la distribuzione dello spopolamento per tutti i comuni italiani con classificazione in tre categorie demografiche
- Calcolata l'accessibilità ferroviaria a livello comunale e costruita la copertura per buffer a 5, 10 e 20 km dalle stazioni
- Analizzata la correlazione tra spopolamento, distanza dalle stazioni e presenza di servizi culturali e turistici
- Prodotta una mappa Leaflet interattiva multi-layer con visualizzazione simultanea di spopolamento, rete ferroviaria e dotazione di servizi
- Identificate le tipologie territoriali dei comuni italiani tramite clustering (comuni isolati, turistici, resilienti)

---

## 🛠️ Tecnologie utilizzate

- **R** — elaborazione e analisi dei dati
- **sf / tidyverse / dplyr** — gestione dati geospaziali e join spaziale
- **Leaflet** — mappa interattiva web
- **ISTAT** — shapefile comunali e serie storiche della popolazione
- **OpenStreetMap** — servizi culturali e turistici
- **RFI (Rete Ferroviaria Italiana)** — dati stazioni ferroviarie

---

## 📁 Struttura del progetto

```
Analisi-Comuni-Spopolamento/
├── README.md
├── script_Spopolamento e Servizi Italia (1).R
├── script_Correlazione spopolamento-servizi.R
├── Executive Summary - Accessibilità, Servizi e Spopolamento in Italia.docx
└── Mappa spopolamento - mobilità.html
```

> ⚠️ I dati grezzi (shapefile comunali, dataset ISTAT, GeoJSON servizi) e i workspace R non sono inclusi nel repository per ragioni di dimensione. Le fonti sono indicate nella sezione Dati.

---

## 📂 Dati

| Fonte | Descrizione |
|-------|-------------|
| [ISTAT](https://www.istat.it) | Shapefile confini comunali, serie storiche popolazione 2001–2021 |
| [OpenStreetMap](https://www.openstreetmap.org) | Servizi culturali e turistici |
| [RFI](https://www.rfi.it) | Dati stazioni ferroviarie |

---

## 🔗 Link utili

- 🌐 [Sport Business Lab Consultancy](https://www.sblconsultancy.it/)
