library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(readxl)
library(DT)
library(writexl)
library(plotly)
library(tidyr)

rsconnect::writeManifest()

#datasetpertama
data_alert <- read_excel("data alert mingguan.xlsx",sheet="data alert")
data_klb <- read_excel("data alert mingguan.xlsx",sheet="data KLB")

gabungan <- rbind(data_alert,data_klb)
gabungan$verif <- ifelse(gabungan$Status == "Verifikasi",yes = 1,no=0)
aaa <- gabungan %>% group_by(Minggu, Penyakit, Propinsi, Kota, Kecamatan, Puskesmas) %>%
  summarize("jumlah alert" = sum(jumlah),
            "jumlah alert direspon" = sum(verif),
            "jumlah KLB" = sum(Status == "KLB"),
            "% alert direspon" = (sum(verif)/sum(jumlah))*100)
aaa$Penyakit <- as.factor(aaa$Penyakit)
aaa$Propinsi <- as.factor(aaa$Propinsi)
aaa$Minggu <- as.factor(aaa$Minggu)
aaa$Kota <- as.factor(aaa$Kota)
aaa$Kecamatan <- as.factor(aaa$Kecamatan)
aaa$Puskesmas <- as.factor (aaa$Puskesmas)

bbb <- gabungan %>% group_by(Minggu, Penyakit, Propinsi) %>%
  summarize("jumlah alert" = sum(jumlah),
            "jumlah alert direspon" = sum(verif),
            "jumlah KLB" = sum(Status == "KLB"),
            "% alert direspon" = (sum(verif)/sum(jumlah))*100)

bbb$Penyakit <- as.factor(bbb$Penyakit)
bbb$Propinsi <- as.factor(bbb$Propinsi)
bbb$Minggu <- as.factor(bbb$Minggu)

data_2021 <- read_excel("data dummy.xlsx",sheet="prov 2021")
dat2022 <- read_excel("data dummy.xlsx",sheet="prov 2022")
data_2022 <- dat2022 %>% select(c("unik","kasus"))
gabung <- merge(x=data_2021,y=data_2022,by="unik",all.x = TRUE)
gabung <- rename(gabung, "kasus2021" = "kasus.x",
                 "kasus2022" = "kasus.y")
prov <- unique(gabung$propinsi)
disease <- unique(gabung$Penyakit)

#datasetkedua
datsar <- read_excel("data provinsi penyakit.xlsx",sheet = "data dasar")
kamus <- read_excel("data provinsi penyakit.xlsx",sheet = "kamus")

datsar <- gather(datsar,minggu,kasus,`1`:`28`,factor_key = TRUE)
datsar$kasus <- as.numeric(datsar$kasus)
dasar <- merge(x=datsar,y=kamus,by="Provinsi")
dasar$Penyakit <- as.factor(dasar$Penyakit)

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  navbarPage(theme = shinytheme("cerulean"),collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">EWARS HELP DASHBOARD</a>'),id="nav",
             windowTitle = "EWARS Help Dashboard",
             tabPanel(
               "Trend Penyakit",tags$div(
                 tags$h1("Trend Penyakit SKDR 2023 Berdasarkan Pulau")
               ),
               tags$br(),
               sidebarPanel(
                 selectInput(
                   inputId = "sakit",
                   label = "pilih Penyakit terlebih dahulu",
                   choices = dasar$Penyakit,
                   selected = dasar$Penyakit[1]
                 )
               ),
               mainPanel(
                 plotlyOutput(outputId = "pnasi"),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "pjabal"),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "psuma"),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "pkali"),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "psula"),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "pmalu"),
                 tags$br(),
                 tags$br(),
                 plotlyOutput(outputId = "ppapua"),
                 tags$br(),
                 tags$br(),
               )
             ),
             tabPanel(
               "Trend tahunan",tags$div(
                 tags$h1("Trend Penyakit Dalam SKDR Sejak 2021"),
               ),
               tags$br(),
               sidebarPanel(
                 selectInput(
                   inputId = "AA",
                   label = "pilih Provinsi",
                   choices = prov,
                   selected = "ACEH"
                 ),
                 selectInput(
                   inputId = "BB",
                   label = "pilih penyakit",
                   choices = disease,
                   selected = "DIARE AKUT"
                 )
               ),
               mainPanel(
                 plotlyOutput(
                   outputId = "graph"
                 )
               )
             ),
             tabPanel("Alert Mingguan",
                      tags$div(
                        tags$h1("Rekapitulasi Alert Potensial Penyakit Per Provinsi"),
                      ),
                      tags$br(),tags$br(),
                      fluidRow(
                        column(12,
                               dataTableOutput('table')
                        )
                      ),
                      tags$br(),tags$br(),
                      ),
             tabPanel("Download Data",
               tags$div(
                 tags$h4()
               ),
               selectInput("dataset",
                           label = "Pilih Data Yang ingin di-download",
                           choices = c("rekap alert provinsi",
                                       "rekap alert lengkap",
                                       "linelist alert lengkap",
                                       "linelist alert yang menjadi KLB",
                                       "linelist Penyakit Per Provinsi (2021)",
                                       "linelist Penyakit Per Provinsi (2022)")),
               radioButtons("formatdownload","tipe file yang diinginkan",
                            choices = c("csv" = ".csv",
                                        "Excel" = ".xlsx",
                                        "tsv" = ".tsv"),
                            inline = TRUE),
               downloadButton("unduh","Download")
             ),
             tabPanel("Tentang Dashboard",
                      tags$div(
                        tags$br(),tags$br(),tags$h4("Tentang Dahshboard"),
                        "Dashboard ini adalah bentuk prototipe visualisasi data untuk membantu menyajikan data rekapitulasi alert menurut provinsi, minggu pelaporan,
               dan juga jenis penyakit potensial KLB (kejadian luar biasa). Dashboard ini dikembangkan dengan packaged dasar 'shiny' melalui aplikasi program Rstudio.
               Dashboard ini ter-deploy dan terpublish secara public melalui laman gratis yaitu shinyapps. Segala hal tentang dashboard ini bersifat rahasia dan confidential.",
                        tags$br(), tags$br(), tags$h4("Tentang Data"),
                        "Data dalam dashboard ini diperoleh dari pelaporan rutin mingguan yang dilakukan oleh petugas surveilans di puskesmas melalui laman ",
                        tags$a(href="https://skdr.surveilans.org/dashboard","SKDR Surveilans Kementerian Kesehatan RI"),
                      )
             )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  datasetInput <- reactive({
    switch(input$dataset,
           "rekap alert provinsi" = bbb,
           "rekap alert lengkap" = aaa,
           "linelist alert lengkap" = data_alert,
           "linelist alert yang menjadi KLB" = data_klb,
           "linelist Penyakit Per Provinsi (2021)" = data_2021,
           "linelist Penyakit Per Provinsi (2022)" = dat2022
           )
  })
  output$unduh <- downloadHandler(
    filename = function() {
      paste0(input$dataset,"_unduh",input$formatdownload)
    },
    content = function(fname) {
      if (input$formatdownload == ".csv"){
        write.csv(datasetInput(),fname,row.names = FALSE)
      } else if (input$formatdownload == ".xlsx") {
        write_xlsx(datasetInput(),fname)
      } else if (input$formatdownload == ".tsv") {
        write.table(datasetInput(),fname, quote = FALSE,sep = '\t',row.names = FALSE)
      }
    }
  )
  output$table <- renderDataTable(datatable(bbb,
                                     extensions = "Buttons",
                                     options= list(
                                       autowidth = TRUE
                                     ),
                                     style = "auto",
                                     filter = list (position = "top",
                                                    clear = TRUE,plain = TRUE)))
  output$graph <- renderPlotly({
    
    filtered <-
      filter(gabung, gabung$Penyakit == input$BB & gabung$propinsi==input$AA)
    
    ggplotly(ggplot(filtered,aes(x=filtered$minggu)) +
               geom_line(aes(y=filtered$kasus2021,color="2021"))+
               geom_line(aes(y=filtered$kasus2022,color="2022"))+
               labs(title=paste("Trend Mingguan ",input$BB," (SKDR) di Provinsi ",input$AA," Tahun 2021-2022",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_test())
    
  })
  output$pnasi <- renderPlotly({
    datanasional <- filter(dasar, dasar$Penyakit == input$sakit)
    datanasional <- filter(datanasional,datanasional$pulau == "Nasional")
    
    ggplotly(ggplot(datanasional,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               labs(title=paste("TREND ",input$sakit," SECARA NASIONAL TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
  output$pjabal <- renderPlotly({
    datajabal <- filter(dasar, dasar$Penyakit == input$sakit)
    datajabal <- filter(datajabal,datajabal$pulau == "Jawa-Bali")
    
    ggplotly(ggplot(datajabal,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               facet_wrap(~datajabal$Provinsi,scales = "free_y")+
               labs(title=paste("TREND ",input$sakit," DI JAWA-BALI TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
  output$psuma <- renderPlotly({
    datasuma <- filter(dasar, dasar$Penyakit == input$sakit)
    datasuma <- filter(datasuma,datasuma$pulau == "Sumatera")
    
    ggplotly(ggplot(datasuma,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               facet_wrap(~datasuma$Provinsi,scales = "free_y")+
               labs(title=paste("TREND ",input$sakit," DI PULAU SUMATERA TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
  output$pkali <- renderPlotly({
    datakali <- filter(dasar, dasar$Penyakit == input$sakit)
    datakali <- filter(datakali,datakali$pulau == "Kalimantan")
    
    ggplotly(ggplot(datakali,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               facet_wrap(~datakali$Provinsi,scales = "free_y")+
               labs(title=paste("TREND ",input$sakit," DI PULAU KALIMANTAN TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
  output$psula <- renderPlotly({
    datasula <- filter(dasar, dasar$Penyakit == input$sakit)
    datasula <- filter(datasula,datasula$pulau == "Sulawesi")
    
    ggplotly(ggplot(datasula,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               facet_wrap(~datasula$Provinsi,scales = "free_y")+
               labs(title=paste("TREND ",input$sakit," DI PULAU SULAWESI TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
  output$pmalu <- renderPlotly({
    datamalu <- filter(dasar, dasar$Penyakit == input$sakit)
    datamalu <- filter(datamalu,datamalu$pulau == "Maluku-Nusa Tenggara")
    
    ggplotly(ggplot(datamalu,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               facet_wrap(~datamalu$Provinsi,scales = "free_y")+
               labs(title=paste("TREND ",input$sakit," DI MALUKU-NUSA TENGGARA TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
  output$ppapua <- renderPlotly({
    datapapua <- filter(dasar, dasar$Penyakit == input$sakit)
    datapapua <- filter(datapapua,datapapua$pulau == "Papua")
    
    ggplotly(ggplot(datapapua,aes(x=`minggu`,y=`kasus`,group = 1))+
               geom_line(color="Blue")+
               facet_wrap(~datapapua$Provinsi,scales = "free_y")+
               labs(title=paste("TREND ",input$sakit," DI PULAU PAPUA TAHUN 2023",sep = "")) +
               labs(x="Minggu",y="Jumlah Kasus") +
               theme_minimal()
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
