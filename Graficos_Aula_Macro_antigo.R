setwd("~/Doutorado/PED - CE342/Aulas/Macro/Empirico")

source('Pacotes.R', verbose = F)

#######################################################################
#                                                                     #
#                     MANIPULAÇÃO E LIMPEZA DOS DADOS                 #
#                                                                     #
#######################################################################

# Começar os dados em 1999/08 e terminar em 2021/10, se possível

# IPCA - Cheio ####
ipca_cheio <- read.csv("Dados/IPCA_cheio.csv", sep = ';', dec = ',', header = F)
ipca_cheio <- as.xts(ts((ipca_cheio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Metas de inflação - Anual ####
metas <- read.csv("Dados/Metasdadosanual.csv", dec = ',')
metas[2:5] <- metas[2:5] * 100
metas

# Metas de inflação - Mensal ####
metas_mensal <- read.csv("Dados/Metasdadosmensal.csv", sep = ';', dec = ',')
metas_mensal[2:4] <- metas_mensal[2:4] * 100
metas_mensal

# IPCA e tradables/nontradables/monitorados ####

IPCA_decomp <- read.csv("Dados/IPCA_decomp.csv", sep = ',', dec = ',', header = T)

IPCA_decomp <- tibble(IPCA_decomp) %>%
  mutate(across(!Data, ~(as.numeric(.)/100)+1)) %>%
  mutate(across(!Data, ~ round((RcppRoll::roll_prodr(., n = 12, na.rm = F, fill = NA)-1)*100,2)))
glimpse(IPCA_decomp)

IPCA_ndur <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_semidur <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),3]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_dur <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),4]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_servicos <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),5]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_trad <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),6]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_monit <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),7]), start = c(1999,8), end = c(2021,10), frequency = 12))
IPCA_nontrad <- as.xts(ts((IPCA_decomp[92:nrow(IPCA_decomp),8]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Cambio ####
cambio <- read.csv("Dados/cambio.csv", sep = ';', dec = ',', header = T)
cambio <- as.xts(ts((cambio[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))

# Commodities ####
ibc <- read.csv("Dados/commodities.csv", sep = ';', dec = ',', header = T)
ibc_total <- as.xts(ts((ibc[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
ibc_agro <- as.xts(ts((ibc[,3]), start = c(1999,8), end = c(2021,10), frequency = 12))
ibc_metal <- as.xts(ts((ibc[,4]), start = c(1999,8), end = c(2021,10), frequency = 12))
ibc_energia <- as.xts(ts((ibc[,5]), start = c(1999,8), end = c(2021,10), frequency = 12))

ibc_df <- reshape2::melt(ibc, id.vars = "Data")
ibc_df$Data <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")


#ggplot(ibc_df) + geom_line(aes(x=Data,y=value,color=variable,linetype=variable), size = 1)

# Selic - Meta ####

selic <- read.csv("Dados/selic.csv")
selic <- as.xts(ts((selic[38:nrow(selic),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(selic)

# EMBI+ ####

embi <- read.csv("Dados/EMBI.csv")
embi
embi <- as.xts(ts((embi[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(embi)

# FED rates ####

fed_rate <- read.csv("Dados/FED_rates.csv")
fed_rate
fed_rate <- as.xts(ts((fed_rate[152:nrow(fed_rate),2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(fed_rate)

# PIB ####

pib <- read.csv("Dados/PIB.csv")
pib
pib <- as.xts(ts((pib[11:nrow(pib),2]), start = c(1999,3), end = c(2021,2), frequency = 4))
plot(pib)

# hiato_pib ####

hiato_pib <- read_excel("Dados/hiato_PIB.xlsx")
hiato_pib <- as.xts(ts((hiato_pib[23:nrow(hiato_pib),4]), start = c(1999,3), end = c(2021,2), frequency = 4))
plot(hiato_pib)

# VIX  ####
vix <- read.csv("Dados/VIX.csv",dec = ',')
vix
vix <- as.xts(ts((vix[,2]), start = c(1999,8), end = c(2021,10), frequency = 12))
plot(vix)

# MSCI ####
MSCI <- read.csv("Dados/MSCI.csv", dec = ',')
MSCI
MSCI <- as.xts(ts((MSCI[,2]), start = c(2004,8), end = c(2021,10), frequency = 12))
plot(MSCI)

#######################################################################
#                                                                     #
#                     GRÁFICOS                                        #
#                                                                     #
#######################################################################

# Criando diretórios
periodos_aula <- c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)','Agregado')

for (i in periodos_aula) {
  ifelse(!dir.exists(file.path(i)),
         dir.create(file.path(i)),
         FALSE)
}

setwd(file.path('Figuras_LP_Diff', nome_modelo))

# Gráficos de RMI

date <- seq(1999,2022,1)

df <- tibble(date,metas[,2],metas[,3],metas[,4],metas[,5], .name_repair = ~ c('date','Meta','Piso','Teto','IPCA'))

# rects <- rects[5,]
# rects
rects <- data.frame(xstart = c(1999,2004,2011,2015,2017,2019),
                    xend = c(2004,2011,2015,2017,2019,2021),
                    col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))


metasplot <- ggplot(df)  + 
  #geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = "#BDD7E7", alpha = 0.4) +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  geom_line(aes(x=date,y=IPCA, color = 'IPCA'), stat = 'identity', size = 1) +
  geom_point(aes(x=date,y=IPCA), size = 3, color = 'darkred') +
  geom_line(aes(x=date, y=Piso, color = 'Piso e teto'),stat = 'identity',size = 1, linetype = 'dotted') +
  geom_line(aes(x=date, y=Teto),stat = 'identity', color = 'darkred',size = 1, linetype = 'dotted') +
  geom_line(aes(x=date, y=Meta, color = 'Meta'),stat = 'identity', size = .75, linetype = 'dashed') +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks=seq(1999,2022,1),                       
                     labels=paste(c(""),rep(1999:2022,each=1)),expand = c(0.012, 0.02)) +
  labs(title = 'Taxa de Inflação acumulada em 12 meses e bandas do Regime de Metas da Inflação.') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  scale_color_manual( name="",
                      labels=c("IPCA","Meta","Piso e teto"),
                      values=c('IPCA' = "darkred",'Meta' = "darkred","Piso e teto" = "darkred"),
                      guide=guide_legend(override.aes=list(linetype=c(1,2,3), lwd=c(1,0.75,0.75)))) +
  scale_linetype_manual(breaks=c("IPCA","Meta","Piso e Teto"),
                        values=c('IPCA' = "solid",'Meta' = "dashed","Piso e Teto" = "dotted")) +
  scale_linetype_manual(values = c(1,2,3)) +
  guides(fill = F) +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="right",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

metasplot
date <- seq(as.Date("1999-08-1"), as.Date("2021-10-1"), by = "month")

df <- tibble(date,metas_mensal[,2],metas_mensal[,3],metas_mensal[,4],metas_mensal[,5], 
             .name_repair = ~ c('date','Meta','Piso','Teto','IPCA'))

# rects_mensal <- data.frame(xstart = c(1999,2004,2011,2015,2017,2019),
#                     xend = c(2004,2011,2015,2017,2019,2021),
#                     col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))


rects_mensal <- data.frame(xstart = c(as.Date("1999-08-1"),as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1")),
                           xend = c(as.Date("2004-12-1"),as.Date("2011-12-1"),as.Date("2015-12-1"),as.Date("2017-12-1"),as.Date("2019-12-1"),as.Date( "2021-10-1")),
                           col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

# rects_mensal <- data.frame(xstart = c(as.Date(1999, origin = "1999-08-1"),as.Date(2004, origin = "2004-12-1"),as.Date(2011, origin = "2011-12-1"),
#                                       as.Date(2015, origin = "2015-12-1"),as.Date(2017, origin = "2017-12-1"),as.Date(2019, origin = "2019-12-1")),
#                            xend = c(as.Date(2004, origin = "2004-12-1"),as.Date(2011, origin = "2011-12-1"),
#                                     as.Date(2015, origin = "2015-12-1"),as.Date(2017, origin = "2017-12-1"),as.Date(2019, origin = "2019-12-1"),as.Date(2021, origin = "2021-10-1")),
#                            col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)','VI (20-21)'))

# rects_mensal[,1] <- as.numeric(rects_mensal[,1])
# rects_mensal[,2] <- as.numeric(rects_mensal[,2])
# 
# df
# df$date <- as.numeric(df$date)
# rects_mensal
View(df)
rects_mensal

metas_mensalplot <- ggplot(df)  + 
  geom_ribbon(aes(x=date, ymin=Meta, ymax =Teto), fill = "GREY80") +
  geom_ribbon(aes(x=date, ymin=Piso, ymax =Meta), fill = "GREY80") +
  geom_rect(data = rects_mensal, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
  geom_line(aes(x=date, y=Piso), color = "GREY70") +
  geom_line(aes(x=date, y=Teto), color = "GREY70") +
  geom_line(aes(x=date, y=Meta), color = "GREY50") +
  geom_line(aes(x=date,y=IPCA), size = 1, color = 'darkred') +
  scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
  scale_x_date(date_breaks = '1 year', date_minor_breaks = '6 months', date_labels = "%y",expand = c(0, 0.02)) +
  # scale_x_continuous(breaks=seq(1999,2021,1),                       
  #                     labels=paste(c(""),rep(1999:2021,each=1)),expand = c(0.012, 0.02)) +
  labs(title = 'Taxa de Inflação acumulada em 12 meses e bandas do Regime de Metas da Inflação.') +
  scale_fill_brewer(palette="Blues", name = 'Períodos') +
  guides(fill = "none") +
  ylab('') +
  xlab('') +
  theme_classic() +
  theme(  panel.grid = element_blank(), 
          panel.border = element_blank(),
          legend.position="right",
          legend.title = element_blank(),
          legend.text = element_text(size=10),
          legend.key = element_rect(colour = "black"),
          legend.box.background = element_rect(colour = "black", size = 1),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
          axis.text.y = element_text(size=10)) 

metas_mensalplot























































































  # # Gráfico Periodização ####
# 
# # IPCA
# ipcacompleto <- read.csv("Dados/ipcacompleto.csv")
# ipcaacumulado <-read.csv('Dados/ipcaacumulado.csv')
# 
# # Cambio
# cambiocompleto <- read.csv("Dados/cambiocompleto.csv")
# 
# # PIB
# pibcompleto <- read_excel('Dados/pibtrimreal.xlsx', col_names = c('data','Taxa12meses','Taxatrimestral'))
# # Transformando em objetos xts
# ipca <- as.xts(ts((ipcaacumulado[2:nrow(ipcaacumulado),2]), start = c(1999,1), end = c(2019,12), frequency = 12))
# cambio <- as.xts(ts((cambiocompleto[,2]), start = c(1999,1), end = c(2019,12), frequency = 12))
# cambio
#   
# cor(cambio,ipca)
# pib <- as.xts(ts(pibcompleto[12:nrow(pibcompleto),2], start = c(1999,1), end = c(2019,4), frequency = 4))
# 
# # Trimestralizando
# ipca <- apply.quarterly(ipca,median)
# cambio <- apply.quarterly(cambio,median)
# 
# cor(pib,ipca)
# cor(cambio,ipca)
# # Criando base de dados
# date <- seq(1999,2019.76,0.25)
# date
# 
# df <- tibble(date,cambio,ipca,pib, .name_repair = ~ c('date','cambio','IPCA','PIB'))
# 
# df
# 
# plot_lst <- vector("list", length = 3)
# options(digits = 2)
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.75), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# rects
# cambioplot <- ggplot(df )  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_line(aes(x=date, y=cambio), size = 0.75, color = 'darkred') +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1T"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxa de Câmbio Nominal') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9, colour = 'black',face = 'bold'),
#           axis.text.y = element_text(size=10,face = 'bold')) 
# 
# 
# IPCAplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   # geom_hline(yintercept = -0.00675634851467799*100, colour= 'darkgrey') +
#   # geom_hline(yintercept = 0.0372755026193464*100, colour= 'darkgrey') +
#   geom_line(aes(x=date, y=IPCA*100),size = 0.75,color = 'darkred') +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 4)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1T"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'IPCA') +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="right",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9, colour = 'black',face = 'bold'),
#           axis.text.y = element_text(size=10,face = 'bold')) 
# 
# IPCAplot
# PIBplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   geom_line(aes(x=date, y=PIB), size = 0.75, color = 'darkred') +
#   scale_fill_brewer(palette="Blues") +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1T"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 4)) +
#   labs(title = 'PIB') +
#   ylab('') +
#   guides(fill = F) +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9, colour = 'black', face = 'bold'),
#           axis.text.y = element_text(size=10,face = 'bold')) 
# 
# 
# plot_lst[[1]] <- cambioplot
# plot_lst[[2]] <- PIBplot
# plot_lst[[3]] <- IPCAplot
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = 0.66, height = .33, width = 0.85) +
#   draw_plot(plot_lst[[3]], x = 0, y = 0.33, height = .33, width = 1) +
#   draw_plot(plot_lst[[2]], x = 0, y = 0, height = .33, width = .85)
# 
# ggsave('FatosEstilizados.png',device = "png")
# 
# 
# 
# # Gráfico Taxa de câmbio e volatilidade ####
# cambiovol <- read.csv2('Dados/cambio1998.csv', sep = ';', dec = ',')
# cambiovol
# cambiovol <- ts((cambiovol[10:nrow(cambiovol),2]), start = c(1998,12), end = c(2019,12), frequency = 12) 
# # cambiodiario <- read.csv('Dados/taxadecambiodiaria.csv', sep = ';', dec = ',')
# # cambiodiario <- ts((cambiodiario[1:nrow(cambiodiario),2]), start = c(1999,1), end = c(2019,1), frequency = 252)
# 
# # Volatilidade cambial - MSD #
# cambiovol
# 
# # Retorno da taxa de câmbio (a série passa a começar em 02/1999)
# cambio.rets = CalculateReturns(cambiovol, "discrete")
# cambio.rets
# 
# # Descartando primeira observação (NA)
# cambio.rets
# cambio.rets2 <- cambio.rets ^ 2
# cambio.rets2
# cambio.rets2 <- ts(cambio.rets2[-1], start = c(1999,1), frequency = 12)
# cambio.rets2
# 
# # MSD (Moving Standard Deviation) #
# 
# # Janela de 1 mes #
# 
# # Calculando SMA
# cambio.MSD.1 <- SMA(cambio.rets2,1)
# cambio.MSD.1
# 
# options(digits = 7)
# date <- seq(1999,2019.99,(1/12))
# length(date)
# date
# length(cambio.MSD.1)
# 
# # EWMA
# 
# # Fator de decaimento
# lambda = 0.94
# 
# # Cálculo do EWMA
# cambio.rets2 <- as.data.frame(cambio.rets2)
# cambio.EWMAM <- covEWMA(cambio.rets2, lambda = 0.97)
# cambio.EWMAM <- ts(cambio.EWMAM[, ,1], start = c(1999,1), frequency = 12)
# 
# # Gráficos
# 
# df <- tibble(date,cambiovol[-1],cambio.MSD.1, .name_repair = ~ c('date','cambio','vol'))
# 
# df
# 
# plot_lst <- vector("list", length = 3)
# 
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-02)','II (03-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# cambioplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=cambio), size = 0.75, color = 'darkred') +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("Jan"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxa de Câmbio Nominal') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# 
# volplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   geom_line(aes(x=date, y=vol),size = 0.75, color = 'darkred') +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("Jan"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Volatilidade da taxa de câmbio nominal') +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           legend.title = element_text(size=11),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# 
# 
# 
# plot_lst[[1]] <- cambioplot
# plot_lst[[2]] <- volplot
# 
# cambiocompleto
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0.5, y = 0, height = 1, width = .5) +
#   draw_plot(plot_lst[[2]], x = 0, y = 0, height = 1, width = .5)
# 
# (cambiocompleto[46,2]-cambiocompleto[45,2])/cambiocompleto[45,2]
# 
# ggsave('NiveleVolatilidadeCambio.png',width = 20, height = 10, units = 'cm', device = "png")
# 
# 
# # Gráfico de Metas e Decomp ####
# 
# metas <- read.csv("Dados/Metasdadosanual.csv", dec = ',')
# metas[2:5] <- metas[2:5] * 100
# metas
# date <- seq(1999,2019,1)
# 
# decomp <- read.csv('Dados/IPCAdecomposto.csv')
# 
# 
# # Gráfico de períodos
# 
# ipcaperiodos <- read.csv('Dados/IPCAperiod.csv', dec = ',')
# ipcaperiodos['Periodo'] <-  c('I (99-02)','II (03-10)','III (11-14)','IV (15-16)','V (17-19)')
# ipcaperiodos
# 
# periodos <- c('I (1999-2003)','II (2004-2010)','III (2011-2014)','IV (2015-2016)','V (2017-2019)')
# 
# df <- tibble(ipcaperiodos[c(1:2,4:6)]) %>%
#   gather('Variable',"Value",-Periodo) %>%
#   mutate(Posicoes =  rep(seq(1,5,1),4)) 
#   # mutate(Value = sprintf("%0.2f", Value))
# 
# df
# df$Value <- round(as.numeric(df$Value) * 100,2)
# df
# 
# c(brewer.pal(n = 5, name = "Blues"),brewer.pal(n = 4, name = "Reds"))
# c("#EFF3FF", "#BDD7E7", "#6BAED6", "#3182BD", "#08519C", "#FEE5D9", "#FCAE91", "#FB6A4A","#CB181D")
#   
# ipcadecompplot <- df %>% ggplot(data = ., aes(x=Posicoes, y = Value, fill = Variable)) +
#   geom_rect(aes(xmin = Posicoes - .5, xmax = Posicoes + .5, ymin = -Inf, ymax = Inf, fill = Periodo), alpha = 0.15, show.legend = F) +
#   geom_bar(position = "dodge", stat = "identity",colour="black",size=0.5) +
#   geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25, size = 3.5, fontface='bold') +
#   scale_x_continuous(name='',breaks=seq(1,5,1),
#                      labels=periodos,
#                      rep(1:5,each=1),
#                      expand = c(0, 0)) +
#   labs(title = 'Decomposição da média da taxa de variação do IPCA acumulada em 12 meses em diferentes componentes') +
#   scale_fill_manual(values = c(
#     "#EFF3FF", # I
#     "#BDD7E7", # II
#     "#6BAED6", # III
#     "#FEE5D9", # IPCA
#     "#3182BD", # IV
#     "#FCAE91", # Monitorados
#     "#FB6A4A", # Nontradables
#     "#CB181D", # Tradables
#     "#08519C"  # V
#     ),
#     breaks = c(
#       "IPCA" ,
#       "Monitorados" ,
#       "Nontradables",
#       "Tradables")
#     ) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 8),expand = c(0, 0)) +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position= "right",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = ggtext::element_markdown(size = 11),
#           axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.6,size=12, colour = 'black'),
#           axis.text.y = element_text(size=10)) +
#   coord_cartesian(ylim = c(0,15.5))
#   
# plot_lst <- vector("list", length = 2)
# 
# plot_lst[[1]] <- metasplot
# plot_lst[[2]] <- ipcadecompplot
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = .55, height = .45, width = 1) +
#   draw_plot(plot_lst[[2]], x = 0, y = 0, height = .55, width = 1)
# 
# ggsave('RMIedecomp.png',device = "png")
# 
# 
# brewer.pal(n = 5, name = "Blues")
# # Gráfico Comm x Cambio x Tradables ####
# 
# # Dados
# 
# ipcadecomp <- read.csv('Dados/IPCAdecomposto.csv')
# comm <- read.csv("Dados/comm.csv")
# ipcadecomp
# 
# # Objetos ts
# comm <- ts((comm[1:nrow(comm),2]),  start = c(1999,1), end = c(2019,12), frequency = 12)
# cambio <- ts((cambiocompleto[,2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# ipcatradables <- ts((ipcadecomp[2:nrow(ipcadecomp),4]), start = c(1999,1), end = c(2019,12), frequency = 12)
# ipca <- ts((ipcadecomp[2:nrow(ipcadecomp),2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# alimbebs <- ts((ipcadecomp[2:nrow(ipcadecomp),3]), start = c(1999,1), end = c(2019,12), frequency = 12)
# 
# (cambio[60]-cambio[48])/cambio[48]
# 
# ipcatradables
# plot(ipcatradables)
# date <- seq(1999,2019.99,(1/12))
# date
# df <- tibble(date,comm,cambio, ipcatradables,ipca,alimbebs, .name_repair = ~ c('date','comm','cambio','Tradables','IPCA','Alimentosbebidas'))
# 
# df
# 
# plot_lst <- vector("list", length = 3)
# max(df$comm)
#   
# max(df$cambio)
# 
# plot(comm)
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# cambioplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=cambio, color = "Taxa de câmbio nominal"), size = 0.75) +
#   geom_line(aes(x=date, y=comm*0.02, color = "Índice de Preços de Commodities"), size = 0.75) +
#   scale_y_continuous(
#     # Features of the first axis
#     name = "",
# 
#     # Add a second axis and specify its features
#     sec.axis = sec_axis(~./max(df$cambio)*(max(df$comm)), name="")
#   ) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("Jan"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   scale_color_manual( name="",
#                       breaks=c("Taxa de câmbio nominal","Índice de Preços de Commodities"),
#                       values=c("Taxa de câmbio nominal" = 'red',"Índice de Preços de Commodities" = 'blue')) +
#   labs(title = '') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   guides(fill = F) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           plot.title = element_blank(),
#           legend.title = element_text(size=11),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9, colour = 'black'),
#           axis.text.y = element_text(size=12),
#           axis.title.y.right = element_text(size=8)) 
# cambioplot
# 
# commplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=comm),size = 0.75, color = 'darkred') +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("Jan"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Índice de Preços de Commodities') +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           legend.title = element_text(size=11),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# 
# tradablesplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   geom_line(aes(x=date, y=Alimentosbebidas*100, color = 'Alimentos e bebidas'),stat = 'identity',size = 1, linetype = 'twodash') +
#   geom_line(aes(x=date, y=Tradables * 100,color = 'Tradables'),stat = 'identity',size = 1,linetype = 'solid') +
#   geom_line(aes(x=date, y=IPCA*100,color = 'IPCA'),stat = 'identity',size = 0.75, linetype = 'dashed') +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("Jan"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxas de variação acumuladas em 12 meses do IPCA de alimentos e bebidas, de bens *tradables* e do índice cheio', size = 8) +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   scale_color_manual( name="",
#                       breaks=c("Alimentos e bebidas","Tradables","IPCA"),
#                       values=c("Alimentos e bebidas" = 'darkred',"Tradables"='darkorange',"IPCA"="black"),
#                       guide=guide_legend(override.aes=list(linetype=c(6,1,2), lwd=c(0.75,0.5,0.5)))) +
#   ylab('') +
#   # guides(fill = F) +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           legend.title = element_text(size=11),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = ggtext::element_markdown(size = 12),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# 
# 
# tradablesplot
# plot_lst[[1]] <- cambioplot
# # plot_lst[[2]] <- commplot
# # plot_lst[[3]] <- tradablesplot
# 
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = 0.66, height = 0.33, width = 1) +
#   draw_plot(plot_lst[[2]], x = 0, y = 0.33, height = 0.33, width = 1) +
#   draw_plot(plot_lst[[3]], x = 0, y = 0, height = 0.33, width = 1)
# 
# 
# ggsave('Cambiocommtradables.png', units = 'cm', device = "png")
# 
# 
# # Gráfico Selic x Embi ####
# 
# 
# comm <- read.csv("Dados/comm.csv")
# selic <- read.csv('Dados/selic.csv')
# selic
# embi <- read.csv('Dados/embi.csv')
# embi
# fedfunds <- read.csv('Dados/fedfunds.csv')
# fedfunds
# vix <- read.csv('Dados/vix.csv', dec = ',')
# vix <- data.frame(vix[,3])
# vix <- vix[seq(dim(vix)[1],1),]
# vix
# 
# # Objetos ts
# comm <- ts((comm[1:nrow(comm),2]),  start = c(1999,1), end = c(2019,12), frequency = 12)
# cambio <- ts((cambiocompleto[,2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# selic <- ts((selic[1:nrow(selic),2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# embi <- ts((embi[58:nrow(embi),2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# fedfunds <- ts((fedfunds[157:nrow(fedfunds),2])/100, start = c(1999,1), end = c(2019,12), frequency = 12)
# vix <- ts(vix, start = c(1999,1), end = c(2019,12), frequency = 12)
# fedembi <- fedfunds + embi/100
# fedembi
# embi <- embi/100
# selic
# 
# date <- seq(1999,2019.99,(1/12))
# date
# df <- tibble(date,comm,cambio, selic, fedembi,vix, .name_repair = ~ c('date','comm','cambio','selic','fedembi','vix'))
# 
# 
# plot_lst <- vector("list", length = 4)
# 
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# cambioplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=cambio), size = 0.75, color = 'darkred') +
#   scale_x_continuous(breaks=seq(1999,2019,2),                       
#                      labels=paste(c(""),rep(seq(1999,2019,2),each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxa de Câmbio Nominal') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           plot.title = ggtext::element_markdown(size = 10),
#           legend.position="none",
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=8, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# cambioplot
# 
# commplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=comm),size = 0.75, color = 'darkred') +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,2),                       
#                      labels=paste(c(""),rep(seq(1999,2019,2),each=1)),expand = c(0, 0)) +
#   labs(title = 'Índice de Preços de *Commodities*') +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           legend.title = element_text(size=10),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           plot.title = ggtext::element_markdown(size = 10),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=8, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# commplot
# 
# 
# vixplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=vix),size = 0.75, color = 'darkred') +
#   scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c(""),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Índice VIX') +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="none",
#           legend.title = element_text(size=10),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           plot.title = ggtext::element_markdown(size = 12),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=8, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# vixplot 
# 
# selicplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_line(aes(x=date, y=selic, color = 'Selic'), size = 0.75, stat = 'identity', linetype = 6) +
#   geom_line(aes(x=date, y=fedembi, color = "*Fed Funds Rates* e EMBI+ Brasil"), size = 0.65,stat = 'identity', linetype = 1) +
#   # geom_bar(data=df2, aes(x=date, y=count*100, fill=type),stat="identity") +
#     # geom_ribbon(aes(x=date,ymin=fedembi, ymax = selic), fill = 'indianred1', alpha = 0.6) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 8),expand = c(0, 0)) +
#   # geom_ribbon(data = . %>% filter(selic > fedembi),
#   #             aes(x=date, ymin = fedembi, ymax = selic), 
#   #             fill="darkred", alpha= 0.7) +
#   # geom_ribbon(data = . %>% filter(selic < fedembi),
#   #             aes(x=date,ymin = selic, ymax = fedembi), 
#   #             fill = 'blue1', alpha = 0.7) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c(""),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxa Selic anualizada e *Fed Funds Rates* somado ao EMBI+ Brasil') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   scale_color_manual( name="",
#                       breaks=c("Selic","*Fed Funds Rates* e EMBI+ Brasil"),
#                       values=c("Selic" = 'darkred',"*Fed Funds Rates* e EMBI+ Brasil"="black"),
#                       guide=guide_legend(override.aes=list(linetype=c(6,1), lwd=c(0.75,0.75)))) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           legend.title = element_text(size=11),
#           legend.text = ggtext::element_markdown(size = 10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = ggtext::element_markdown(size = 12),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=8, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# selicplot
# 
# plot_lst[[1]] <- cambioplot
# plot_lst[[2]] <- commplot
# plot_lst[[3]] <- vixplot
# plot_lst[[4]] <- selicplot
# 
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = 0.7, height = 0.3, width = 0.5) +
#   draw_plot(plot_lst[[2]], x = 0.5, y = 0.7, height = 0.3, width = 0.5) +
#   draw_plot(plot_lst[[3]], x = 0, y = 0.4, height = 0.3, width = 1) +
#   draw_plot(plot_lst[[4]], x = 0, y = 0, height = 0.4, width = 1)
# 
# # ggdraw() +
# #   draw_plot(plot_lst[[1]], x = 0, y = 0.5, height = 0.5, width = 0.5) +
# #   draw_plot(plot_lst[[2]], x = 0.5, y = 0.5, height = 0.5, width = 0.5) +
# #   draw_plot(plot_lst[[3]], x = 0, y = 0, height = 0.5, width = 0.5) +
# #   draw_plot(plot_lst[[4]], x = 0.5, y = 0, height = 0.5, width = 0.5)
# 
# 
# ggsave('Cambiocommselic.png', units = 'cm', device = "png")
# 
# # Gráfico Cambio x Monitorados ####
# 
# ipcadecomp <- read.csv('Dados/IPCAdecomposto.csv')
# comm <- read.csv("Dados/comm.csv")
# ipcadecomp
# 
# # Objetos ts
# cambio <- as.xts(ts((cambiocompleto[,2]), start = c(1999,1), end = c(2019,12), frequency = 12))
# monit <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),6]), start = c(1999,1), end = c(2019,12), frequency = 12))
# ipca <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),2]), start = c(1999,1), end = c(2019,12), frequency = 12))
# ipcatradables <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),4]), start = c(1999,1), end = c(2019,12), frequency = 12))
# nontradables <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),5]), start = c(1999,1), end = c(2019,12), frequency = 12))
# alimbebs <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),3]), start = c(1999,1), end = c(2019,12), frequency = 12))
# date <- seq(1999,2019.99,(1/12))
# df1 <- tibble(date,cambio,ipca, .name_repair = ~ c('date','Cambio','IPCA'))
# 
# # Trimestralizando
# ipca <- as.numeric(apply.quarterly(ipca,median))
# cambio <- as.numeric(apply.quarterly(cambio,median))
# monit <- as.numeric(apply.quarterly(monit,median))
# ipcatradables <- as.numeric(apply.quarterly(ipcatradables,median))
# nontradables <- as.numeric(apply.quarterly(nontradables,median))
# 
# 
# date <- seq(1999,2019.99,(1/4))
# date
# df <- tibble(date,cambio, monit,ipca,ipcatradables,nontradables, .name_repair = ~ c('date','Cambio','Monitorados','IPCA','Tradables','Nontradables'))
# 
# df
# 
# plot_lst <- vector("list", length = 2)
# max(df$Cambio)/max(df$IPCA)
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# cambioplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = T) +
#   # geom_line(aes(x=date, y=IPCA * 25),stat = 'identity',size = 1, color = 'black', linetype = 'solid') +
#   geom_line(aes(x=date, y=Cambio), size = 1, color = 'darkred') +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1ºT"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   # scale_y_continuous(
#   #   # Features of the first axis
#   #   name = "Taxa de câmbio nominal",
#   #   
#   #   # Add a second axis and specify its features
#   #   sec.axis = sec_axis(~.*max(df$IPCA)*25, name="IPCA",labels = function(x) paste0(x, "%"))
#   # ) +
#   # scale_color_manual(breaks = c('Taxa de câmbio nominal','IPCA'),
#   #                    values=c('Taxa de câmbio nominal' ='red', 'IPCA'="black")) +
#   labs(title = 'Taxa de Câmbio Nominal') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   guides(fill = F) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9,face='bold', colour = 'black'),
#           axis.text.y = element_text(size=10),
#           axis.title.y.right = element_text(size=10)
#   ) 
# cambioplot
# 
# tradablesplot <- ggplot(df)  + 
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_line(aes(x=date, y=IPCA * 100,color = 'IPCA'),stat = 'identity',size = 1,linetype = 'solid') +
#   geom_line(aes(x=date, y=Monitorados*100,color = 'Monitorados'),stat = 'identity',size = 0.75) +
#   geom_line(aes(x=date, y=Nontradables*100,color = 'Não tradables'),stat = 'identity',size = 0.75) +
#   geom_line(aes(x=date, y=Tradables*100,color = 'Tradables'),stat = 'identity',size = 0.75) +
#   
#   geom_point(aes(x=date,y=IPCA * 100, color = 'IPCA', shape = 'IPCA' ),stat = 'identity', size = 2,show.legend = T) +
#   geom_point(aes(x=date,y=Monitorados * 100, color = 'Monitorados', shape = 'Monitorados'),stat = 'identity', size = 2, show.legend = T) +
#   geom_point(aes(x=date,y=Nontradables * 100, color = 'Não tradables', shape = 'Não tradables'),stat = 'identity', size = 2, show.legend = T) +
#   geom_point(aes(x=date,y=Tradables * 100, color = 'Tradables', shape = 'Tradables'),stat = 'identity', size = 2, show.legend = T) +
#   
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1ºT"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Variação acumulada em 12 meses de desagregações do IPCA e índice cheio') +
#   
#   scale_color_manual(breaks = c('IPCA','Monitorados','Tradables','Não tradables'),
#                      values=c('IPCA'="black",'Monitorados'= "red1", 'Tradables' = "orange", 'Não tradables' = 'blue')) +
#   scale_shape_manual(values = c(4,19,17,15)) +
#   
#   ylab('') +
#   xlab('') +
#   guides(name = '',
#          fill = F, 
#          color = F, 
#          shape = guide_legend(override.aes = list(size = 3,
#                                                   shape = c(4,19,17,15),
#                                                   colour = c('black', 'red1','blue','orange')))) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size=9),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = ggtext::element_markdown(size = 11),
#           # plot.title = element_blank(), 
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9,face='bold', colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# tradablesplot
# 
# 
# plot_lst[[1]] <- cambioplot
# plot_lst[[2]] <- tradablesplot
# 
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = 0.6, height = 0.4, width = 1) +
#   draw_plot(plot_lst[[2]], x = 0, y = 0, height = 0.6, width = 1)
# 
# 
# ggsave('Cambiomonitorados.png', units = 'cm', device = "png")
# 
# # Gráfico industrializados ####
# 
# ipcaindust <- read.csv('Dados/IPCAindust.csv')
# ipcaindust
# 
# # Objetos ts
# ipca <- as.xts(ts((ipcaindust[2:nrow(ipcaindust),2]), start = c(1999,1), end = c(2019,12), frequency = 12))
# dura <- as.xts(ts((ipcaindust[2:nrow(ipcaindust),3]), start = c(1999,1), end = c(2019,12), frequency = 12))
# semi <- as.xts(ts((ipcaindust[2:nrow(ipcaindust),4]), start = c(1999,1), end = c(2019,12), frequency = 12))
# ndura <- as.xts(ts((ipcaindust[2:nrow(ipcaindust),5]), start = c(1999,1), end = c(2019,12), frequency = 12))
# 
# ipca <- as.numeric(apply.quarterly(ipca,mean))
# dura <- as.numeric(apply.quarterly(dura,mean))
# semi <- as.numeric(apply.quarterly(semi,mean))
# ndura <- as.numeric(apply.quarterly(ndura,mean))
# 
# brewer.pal(n = 9, name = "Reds")
# # "#FFF5F0" "#FEE0D2" "#FCBBA1" "#FC9272" "#FB6A4A" "#EF3B2C" "#CB181D" "#A50F15" "#67000D"
# 
# date <- seq(1999,2019.99,(1/4))
# date
# df
# df <- tibble(date,ipca,dura,semi,ndura, .name_repair = ~ c('date','IPCA','Duraveis','Semiduraveis', 'Naoduraveis'))
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# ggplot(df)  + 
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_line(aes(x=date, y=IPCA * 100,color = 'IPCA'),stat = 'identity',size = 1,linetype = 'dashed') +
#   geom_line(aes(x=date, y=Duraveis*100,color = 'Duráveis'),stat = 'identity',size = 0.75) +
#   geom_line(aes(x=date, y=Semiduraveis*100,color = 'Semiduráveis'),stat = 'identity',size = 0.75) +
#   geom_line(aes(x=date, y=Naoduraveis*100,color = 'Não duráveis'),stat = 'identity',size = 0.75) +
#   
#   geom_point(aes(x=date,y=IPCA * 100, color = 'IPCA', shape = 'IPCA'),stat = 'identity', size = 2,show.legend = T) +
#   geom_point(aes(x=date,y=Duraveis * 100, color = 'Duráveis', shape = 'Duráveis'),stat = 'identity', size = 2, show.legend = T) +
#   geom_point(aes(x=date,y=Semiduraveis * 100, color = 'Semiduráveis', shape = 'Semiduráveis'),stat = 'identity', size = 2, show.legend = T) +
#   geom_point(aes(x=date,y=Naoduraveis * 100, color = 'Não duráveis', shape = 'Não duráveis'),stat = 'identity', size = 2, show.legend = T) +
#   
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1ºT"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxas de variação acumuladas em 12 meses do IPCA de bens duráveis, não duráveis e semiduráveis e índice cheio (linha tracejada)', size = 4) +
#   
#   scale_color_manual(breaks = c('IPCA','Duráveis','Não duráveis','Semiduráveis'),
#                      values=c('IPCA'="black",'Duráveis'= "red1",  'Semiduráveis' = "blue", 'Não duráveis' = "orange")) +
#   scale_shape_manual(values = c(16,4,17,15)) +
#   
#   ylab('') +
#   xlab('') +
#   guides(name = '',
#          fill = F, 
#          color = F, 
#          shape = guide_legend(override.aes = list(size = 3,
#                                                   shape = c(16,4,17,15),
#                                                   colour = c('red1','black', 'orange', 'blue')))) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = element_blank(),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=14)) 
# 
# ggsave('Industrializados.png',height = 6, width = 10, units = 'in', device = "png")
# 
# 
# 
# # Gráfico Reajustes ####
# 
# # Dados
# ipcainpc <- read.csv('Dados/IPCA-INPC.csv')
# reajustes <- read.csv('Dados/reajustessalariais.csv', dec = ',')
# colnames(reajustes) <- c('ano','Acima','Igual','Abaixo')
# 
# # Verificando séries
# ipcainpc
# reajustes
# 
# # Objetos ts
# dfreaj <- reshape2::melt(reajustes,id.vars='ano', measure.vars = 2:4)
# df
# 
# plot_lst <- vector("list", length = 2)
# 
# rectsreaj <- data.frame(xstart = c(1998.5,2003.5,2010.5,2014.5,2016.5), xend = c(2003.5,2010.5,2014.5,2016.5,2018.5), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-18)'))
# c(brewer.pal(n = 5, name = "Blues"),brewer.pal(n = 4, name = "Reds"))
# 
# 
# reajustesplot <- ggplot(dfreaj)  + 
#   geom_rect(data = rectsreaj, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col),stat="identity", alpha = 0.4, show.legend = F) +
#   geom_bar(aes(x=ano, y=value,fill = variable),position ='stack', stat = "identity", size = 0.4, width = 1) +
#   # geom_text(aes(x=date, y=salarios*100,label=salarios * 100), position=position_dodge(width=0.9), vjust=-0.25, size = 4) +
#   scale_x_continuous(breaks=seq(1999,2018,1),                       
#                      labels=paste(c(""),rep(1999:2018,each=1)),expand = c(0, 0)) +
#   # scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 8),expand = c(0, 0)) +
#   labs(title = 'Balanço de reajustes salariais acima, igual ou abaixo do INPC no ano') +
#   scale_fill_manual(values = c('I (99-03)' = "#EFF3FF",
#                                'II (04-10)' = "#BDD7E7",
#                                'III (11-14)' = "#6BAED6",
#                                'IV (15-16)' = "#3182BD", 
#                                'V (17-18)' = "#08519C",
#                                "Abaixo" = "#CB181D",
#                                "Igual" = "#FB6A4A",
#                                "Acima" = "#FCAE91")                    , 
#   # scale_fill_manual(values = c(
#   #   "#FEE0D2", # Abaixo do INPC
#   #   '#EFF3FF', # I
#   #   "#FC9272", # Acima do INPC
#   #   "#FEE5D9", # III
#   #   "#3182BD", # IV
#   #   "#FCAE91", # Monitorados
#   #   "#DE2D26", # Nontradables
#   #   "#CB181D" # Tradables
#   # ),
#   breaks = c(
#     "Acima" ,
#     "Igual" ,
#     "Abaixo")
#   ) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 8),expand = c(0, 0)) +  ylab('') +
#   xlab('') +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="right",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = ggtext::element_markdown(size = 15),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=14)) 
# 
# reajustesplot
# 
# ipca <- ts((ipcainpc[2:nrow(ipcainpc),2]),  start = c(1999,1), end = c(2019,12), frequency = 12)
# inpc <- ts((ipcainpc[2:nrow(ipcainpc),3]),  start = c(1999,1), end = c(2019,12), frequency = 12)
# 
# 
# date <- seq(1999,2019.99,(1/12))
# 
# df <- tibble(date,ipca,inpc, .name_repair = ~ c('date','IPCA','INPC'))
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# ipcainpcplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_line(aes(x=date, y=INPC*100, color = 'INPC', linetype = 'INPC'),size = 1.25) +
#   geom_line(aes(x=date, y=IPCA*100, color = 'IPCA',  linetype = 'IPCA'),stat = 'identity',size = 1.25) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Taxa de variação do IPCA e INPC acumulada em 12 meses', size = 8) +
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   scale_color_manual(name = '',
#                      breaks = c('INPC','IPCA'),
#                      values=c('INPC' = 'indianred', 'IPCA'="black")) +
#   scale_linetype_manual(name='',values = c(1,2)) +
#   ylab('') +
#   xlab('') +
#   guides(colour = guide_legend(override.aes = list(size = c(1, 0.5)))) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="right",
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           plot.title = ggtext::element_markdown(size = 15),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=14)) 
# 
# 
# ipcainpcplot
# plot_lst[[1]] <- reajustesplot
# plot_lst[[2]] <- ipcainpcplot
# 
# ggdraw() +
#   draw_plot(plot_lst[[1]], x = 0, y = 0.4, height = 0.6, width = 1) +
#   draw_plot(plot_lst[[2]], x = 0, y = 0, height = 0.4, width = 1)
# 
# 
# ggsave('reajustes.png', units = 'cm', device = "png")
# 
# 
# 
# # Gráfico serviços ####
# 
# 
# ipcadecomp
# ipca <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),2]), start = c(1999,1), end = c(2019,12), frequency = 12))
# servicos <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),7]), start = c(1999,1), end = c(2019,12), frequency = 12))
# nontradables <- as.xts(ts((ipcadecomp[2:nrow(ipcadecomp),5]), start = c(1999,1), end = c(2019,12), frequency = 12))
# 
# # Trimestralizando
# ipca <- as.numeric(apply.quarterly(ipca,median))
# servicos <- as.numeric(apply.quarterly(servicos,median))
# nontradables <- as.numeric(apply.quarterly(nontradables,median))
# 
# # Dados
# date <- seq(1999,2019.99,(1/4))
# 
# df1 <- tibble(date,ipca,nontradables,servicos, .name_repair = ~ c('date','IPCA','Nontradables','Servicos'))
# 
# df1
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# servicosplot <-  ggplot(df1)  + 
#   geom_hline(yintercept = 0, colour= 'darkgrey') +
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
#   geom_line(aes(x=date, y=IPCA * 100,color = 'IPCA', linetype = 'IPCA'),stat = 'identity',size = 1,linetype = 'solid') +
#   geom_line(aes(x=date, y=Servicos*100,color = 'Serviços'),stat = 'identity',size = 0.75) +
#   geom_line(aes(x=date, y=Nontradables*100,color = 'Não tradables'),stat = 'identity',size = 0.75) +
# 
#   geom_point(aes(x=date,y=IPCA * 100, color = 'IPCA', shape = 'IPCA' ),stat = 'identity', size = 1.5,show.legend = T) +
#   geom_point(aes(x=date,y=Servicos * 100, color = 'Serviços', shape = 'Serviços'),stat = 'identity', size = 2, show.legend = T) +
#   geom_point(aes(x=date,y=Nontradables * 100, color = 'Não tradables', shape = 'Não tradables'),stat = 'identity', size = 2, show.legend = T) +
# 
#   scale_fill_brewer(palette="Blues", name = 'Períodos') +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("1ºT"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   labs(title = 'Variação acumulada em 12 meses de desagregações do IPCA e índice cheio - 1ºT 1999 a 4ºT 2019') +
#   
#   scale_color_manual(breaks = c('IPCA','Serviços','Não tradables'),
#                      values=c('IPCA'="black",'Serviços'= "red1", 'Não tradables' = 'blue')) +
#   scale_shape_manual(values = c(4,19,15)) +
#   ylab('') +
#   xlab('') +
#   guides(name = '',
#          fill = F, 
#          shape = F,
#          color = guide_legend(override.aes = list(shape = c(4,15,19),
#                                                   colour = c('black', 'red1','blue')))) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size=9),
#           legend.key = element_rect(colour = "black"),
#           legend.box.background = element_rect(colour = "black", size = 1),
#           # plot.title = ggtext::element_markdown(size = 12),
#           plot.title = element_blank(),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
#           axis.text.y = element_text(size=10)) 
# servicosplot
# 
# plot(nontradables)
# plot(servicos)  
# 
# # Cambio, comm e alimbebs ####
# 
# comm <- read.csv("Dados/comm.csv")
# ipcadecomp
# 
# # Objetos ts
# comm <- ts((comm[1:nrow(comm),2]),  start = c(1999,1), end = c(2019,12), frequency = 12)
# 
# cambio <- ts((cambiocompleto[,2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# ipca <- ts((ipcadecomp[2:nrow(ipcadecomp),2]), start = c(1999,1), end = c(2019,12), frequency = 12)
# alimbebs <- ts((ipcadecomp[2:nrow(ipcadecomp),3]), start = c(1999,1), end = c(2019,12), frequency = 12)
# 
# (cambio[60]-cambio[48])/cambio[48]
# 
# ipcatradables
# plot(ipcatradables)
# date <- seq(1999,2019.99,(1/12))
# date
# df <- tibble(date,comm,cambio, ipca,alimbebs, .name_repair = ~ c('date','comm','cambio','IPCA','Alimentosbebidas'))
# 
# df
# 
# plot_lst <- vector("list", length = 3)
# max(df$comm)
# 
# max(df$cambio)
# 
# plot(comm)
# 
# rects <- data.frame(xstart = c(1999,2004,2011,2015,2017), xend = c(2004,2011,2015,2017,2019.917), col = c('I (99-03)','II (04-10)','III (11-14)','IV (15-16)','V (17-19)'))
# 
# ALIMBEBSplot <- ggplot(df)  + 
#   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4) +
#   geom_line(aes(x=date, y=Alimentosbebidas*100, color = "Alimentos e Bebidas", linetype = "Alimentos e Bebidas"), size = 1) +
#   geom_line(aes(x=date, y=IPCA*100, color = "IPCA", linetype = "IPCA"), size = 0.75) +
#   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
#   scale_x_continuous(breaks=seq(1999,2019,1),                       
#                      labels=paste(c("Jan"),rep(1999:2019,each=1)),expand = c(0, 0)) +
#   scale_color_manual( name="",
#                       breaks=c("Alimentos e Bebidas","IPCA"),
#                       values=c("Alimentos e Bebidas" = 'indianred1',"IPCA" = 'black')) +
#   scale_linetype_manual(breaks = c("Alimentos e Bebidas","IPCA"),
#                      values=c("Alimentos e Bebidas" = 'solid',"IPCA" = 'dashed')) +
#   ylab('') +
#   xlab('') +
#   guides(name = '',
#          fill = F, 
#          color = F, 
#          linetype = guide_legend(override.aes = list(size = 0.65,
#                                                      linetype = c(1,2),
#                                                      colour = c('indianred1','black')))) +
#   labs(title = '') +
#   scale_fill_brewer(palette="Blues") +
#   ylab('') +
#   xlab('') +
#   guides(fill = F) +
#   theme_classic() +
#   theme(  panel.grid = element_blank(), 
#           panel.border = element_blank(),
#           legend.position="bottom",
#           plot.title = element_blank(),
#           legend.title = element_blank(),
#           legend.text = element_text(size=10),
#                     legend.key = element_rect(colour = "black"),
# legend.box.background = element_rect(colour = "black", size = 1),
#           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=9, colour = 'black'),
#           axis.text.y = element_text(size=12),
#           axis.title.y.right = element_text(size=8)) 
# ALIMBEBSplot
# 
# alugueis <- read_excel('Dados/alugueis.xlsx', col_names = c('Data','Variação mensal','variação acumulada em 12 meses'), skip = 1)
# tail(alugueis,15)
# alugueis <- ts(alugueis[1:255,2], start = c(1999,12), end = c(2021,2), frequency = 12)
# monthplot(alugueis, type ='h')
# DF <- data.frame(alugueis)
# DF
# DF$month <- factor(strftime(DF$Time,"%b"),levels=month.abb)
# boxplot(Data~month,DF)
# boxplot
# ?monthplot
# outlier(alugueis)
# 
# # Antigos ####
# 
# # Grafico dfmelted #
# # dfmelted <- tibble(date,ipca,dura,semi,ndura, .name_repair = ~ c('date','IPCA','Duráveis','Semiduráveis', 'Não duráveis'))
# # 
# # dfmelted <- reshape2::melt(dfmelted,id.vars= c('date'), measure.vars = 2:5)
# # dfmelted
# # 
# # dfmelted <- tibble(dfmelted)
# # dfmelted['variable']
# # 
# # ggplot(dfmelted)  +
# #   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
# #   geom_hline(yintercept = 0, color= 'darkgrey') +
# #   geom_line(aes(x=date,y=value, color = variable, linetype=variable),size = 1) +
# #   geom_point(aes(x=date,y=value, color = variable, shape = variable),stat = 'identity', size = 2.5,show.legend = F) +
# #   scale_fill_brewer(palette="Blues", name = 'Períodos') +
# #   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
# #   scale_x_continuous(breaks=seq(1999,2019,1),
# #                      labels=paste(c("1ºT"),rep(1999:2019,each=1)),expand = c(0, 0)) +
# #   labs(title = 'Taxas de variação acumuladas em 12 meses do IPCA de bens monitorados e do índice cheio') +
# #   scale_linetype_manual(values = c(2,1,1,1)) +
# #   scale_color_manual( values=c('IPCA'="black", 'Duráveis'= "red1", 'Semiduráveis' = "blue", 'Não duráveis' = "orange")) +
# #   scale_shape_manual(values = c(4,16,17,15),
# #                      guide = guide_legend(override.aes = list(shape = c(4,16,17,15),
# #                                                               color = c('black','red1', 'orange', 'blue')))) +
# #   ylab('') +
# #   xlab('') +
# #   # guides(name = '',
# #   #        shape = guide_legend(override.aes = list(shape = c(4,16,17,15),
# #   #                                                 colour = c('black','red1', 'orange', 'blue')))) +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(),
# #           panel.border = element_blank(),
# #           legend.position="bottom",
# #           legend.title = element_blank(),
# #           legend.text = element_text(size=10),
# #           legend.key = element_rect(colour = "black"),
# #           legend.box.background = element_rect(colour = "black", size = 1),
# #           plot.title = ggtext::element_markdown(size = 12),
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10))
# 
# # # IPCA
# # ipcacompleto <- read.csv("Dados/ipcacompleto.csv")
# # 
# # # Cambio 
# # cambiocompleto <- read.csv("Dados/cambiocompleto.csv")
# # 
# # # PIB
# # pib <- read_excel('Dados/pibtrimreal.xlsx', col_names = c('data','Taxa12meses','Taxatrimestral'))
# # pib.trim <- ts(pib[16:nrow(pib),3], start = c(2000,1), end = c(2019,4), frequency = 4)
# # 
# # ipca <- ts((ipcacompleto[,2]/100), start = c(1999,1), end = c(2020,02), frequency = 12)
# # cambio <- ts((cambiocompleto[,2]), start = c(1999,1), end = c(2020,02), frequency = 12)
# # 
# # plot_lst <- vector("list", length = 3)
# # options(digits = 2)
# # 
# # date = seq(from = as.Date("1999-01-01"), to = as.Date("2020-02-01"), by = 'month')
# # 
# # df<- tibble(date,cambio,ipca, .name_repair = ~ c('date','cambio','ipca'))
# # 
# # cambioplot <- ggplot(df)  + 
# #   geom_line(aes(x=date, y=cambio),stat="identity") +
# #   scale_x_continuous(breaks=seq(2000.25,2020,1),                       
# #                      labels=paste(c("Jan"),rep(2000:2019,each=1))) +
# #   labs(title = 'Taxa de Câmbio Nominal') +
# #   ylab('') +
# #   xlab('') +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(), 
# #           panel.border = element_blank(),
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10)) +
# #   coord_cartesian(ylim=c(1,4.5))
# # 
# # IPCAplot <- ggplot(df)  + 
# #   geom_hline(yintercept = 0, colour= 'darkgrey') +
# #   geom_line(aes(x=date, y=IPCA*100),stat="identity") +
# #   # scale_y_continuous(~./max(df$IPCA),expand = c(0, 0)) +
# #   scale_y_continuous(labels = function(x) paste0(x, "%")) +
# #   scale_x_continuous(breaks=seq(2000.25,2020,1),                       
# #                      labels=paste(c("Jan"),rep(2000:2019,each=1))) +
# #   labs(title = 'IPCA') +
# #   ylab('') +
# #   xlab('') +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(), 
# #           panel.border = element_blank(),
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10)) +
# #   coord_cartesian(ylim=c(-0.1,2))
# # 
# # PIBplot <- ggplot(dadosgraficos)  + 
# #   geom_hline(yintercept = 0, colour= 'darkgrey') +
# #   geom_line(aes(x=date, y=PIB),stat="identity") +
# #   scale_x_continuous(breaks=seq(2000.25,2020,1),                       
# #                      labels=paste(c("1ºT"),rep(2000:2019,each=1))) +
# #   scale_y_continuous(labels = function(x) paste0(x, "%")) +
# #   labs(title = 'PIB') +
# #   ylab('') +
# #   xlab('') +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(), 
# #           panel.border = element_blank(),
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10))
# # 
# # plot_lst[[1]] <- cambioplot
# # plot_lst[[2]] <- PIBplot
# # plot_lst[[3]] <- IPCAplot
# 
# # 
# # # IPCA
# # ipcaanualcompleto <- read.csv("Dados/ipcaanual.csv")
# # 
# # # Cambio
# # cambioanualcompleto <- read.csv("Dados/cambioanual.csv")
# # 
# # # PIB
# # pibanualcompleto <- read.csv('Dados/pibanual.csv')
# # 
# # # Transformando em objetos xts
# # ipca <- as.xts(ts((ipcaanualcompleto[2:nrow(ipcaanualcompleto),2]), start = c(1999,1), end = c(2019,1), frequency = 1))
# # cambio <- as.xts(ts((cambioanualcompleto[,2]), start = c(1999,1), end = c(2019,1), frequency = 1))
# # pib <- as.xts(ts(pibanualcompleto[1:nrow(pibanualcompleto),2], start = c(1999,1), end = c(2019,1), frequency = 1))
# # 
# # ipca
# # cambio[1] <- 1.842
# # cambio
# # pib
# # 
# # # Criando base de dados
# # date <- seq(1999,2019,1)
# # date
# # 
# # df <- tibble(date,cambio,ipca,pib, .name_repair = ~ c('date','cambio','IPCA','PIB'))
# # 
# # df
# # 
# # plot_lst <- vector("list", length = 3)
# # options(digits = 2)
# # 
# # rects <- data.frame(xstart = c(1999,2003,2011,2015,2017), xend = c(2003,2011,2015,2017,2019), col = c('I (99-02)','II (03-10)','III (11-14)','IV (15-16)','V (17-19)'))
# # 
# # cambioplot <- ggplot(df)  + 
# #   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.6) +
# #   geom_line(aes(x=date, y=cambio)) +
# #   scale_x_continuous(breaks=seq(1999,2019,1),                       
# #                      labels=paste(c("1T"),rep(1999:2019,each=1)),expand = c(0, 0)) +
# #   labs(title = 'Taxa de Câmbio Nominal') +
# #   scale_fill_brewer(palette="Blues") +
# #   ylab('') +
# #   xlab('') +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(), 
# #           panel.border = element_blank(),
# #           legend.position="none",
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10)) 
# # 
# # PIBplot <- ggplot(df)  + 
# #   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.6) +
# #   geom_hline(yintercept = 0, colour= 'darkgrey') +
# #   geom_line(aes(x=date, y=PIB)) +
# #   scale_fill_brewer(palette="Blues") +
# #   scale_x_continuous(breaks=seq(1999,2019,1),                       
# #                      labels=paste(c("1ºT"),rep(1999:2019,each=1)),expand = c(0, 0)) +
# #   scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = scales::pretty_breaks(n = 8)) +
# #   labs(title = 'PIB') +
# #   ylab('') +
# #   xlab('') +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(),
# #           panel.border = element_blank(),
# #           legend.position="none",
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10))
# # 
# # IPCAplot <- ggplot(df)  + 
# #   geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.6) +
# #   geom_hline(yintercept = 0, colour= 'darkgrey') +
# #   geom_line(aes(x=date, y=IPCA*100)) +
# #   scale_y_continuous(labels = function(x) paste0(x, "%"),breaks = scales::pretty_breaks(n = 6)) +
# #   scale_x_continuous(breaks=seq(1999,2019,1),                       
# #                      labels=paste(c("1T"),rep(1999:2019,each=1)),expand = c(0, 0)) +
# #   labs(title = 'IPCA') +
# #   scale_fill_brewer(palette="Blues", name = 'Períodos') +
# #   ylab('') +
# #   xlab('') +
# #   theme_classic() +
# #   theme(  panel.grid = element_blank(), 
# #           panel.border = element_blank(),
# #           legend.position="bottom",
# #           legend.title = element_text(size=11),
# #           legend.text = element_text(size=10),
# #           legend.key = element_rect(colour = "black"),
# #           legend.box.background = element_rect(colour = "black", size = 1),
# #           axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.6,size=10, colour = 'black'),
# #           axis.text.y = element_text(size=10)) 
# # 
# # 
# # 
# # plot_lst[[1]] <- cambioplot
# # plot_lst[[2]] <- PIBplot
# # plot_lst[[3]] <- IPCAplot
# # 
# # ggdraw() +
# #   draw_plot(plot_lst[[1]], x = 0, y = 0.70, height = .30, width = 1) +
# #   draw_plot(plot_lst[[2]], x = 0, y = 0.40, height = .30, width = 1) +
# #   draw_plot(plot_lst[[3]], x = 0, y = 0, height = .40, width = 1)
# # 
# # ggsave('GraficosDadosFatosEstilizados.png',device = "png")
# 
