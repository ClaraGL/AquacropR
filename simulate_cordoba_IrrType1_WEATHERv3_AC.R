

library(XML)
library(xml2)
library(pracma)
library(kulife)
library(ggplot2)
library(reshape)
library(data.table)
library(dplyr)
library(elliptic)
library(doParallel)

#library(AquaCropR)

 t <- proc.time()


lapply(paste('../aquacropR/R/', list.files('../aquacropr/R', pattern='*.R'),
             sep=''), function(x) source(x))

# results<- matrix(NA, 2401, 6)


#break
SMTTable <- read.csv('SMT.csv', header= TRUE)
data_table <- read.csv('input_wheat_cordoba/table_run.csv', header= TRUE)
folder_name <- dir(pattern='input_wheat_cordoba')
FileLocation = ReadFileLocations(paste(folder_name,'/', 'filesetup.xml',
sep=''))



#' NOTE: Indent blocks when using loops, it makes code easier to understand.

#resultados <- c()
for (iii in 2:2){#nrow(data_table)){
      FileLocation[[3]]=data_table[iii,1]
      FileLocation[[5]]=data_table[iii,3]
      model<-data_table[iii,5]
      rcp <-data_table[iii,6]

      InitialiseStruct <- Initialise(FileLocation)

   # Seasons <-as.numeric(InitialiseStruct$ClockStruct$nSeasons)
 # for (i in 1:Seasons){

      # para leer los distintos weather

      resultados <- c()

        # InitialiseStruct$WeatherStruct=WeatherStruct
      #para leer tabla SMT
      for(ii in 1:nrow(SMTTable)){
        print(ii)

        InitialiseStruct$IrrigationManagement$Wheat$SMT = as.numeric(SMTTable[ii, ])
        InitialiseStruct$IrrigationManagement$Wheat$SMT1=SMTTable$SMT1[ii]
        InitialiseStruct$IrrigationManagement$Wheat$SMT2=SMTTable$SMT2[ii]
        InitialiseStruct$IrrigationManagement$Wheat$SMT3=SMTTable$SMT3[ii]
        InitialiseStruct$IrrigationManagement$Wheat$SMT4=SMTTable$SMT4[ii]

        Outputs <- PerformSimulation(InitialiseStruct)

        Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
        Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
        Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
        Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
        Outputs_month <- split(Outputs, by = 'PlantingDate')
        Outputs_season<-Outputs_month#[[i]]



        #' Plot stuff
        # par(mfrow = c(1,2))
        # plot(Outputs$Bio, main= 'Cum Biomass', col='green', xlab='days')
        # plot(Outputs$Irr, main='Irrigation',  col='blue',  xlab='days')
        #

        # Yield <- tail(Outputs, 1)
        Yield_season <- tail(Outputs_season, 1)
        #year<-as.numeric(Yield_season[1,1])


        #' NOTE: avoid long lines of code, split them like that
        x <- data.frame(t(sapply(split(Outputs, Outputs$Season), function(x)
          tail(x,1))))

        resultados <- rbind(resultados, cbind(year = as.numeric(x$Year),
                                               Weather = iii,'SMT1'=SMTTable[ii,1],
                                              'SMT2'=SMTTable[ii,2],
                                              'SMT3'=SMTTable[ii,3],'SMT4'=SMTTable[ii,4],
                                              Yield = as.numeric(x$Yield),
                                              IrrTot=as.numeric(x$IrrTot)))



      }

      resultados <- data.frame(resultados)
      y = split(data.frame(resultados), resultados$year)
      sapply(names(y), function(x) write.csv(y[[x]], paste('r', iii, '_', x,model, rcp,'.csv', sep=''),row.names=F))
            # PROBANDO GRABAR OUTPUS DE CADA WEATHER
       # resultados<-rbind(resultados, cbind('Weather',(iii)))
      # nombre_archivo<-paste('resultados_weather_',(iii))
 # }

}
proc.time()-t


#'resutlados <-data.frame(resultados)
# print(resultados)
# debugonce(Solution)
#break
# Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
# Outputs <- subset(Outputs, PlantingDate != '0000-01-01')
# Outputs$PlantingDate <- as.factor(Outputs$PlantingDate)
# Outputs <- setDT(mutate(Outputs, DOY = convertDOY(Outputs$PlantingDate)))
# Outputs_month <- split(Outputs, by = 'PlantingDate')



#' Plot stuff
# par(mfrow = c(1,2))
# plot(Outputs$Bio, main= 'Cum Biomass', col='green', xlab='days')
# plot(Outputs$Irr, main='Irrigation',  col='blue',  xlab='days')
