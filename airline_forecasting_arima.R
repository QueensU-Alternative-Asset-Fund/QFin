####THIS PROJECT SEEKS TO PREDICT THE FUTURE SHARE PRICE PERFORMANCE OF THE 10 MAJOR NORTH AMERICAN AIRLINE STOCKS USING A SOPHISTICATED SEASONAL ARIMAX MODEL

#### PACKAGE IMPORTS ####
library(tidyverse) 
library(quantmod)
library(jsonlite)
library(httr)
library(readr)
library(data.table)
library(abind)
library(RSQLite)
library(gsubfn)
library(proto)
library(sqldf)
library(Stack)
library(ggplot2)
library(dplyr)
library(timeDate)
library(timeSeries)
library(fUnitRoots)
library(fBasics)
library(urca)
library(forecast)
library(bestglm)
library(FitAR)
library(lmtest)
library(zoo)
library(Quandl)
library(eia)
library(car)
library(corrplot)
library(RColorBrewer)
library(xlsx)

#### DATA DICTIONARY ####

# Data Sources:
## Yahoo Finance - source for open, high, low and closing values for stocks and benchmarks
## U.S. Energy Information Administration (EIA) - source for US energy and oil data
## Federal Reserve Bank of St. Louis (FRED) - source for US and global macroeconomic data

# Data Frequencies:
## Daily
## Monthly

# Data Formats:
## Dataframe (df) - all variables anotated with "_DF" are in dataframe format
## Extensible Time Series (xts) - variables without the "_DF" anotation are in xts format

# Variable Explanations:
# 1. Benchmarks
## SP500_GSPC_Daily - Daily price and volume values for S&P 500 (^GSPC)
## DowJones_DJI_Daily - Daily price and volume valies for Dow Jones Industrial Average (^DJI)
## TSXComposite_GSPTSE_Daily - Daily price and volume values for SP/TSX Composite Index (^GSPTSE)
## CrudeOil_CLF_Daily - Daily price and volume values for crude oil (CL=F)
## Volatility_VIX_Daily - Daily price and volume values for volatility index (^VIX)
## Treasury10yrBond_TNX_Daily - Daily price and volume values for 10yr treasury bonds (^TNX)
## SP500_GSPC_Monthly - Monthly price and volume values for S&P 500 (^GSPC)
## DowJones_DJI_Monthly - Monthly price and volume valies for Dow Jones Industrial Average (^DJI)
## TSXComposite_GSPTSE_Monthly - Monthly price and volume values for SP/TSX Composite Index (^GSPTSE)
## CrudeOil_CLF_Monthly - Monthly price and volume values for crude oil (CL=F)
## Volatility_VIX_Monthly - Monthly price and volume values for volatility index (^VIX)
## Treasury10yrBond_TNX_Monthly - Monthly price and volume values for 10yr treasury bonds (^TNX)
## OilSupply_OPEC_Monthly - Monthly OPEC oil supply from 04/2000 to 04/2020
## OilProduction_OPEC_Monthly - Monthly OPEC oil production from 04/2000 to 04/2020
## OilWeightedGDP_Global_Monthly - Monthly global oil weighted GDP from 04/2000 to 04/2020 (this an EIA specific metric)
## OilConsumption_Global_Monthly - Monthly global oil consumption from 04/2000 to 04/2020
## OilProdCapacity_OPEC_Monthly - Monthly OPEC production capacity from 04/2000 to 04/2020
## OilSpareProdCapacity_OPEC_Monthly - Monthly OPEC spare production capacity from 04/2000 to 04/2020
## Unemployment_US_Monthly - Monthly unemployment rates in the US from 04/2000 to 04/2020
## Nonfarm_US_Monthly - Monthly measure of U.S. workers that are not proprietors, private household employees, volunteers, farmers or unincorporated self-employed
## Claims_US_Monthly - Monthly number of U.S. jobless claims from 04/2000 to 04/2020
## CPI_US_Monthly - Monthly Consumer Price Index (CPI) for the US from 04/2000 to 04/2020
## WTISpotPrice_US_Monthly - Monthly Cushing WTI spot price from 04/2000 to 04/2020
## JetFuelSpotPrice_US_Monthly - Monthly U.S. Gulf Coast Kerosene Jet Fuel Spot Price from 04/2000 to 04/2020

# 2. Airline Stocks
## AlaskaAir_ALK_Daily - Daily price and volume values for Alaska Airlines (ALK) - data available back to 04/2000
## SouthwestAir_LUV_Daily - Daily price and volume values for SouthWest Airlines (LUV) - data available back to 04/2000
## AirCanada_AC.TO_Daily - Daily price and volume values for Air Canada (AC.T)
## AmericanAir_AAL_Daily - Daily price and volume values for American Airlines (AAL)
## CargoJet_CJT.TO_Daily - Daily price and volume values for Cargojet (CJT.TO)
## Chorus_CHR.TO_Daily - Daily price and volume values for Chorus Aviation (CHR.TO)
## Delta_DAL_Daily - Daily price and volume values for Delta Airlines (DAL)
## JetBlue_JBLU_Daily - Daily price and volume values for Jet Blue (JBLU)
## Spirit_SAVE_Daily - Daily price and volume values for Spirit Airlines (SAVE)
## UnitedAir_UAL_Daily - Daily price and volume values for United Airlines (UAL)
## AlaskaAir_ALK_Monthly - Monthly price and volume values for Alaska Airlines (ALK) - data available back to 04/2000
## SouthwestAir_LUV_Monthly - Monthly price and volume values for SouthWest Airlines (LUV) - data available back to 04/2000
## AirCanada_AC.TO_Monthly - Monthly price and volume values for Air Canada (AC.T)
## AmericanAir_AAL_Monthly - Monthly price and volume values for American Airlines (AAL)
## CargoJet_CJT.TO_Monthly - Monthly price and volume values for Cargojet (CJT.TO)
## Chorus_CHR.TO_Monthly - Monthly price and volume values for Chorus Aviation (CHR.TO)
## Delta_DAL_Monthly - Monthly price and volume values for Delta Airlines (DAL)
## JetBlue_JBLU_Monthly - Monthly price and volume values for Jet Blue (JBLU)
## Spirit_SAVE_Monthly - Monthly price and volume values for Spirit Airlines (SAVE)
## UnitedAir_UAL_Monthly - Monthly price and volume values for United Airlines (UAL)

# 3. Composite Variables
## AirlineAverage_Daily - Average daily airline closing prices across 10 airlines from 04/2000 to 04/2020
## AirlineAverage_Monthly - Average monthly airline closing prices across 10 airlines from 04/2000 to 04/2020

#### DAILY DATA ####

# These functions call in daily benchmark and stock data from Yahoo Finance

SP500_GSPC_Daily <- na.omit(getSymbols(Symbols = "^GSPC",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                       auto.assign = FALSE))
DowJones_DJI_Daily <- na.omit(getSymbols(Symbols = "^DJI",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                         auto.assign = FALSE))
TSXComposite_GSPTSE_Daily <- na.omit(getSymbols(Symbols = "^GSPTSE",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                                auto.assign = FALSE))
CrudeOil_CLF_Daily <- na.omit(getSymbols(Symbols = "CL=F",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                         auto.assign = FALSE))
Volatility_VIX_Daily <- na.omit(getSymbols(Symbols = "^VIX",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                           auto.assign = FALSE))
Treasury10yrBond_TNX_Daily <- na.omit(getSymbols(Symbols = "^TNX",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                                 auto.assign = FALSE))


AlaskaAir_ALK_Daily <- na.omit(getSymbols(Symbols = "ALK",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                          auto.assign = FALSE))
SouthwestAir_LUV_Daily <- na.omit(getSymbols(Symbols = "LUV",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                             auto.assign = FALSE))
AirCanada_AC.TO_Daily <- na.omit(getSymbols(Symbols = "AC.TO",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                            auto.assign = FALSE))
AmericanAir_AAL_Daily <- na.omit(getSymbols(Symbols = "AAL",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                            auto.assign = FALSE))
CargoJet_CJT.TO_Daily <- na.omit(getSymbols(Symbols = "CJT.TO",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                            auto.assign = FALSE))
Chorus_CHR.TO_Daily <- na.omit(getSymbols(Symbols = "CHR.TO",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                          auto.assign = FALSE))
Delta_DAL_Daily <- na.omit(getSymbols(Symbols = "DAL",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                      auto.assign = FALSE))
JetBlue_JBLU_Daily <- na.omit(getSymbols(Symbols = "JBLU",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                         auto.assign = FALSE))
Spirit_SAVE_Daily <- na.omit(getSymbols(Symbols = "SAVE",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                        auto.assign = FALSE))
UnitedAir_UAL_Daily <- na.omit(getSymbols(Symbols = "UAL",src = "yahoo", from="2000-04-01", to="2020-04-01", 
                                          auto.assign = FALSE))

#### MONTHLY DATA ####

# These functions convert daily stock and benchmark data from Yahoo Finance to monthly

SP500_GSPC_Monthly <- to.monthly(SP500_GSPC_Daily)
DowJones_DJI_Monthly <- to.monthly(DowJones_DJI_Daily)
TSXComposite_GSPTSE_Monthly <- to.monthly(TSXComposite_GSPTSE_Daily)
CrudeOil_CLF_Monthly <- to.monthly(CrudeOil_CLF_Daily)
Volatility_VIX_Monthly <- to.monthly(Volatility_VIX_Daily)
Treasury10yrBond_TNX_Monthly <- to.monthly(Treasury10yrBond_TNX_Daily)

AlaskaAir_ALK_Mothnly <- to.monthly(AlaskaAir_ALK_Daily)
SouthwestAir_LUV_Monthly <- to.monthly(SouthwestAir_LUV_Daily)
AirCanada_AC.TO_Monthly <- to.monthly(AirCanada_AC.TO_Daily)
AmericanAir_AAL_Monthly <- to.monthly(AmericanAir_AAL_Daily)
CargoJet_CJT.TO_Monthly <- to.monthly(CargoJet_CJT.TO_Daily)
Chorus_CHR.TO_Monthly <- to.monthly(Chorus_CHR.TO_Daily)
Delta_DAL_Monthly <- to.monthly(Delta_DAL_Daily)
JetBlue_JBLU_Monthly <- to.monthly(JetBlue_JBLU_Daily)
Spirit_SAVE_Monthly <- to.monthly(Spirit_SAVE_Daily)
UnitedAir_UAL_Monthly <- to.monthly(UnitedAir_UAL_Daily)

# Function uses an API wrapper package (eia) to call in data from the US Energy Information Administration (EIA). Contains monthly data for oil production.

eia_set_key("66bb45c82f750c61ca859c646267ead7")

eiadata1 <- eia_series("STEO.PAPR_OPEC.M", start = 200004, end = 202003)
OilSupply_OPEC_Monthly_DF <- as.data.frame(eiadata1$data)
OilSupply_OPEC_Monthly <- xts(OilSupply_OPEC_Monthly_DF, order.by = OilSupply_OPEC_Monthly_DF$date)

eiadata2 <- eia_series("STEO.COPR_OPEC.M", start = 200004, end = 202003)
OilProduction_OPEC_Monthly_DF <- as.data.frame(eiadata2$data)
OilProduction_OPEC_Monthly <- xts(OilProduction_OPEC_Monthly_DF, order.by = OilProduction_OPEC_Monthly_DF$date)

eiadata3 <- eia_series("STEO.RGDPQ_WORLD.M", start = 200004, end = 202003)
OilWeightedGDP_Global_Monthly_DF <- as.data.frame(eiadata3$data)
OilWeightedGDP_Global_Monthly <- xts(OilWeightedGDP_Global_Monthly_DF, order.by = OilWeightedGDP_Global_Monthly_DF$date)

eiadata4 <- eia_series("STEO.PATC_WORLD.M", start = 200004, end = 202003)
OilConsumption_Global_Monthly_DF <- as.data.frame(eiadata4$data)
OilConsumption_Global_Monthly <- xts(OilConsumption_Global_Monthly_DF, order.by = OilConsumption_Global_Monthly_DF$date)

eiadata5 <- eia_series("STEO.COPC_OPEC.M", start = 200004, end = 202003)
OilProdCapacity_OPEC_Monthly_DF <- as.data.frame(eiadata5$data)
OilProdCapacity_OPEC_Monthly <- xts(OilProdCapacity_OPEC_Monthly_DF, order.by = OilProdCapacity_OPEC_Monthly_DF$date)

eiadata6 <- eia_series("STEO.COPS_OPEC.M", start = 200004, end = 202003)
OilSpareProdCapacity_OPEC_Monthly_DF <- as.data.frame(eiadata6$data)
OilSpareProdCapacity_OPEC_Monthly <- xts(OilSpareProdCapacity_OPEC_Monthly_DF, order.by = OilSpareProdCapacity_OPEC_Monthly_DF$date)

eiadata7 <- eia_series("PET.RWTC.M", start = 200004, end = 202003)
WTISpotPrice_US_Monthly_DF <- as.data.frame(eiadata7$data)
WTISpotPrice_US_Monthly <- xts(WTISpotPrice_US_Monthly_DF, order.by = WTISpotPrice_US_Monthly_DF$date)

eiadata8 <- eia_series("PET.EER_EPJK_PF4_RGC_DPG.M", start = 200004, end = 202003)
JetFuelSpotPrice_US_Monthly_DF <- as.data.frame(eiadata8$data)
JetFuelSpotPrice_US_Monthly <- xts(JetFuelSpotPrice_US_Monthly_DF, order.by = JetFuelSpotPrice_US_Monthly_DF$date)

# Function pulls in US macroeconomic data from FRED using Quandl

Quandl.api_key("_tCb7ffjZu6xX-2mkGcp")

Unemployment_US_Monthly <- Quandl("FRED/UNRATE", start_date = "2000-04-01", end_date = "2020-04-01", type="xts")
Unemployment_US_Monthly_DF <- Quandl("FRED/UNRATE", start_date = "2000-04-01", end_date = "2020-04-01")

Nonfarm_US_Monthly <- Quandl("FRED/PAYEMS", start_date = "2000-04-01", end_date = "2020-04-01", type="xts")
Nonfarm_US_Monthly_DF <- Quandl("FRED/PAYEMS", start_date = "2000-04-01", end_date = "2020-04-01")

Claims_US_Monthly <- Quandl("FRED/ICSA", start_date = "2000-04-01", end_date = "2020-04-01", type="xts", collapse = "monthly")
Claims_US_Monthly_DF <- Quandl("FRED/ICSA", start_date = "2000-04-01", end_date = "2020-04-01", collapse = "monthly")

CPI_US_Monthly <- Quandl("FRED/CPIAUCSL", start_date = "2000-04-01", end_date = "2020-04-01", type="xts", collapse = "monthly")
CPI_US_Monthly_DF <- Quandl("FRED/CPIAUCSL", start_date = "2000-04-01", end_date = "2020-04-01", collapse = "monthly")

#### DATAFRAME AND COLUMN NAMING #####

# These functions convert stock xts variables into dataframes for easier manipulation

AirCanada_AC.TO_Daily_DF <- data.frame(Date=index(AirCanada_AC.TO_Daily), coredata(AirCanada_AC.TO_Daily))
AirCanada_AC.TO_Monthly_DF <- data.frame(Date=index(AirCanada_AC.TO_Monthly), coredata(AirCanada_AC.TO_Monthly))

AlaskaAir_ALK_Daily_DF <- data.frame(Date=index(AlaskaAir_ALK_Daily), coredata(AlaskaAir_ALK_Daily))
AlaskaAir_ALK_Mothnly_DF <- data.frame(Date=index(AlaskaAir_ALK_Mothnly), coredata(AlaskaAir_ALK_Mothnly))

AmericanAir_AAL_Daily_DF <- data.frame(Date=index(AmericanAir_AAL_Daily), coredata(AmericanAir_AAL_Daily))
AmericanAir_AAL_Monthly_DF <- data.frame(Date=index(AmericanAir_AAL_Monthly), coredata(AmericanAir_AAL_Monthly))

CargoJet_CJT.TO_Daily_DF <- data.frame(Date=index(CargoJet_CJT.TO_Daily), coredata(CargoJet_CJT.TO_Daily))
CargoJet_CJT.TO_Monthly_DF <- data.frame(Date=index(CargoJet_CJT.TO_Monthly), coredata(CargoJet_CJT.TO_Monthly))

Chorus_CHR.TO_Daily_DF <- data.frame(Date=index(Chorus_CHR.TO_Daily), coredata(Chorus_CHR.TO_Daily))
Chorus_CHR.TO_Monthly_DF <- data.frame(Date=index(Chorus_CHR.TO_Monthly), coredata(Chorus_CHR.TO_Monthly))

Delta_DAL_Daily_DF <- data.frame(Date=index(Delta_DAL_Daily), coredata(Delta_DAL_Daily))
Delta_DAL_Monthly_DF <- data.frame(Date=index(Delta_DAL_Monthly), coredata(Delta_DAL_Monthly))

JetBlue_JBLU_Daily_DF <- data.frame(Date=index(JetBlue_JBLU_Daily), coredata(JetBlue_JBLU_Daily))
JetBlue_JBLU_Monthly_DF <- data.frame(Date=index(JetBlue_JBLU_Monthly), coredata(JetBlue_JBLU_Monthly))

SouthwestAir_LUV_Daily_DF <- data.frame(Date=index(SouthwestAir_LUV_Daily), coredata(SouthwestAir_LUV_Daily))
SouthwestAir_LUV_Monthly_DF <- data.frame(Date=index(SouthwestAir_LUV_Monthly), coredata(SouthwestAir_LUV_Monthly))

Spirit_SAVE_Daily_DF <- data.frame(Date=index(Spirit_SAVE_Daily), coredata(Spirit_SAVE_Daily))
Spirit_SAVE_Monthly_DF <- data.frame(Date=index(Spirit_SAVE_Monthly), coredata(Spirit_SAVE_Monthly))

UnitedAir_UAL_Daily_DF <- data.frame(Date=index(UnitedAir_UAL_Daily), coredata(UnitedAir_UAL_Daily))
UnitedAir_UAL_Monthly_DF <- data.frame(Date=index(UnitedAir_UAL_Monthly), coredata(UnitedAir_UAL_Monthly))

CrudeOil_CLF_Daily_DF <- data.frame(Date=index(CrudeOil_CLF_Daily), coredata(CrudeOil_CLF_Daily))
CrudeOil_CLF_Monthly_DF <- data.frame(Date=index(CrudeOil_CLF_Monthly), coredata(CrudeOil_CLF_Monthly))

DowJones_DJI_Daily_DF <- data.frame(Date=index(DowJones_DJI_Daily), coredata(DowJones_DJI_Daily))
DowJones_DJI_Monthly_DF <- data.frame(Date=index(DowJones_DJI_Monthly), coredata(DowJones_DJI_Monthly))

SP500_GSPC_Daily_DF <- data.frame(Date=index(SP500_GSPC_Daily), coredata(SP500_GSPC_Daily))
SP500_GSPC_Monthly_DF <- data.frame(Date=index(SP500_GSPC_Monthly), coredata(SP500_GSPC_Monthly))

Treasury10yrBond_TNX_Daily_DF <- data.frame(Date=index(Treasury10yrBond_TNX_Daily), coredata(Treasury10yrBond_TNX_Daily))
Treasury10yrBond_TNX_Monthly_DF <- data.frame(Date=index(Treasury10yrBond_TNX_Monthly), coredata(Treasury10yrBond_TNX_Monthly))

TSXComposite_GSPTSE_Daily_DF <- data.frame(Date=index(TSXComposite_GSPTSE_Daily), coredata(TSXComposite_GSPTSE_Daily))
TSXComposite_GSPTSE_Monthly_DF <- data.frame(Date=index(TSXComposite_GSPTSE_Monthly), coredata(TSXComposite_GSPTSE_Monthly))

Volatility_VIX_Daily_DF <- data.frame(Date=index(Volatility_VIX_Daily), coredata(Volatility_VIX_Daily))
Volatility_VIX_Monthly_DF <- data.frame(Date=index(Volatility_VIX_Monthly), coredata(Volatility_VIX_Monthly))

colnames(AirCanada_AC.TO_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(AirCanada_AC.TO_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(AlaskaAir_ALK_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(AlaskaAir_ALK_Mothnly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(AmericanAir_AAL_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(AmericanAir_AAL_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(CargoJet_CJT.TO_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(CargoJet_CJT.TO_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(Chorus_CHR.TO_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(Chorus_CHR.TO_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(Delta_DAL_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(Delta_DAL_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(JetBlue_JBLU_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(JetBlue_JBLU_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(SouthwestAir_LUV_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(SouthwestAir_LUV_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(Spirit_SAVE_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(Spirit_SAVE_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(UnitedAir_UAL_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(UnitedAir_UAL_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

# These functions adjust variable column namens for easier merges and manipulation

colnames(CrudeOil_CLF_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(CrudeOil_CLF_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(DowJones_DJI_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(DowJones_DJI_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(SP500_GSPC_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(SP500_GSPC_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(Treasury10yrBond_TNX_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(Treasury10yrBond_TNX_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(TSXComposite_GSPTSE_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(TSXComposite_GSPTSE_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(Volatility_VIX_Daily_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
colnames(Volatility_VIX_Monthly_DF) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(OilProdCapacity_OPEC_Monthly_DF) <- c("MBarrels", "Date", "Year", "Month")
colnames(OilSupply_OPEC_Monthly_DF) <- c("MBarrels", "Date", "Year", "Month")
colnames(OilProduction_OPEC_Monthly_DF) <- c("MBarrels", "Date", "Year", "Month")
colnames(OilWeightedGDP_Global_Monthly_DF) <- c("Units", "Date", "Year", "Month")
colnames(OilConsumption_Global_Monthly_DF) <- c("MBarrels", "Date", "Year", "Month")
colnames(OilSpareProdCapacity_OPEC_Monthly_DF) <- c("MBarrels", "Date", "Year", "Month")
colnames(WTISpotPrice_US_Monthly_DF) <- c("Value", "Date", "Year", "Month")
colnames(JetFuelSpotPrice_US_Monthly_DF) <- c("Value", "Date", "Year", "Month")

####COMPOSITE VARIABLES #####

# Out of the 10 airlines stock, only 2 have data going back to 04-2000
# To effectively capture effects of 9/11 and SARS, we require data going back to 04-2000
# This function will average all airlines stock data to create a unified pricing base
# All individual airline stock data is averaged into one dataframe and xts below

AggregateDailyAirlineData <- list(SouthwestAir_LUV_Daily_DF,
                                  AirCanada_AC.TO_Daily_DF, 
                                  AlaskaAir_ALK_Daily_DF, 
                                  AmericanAir_AAL_Daily_DF,
                                  CargoJet_CJT.TO_Daily_DF,
                                  Chorus_CHR.TO_Daily_DF,
                                  Delta_DAL_Daily_DF,
                                  JetBlue_JBLU_Daily_DF,
                                  Spirit_SAVE_Daily_DF,
                                  UnitedAir_UAL_Daily_DF)

AggregateMonthlyAirlineData <- list(SouthwestAir_LUV_Monthly_DF,
                                    AirCanada_AC.TO_Monthly_DF, 
                                    AlaskaAir_ALK_Mothnly_DF,
                                    AmericanAir_AAL_Monthly_DF,
                                    CargoJet_CJT.TO_Monthly_DF,
                                    Chorus_CHR.TO_Monthly_DF,
                                    Delta_DAL_Monthly_DF,
                                    JetBlue_JBLU_Monthly_DF,
                                    Spirit_SAVE_Monthly_DF,
                                    UnitedAir_UAL_Monthly_DF)

AirlineAverage_Daily_DF <- as.data.frame(rbindlist(AggregateDailyAirlineData)[, lapply(.SD, mean), list(Date)])
AirlineAverage_Daily <- xts(AirlineAverage_Daily_DF, order.by = as.Date(AirlineAverage_Daily_DF$Date))

AirlineAverage_Monthly_DF <- as.data.frame(rbindlist(AggregateMonthlyAirlineData)[, lapply(.SD, mean), list(Date)])
AirlineAverage_Monthly <- xts(AirlineAverage_Monthly_DF, order.by = as.Date(AirlineAverage_Monthly_DF$Date))

#### DATA MERGING FOR LINEAR REGRESSIONS ####

##1. Convert dfs that still had a specific date to month/year format
##convert date to year and month
CPI_US_Monthly_DF$Date<-as.yearmon(as.Date(CPI_US_Monthly_DF$Date, "%Y-%m-%d"))
WTISpotPrice_US_Monthly_DF$Date<-as.yearmon(as.Date(WTISpotPrice_US_Monthly_DF$Date, "%Y-%m-%d"))
Unemployment_US_Monthly_DF$Date<-as.yearmon(as.Date(Unemployment_US_Monthly_DF$Date, "%Y-%m-%d"))
OilWeightedGDP_Global_Monthly_DF$Date<-as.yearmon(as.Date(OilWeightedGDP_Global_Monthly_DF$Date, "%Y-%m-%d"))
OilSupply_OPEC_Monthly_DF$Date<-as.yearmon(as.Date(OilSupply_OPEC_Monthly_DF$Date, "%Y-%m-%d"))
OilSpareProdCapacity_OPEC_Monthly_DF$Date<-as.yearmon(as.Date(OilSpareProdCapacity_OPEC_Monthly_DF$Date, "%Y-%m-%d"))
OilProduction_OPEC_Monthly_DF$Date<-as.yearmon(as.Date(OilProduction_OPEC_Monthly_DF$Date, "%Y-%m-%d"))
OilProdCapacity_OPEC_Monthly_DF$Date<-as.yearmon(as.Date(OilProdCapacity_OPEC_Monthly_DF$Date, "%Y-%m-%d"))
OilConsumption_Global_Monthly_DF$Date<-as.yearmon(as.Date(OilConsumption_Global_Monthly_DF$Date, "%Y-%m-%d"))
Nonfarm_US_Monthly_DF$Date<-as.yearmon(as.Date(Nonfarm_US_Monthly_DF$Date, "%Y-%m-%d"))
JetFuelSpotPrice_US_Monthly_DF$Date<-as.yearmon(as.Date(JetFuelSpotPrice_US_Monthly_DF$Date, "%Y-%m-%d"))
Claims_US_Monthly_DF$Date<-as.yearmon(as.Date(Claims_US_Monthly_DF$Date, "%Y-%m-%d"))

##2. Remove additional year and month columns for certain dfs
WTISpotPrice_US_Monthly_DF<-WTISpotPrice_US_Monthly_DF[ -c(3:4)]
OilWeightedGDP_Global_Monthly_DF<-OilWeightedGDP_Global_Monthly_DF[ -c(3:4)]
OilSupply_OPEC_Monthly_DF<-OilSupply_OPEC_Monthly_DF[ -c(3:4)]
OilSpareProdCapacity_OPEC_Monthly_DF<-OilSpareProdCapacity_OPEC_Monthly_DF[ -c(3:4)]
OilProduction_OPEC_Monthly_DF<-OilProduction_OPEC_Monthly_DF[ -c(3:4)]
OilProdCapacity_OPEC_Monthly_DF<-OilProdCapacity_OPEC_Monthly_DF[ -c(3:4)]
OilConsumption_Global_Monthly_DF<-OilConsumption_Global_Monthly_DF[ -c(3:4)]
JetFuelSpotPrice_US_Monthly_DF<-JetFuelSpotPrice_US_Monthly_DF[ -c(3:4)]

##Create the data frame of original variables
Set1 <- data.frame(AirlineAverage_Monthly_DF$Date,  
                   AirlineAverage_Monthly_DF$Close,
                   SP500_GSPC_Monthly_DF$Close,
                   DowJones_DJI_Monthly_DF$Close,
                   TSXComposite_GSPTSE_Monthly_DF$Close,
                   CrudeOil_CLF_Monthly_DF$Close,
                   Volatility_VIX_Monthly_DF$Close,
                   Treasury10yrBond_TNX_Monthly_DF$Close)

names(Set1)[1] <-"Date"
names(Set1)[2] <-"Airline"
names(Set1)[3] <-"SP500"
names(Set1)[4] <-"DowJ"
names(Set1)[5] <-"TSX"
names(Set1)[6] <-"CrudeOil"
names(Set1)[7] <-"Volatility"
names(Set1)[8] <-"Treasury10"

##rename other variables
Unemployment_US_Monthly_DF<-rename(Unemployment_US_Monthly_DF, c("Unemployment"="Value"))
OilWeightedGDP_Global_Monthly_DF<-rename(OilWeightedGDP_Global_Monthly_DF, "Oil_GDP" ="Units")
OilSupply_OPEC_Monthly_DF<-rename(OilSupply_OPEC_Monthly_DF, c("Oil_Supply"="MBarrels"))
OilSpareProdCapacity_OPEC_Monthly_DF<-rename(OilProduction_OPEC_Monthly_DF,c("Oil_Spare"="MBarrels")) 
OilProduction_OPEC_Monthly_DF<-rename(OilProduction_OPEC_Monthly_DF, c("Oil_Production"="MBarrels")) 
OilProdCapacity_OPEC_Monthly_DF<-rename(OilProdCapacity_OPEC_Monthly_DF,c("Oil_ProdCapacity"="MBarrels")) 
OilConsumption_Global_Monthly_DF<-rename(OilConsumption_Global_Monthly_DF, c("Oil_Consumption"="MBarrels"))
Nonfarm_US_Monthly_DF<-rename(Nonfarm_US_Monthly_DF,c("Employment"="Value"))
JetFuelSpotPrice_US_Monthly_DF<-rename(JetFuelSpotPrice_US_Monthly_DF, c("JetFuel"="Value")) 
CPI_US_Monthly_DF<-rename(CPI_US_Monthly_DF, c("CPI"="Value"))
Claims_US_Monthly_DF<-rename(Claims_US_Monthly_DF, c("Jobless_Claims"="Value"))
WTISpotPrice_US_Monthly_DF<-rename(WTISpotPrice_US_Monthly_DF, c("WTISpotPrice"="Value"))
SouthwestAir_LUV_Monthly_DF<-rename(SouthwestAir_LUV_Monthly_DF, c("Southwest"="Close"))
AirCanada_AC.TO_Monthly_DF<-rename(AirCanada_AC.TO_Monthly_DF, c("AirCanada"="Close")) 
AlaskaAir_ALK_Mothnly_DF<-rename(AlaskaAir_ALK_Mothnly_DF, c("AlaskaAir"="Close"))
AmericanAir_AAL_Monthly_DF<-rename(AmericanAir_AAL_Monthly_DF, c("AmericanAir"="Close"))
CargoJet_CJT.TO_Monthly_DF<-rename(CargoJet_CJT.TO_Monthly_DF, c("CargoJet"="Close"))
Chorus_CHR.TO_Monthly_DF<-rename(Chorus_CHR.TO_Monthly_DF, c("Chorus"="Close"))
Delta_DAL_Monthly_DF<-rename(Delta_DAL_Monthly_DF, c("Delta"="Close"))
JetBlue_JBLU_Monthly_DF<-rename(JetBlue_JBLU_Monthly_DF, c("JetBlue"="Close"))
Spirit_SAVE_Monthly_DF<-rename(Spirit_SAVE_Monthly_DF, c("SpiritSAVE"="Close"))
UnitedAir_UAL_Monthly_DF<-rename(UnitedAir_UAL_Monthly_DF, c("UnitedAir"="Close"))


##remove additional variables from airline stock dataframes
AirCanada_AC.TO_Monthly_DF<-AirCanada_AC.TO_Monthly_DF[ c(1,5)]
SouthwestAir_LUV_Monthly_DF<-SouthwestAir_LUV_Monthly_DF[ c(1,5)]
AlaskaAir_ALK_Mothnly_DF<-AlaskaAir_ALK_Mothnly_DF[ c(1,5)]
AmericanAir_AAL_Monthly_DF<-AmericanAir_AAL_Monthly_DF[ c(1,5)]
CargoJet_CJT.TO_Monthly_DF<-CargoJet_CJT.TO_Monthly_DF[ c(1,5)]
Chorus_CHR.TO_Monthly_DF<-Chorus_CHR.TO_Monthly_DF[ c(1,5)]
Delta_DAL_Monthly_DF<-Delta_DAL_Monthly_DF[ c(1,5)]
JetBlue_JBLU_Monthly_DF<-JetBlue_JBLU_Monthly_DF[ c(1,5)]
Spirit_SAVE_Monthly_DF<-Spirit_SAVE_Monthly_DF[ c(1,5)]
UnitedAir_UAL_Monthly_DF<-UnitedAir_UAL_Monthly_DF[ c(1,5)]

#create list to merge data
mergelist<-list(Set1,
                Unemployment_US_Monthly_DF,
                OilWeightedGDP_Global_Monthly_DF,
                OilSupply_OPEC_Monthly_DF,
                OilSpareProdCapacity_OPEC_Monthly_DF, 
                OilProduction_OPEC_Monthly_DF, 
                OilProdCapacity_OPEC_Monthly_DF, 
                OilConsumption_Global_Monthly_DF, 
                Nonfarm_US_Monthly_DF, 
                JetFuelSpotPrice_US_Monthly_DF, 
                CPI_US_Monthly_DF,
                Claims_US_Monthly_DF,
                WTISpotPrice_US_Monthly_DF,
                SouthwestAir_LUV_Monthly_DF,
                AirCanada_AC.TO_Monthly_DF, 
                AlaskaAir_ALK_Mothnly_DF,
                AmericanAir_AAL_Monthly_DF,
                CargoJet_CJT.TO_Monthly_DF,
                Chorus_CHR.TO_Monthly_DF,
                Delta_DAL_Monthly_DF,
                JetBlue_JBLU_Monthly_DF,
                Spirit_SAVE_Monthly_DF,
                UnitedAir_UAL_Monthly_DF)

##merge data
merged_data<- Reduce(
  function(x, y, ...) merge(x, y, ..., all=TRUE),
  mergelist)


#### EXPLORATORY VISUALIZATION ####

#various share prices vs. time

##industry composite vs Southwest, Delta, AirCanada & JetBlue (low share prices)
airline_shares_plot_a <- ggplot() +
  geom_line(aes(x=Date,y=Close,group = 1),data = AirlineAverage_Monthly_DF, color = 'black',size = 1.3) +
  geom_line(aes(x=Date,y=Close,group = 1),data = SouthwestAir_LUV_Monthly_DF, color = 'orange', size = 0.6) +
  geom_line(aes(x=Date,y=Close,group = 1),data = Delta_DAL_Monthly_DF, color = 'green', size = 0.6) +
  geom_line(aes(x=Date,y=Close,group = 1),data = AirCanada_AC.TO_Monthly_DF, color = 'red', size = 0.6) +
  geom_line(aes(x=Date,y=Close,group = 1),data = JetBlue_JBLU_Monthly_DF, color = 'blue', size = 0.6) +
  xlab('Date') +
  ylab('Share Price (Monthly Close)') +
  ggtitle('LUV, DAL, AC, JBLU vs Industry Composite (Monthly Data)')

airline_shares_plot_a

##industry composite vs CargoJet, United, Alaska & American (high share prices)
airline_shares_plot_b <- ggplot() +
  geom_line(aes(x=Date,y=Close,group = 1),data = AirlineAverage_Monthly_DF, color = 'black', size = 1.3) +
  geom_line(aes(x=Date,y=Close,group = 1),data = CargoJet_CJT.TO_Monthly_DF, color = 'red', size = 0.6) + 
  geom_line(aes(x=Date,y=Close,group = 1),data = UnitedAir_UAL_Monthly_DF, color = 'blue', size = 0.6) +
  geom_line(aes(x=Date,y=Close,group = 1),data = AlaskaAir_ALK_Mothnly_DF, color = 'green', size = 0.6) + 
  geom_line(aes(x=Date,y=Close,group = 1),data = AmericanAir_AAL_Monthly_DF, color = 'orange', size = 0.6) +
  xlab('Date') +
  ylab('Share Price (Monthly Close)') +
  ggtitle('CJT, UAL, ALK, AAL vs Industry Composite (Monthly Data)')

airline_shares_plot_b

#share price vs. bond price and finally oil price vs. share price
#oil price vs. time, bond price vs. time, oil price vs bond prie

#oil price vs time
oil_plot <- ggplot() +
  geom_line(aes(x=Date, y=Close, group =1), data = CrudeOil_CLF_Monthly_DF, size = 1) +
  xlab('Date') +
  ylab('Price per Barrel ($USD)') + 
  ggtitle('NYMEX Light Sweet Crude Oil Futures (Jan 2000 - Mar 2020')
oil_plot

#oil price & VIX vs time
oil_vix_plot <- ggplot() +
  geom_line(aes(x=Date, y=Close, group =1), data = CrudeOil_CLF_Monthly_DF, size = 1) +
  geom_line(aes(x=Date,y=Close,group = 1),data = Volatility_VIX_Monthly_DF, color = 'red', size = 1)+
  xlab('Date') +
  ylab('Value') + 
  ggtitle('NYMEX Light Sweet Crude Oil Futures/CBOE VIX')
oil_vix_plot

#US treasury 10 bond yield vs time
bond_plot <- ggplot() + 
  geom_line(aes(x=Date, y=Close, group =1), data = Treasury10yrBond_TNX_Daily_DF, color = 'red', size = 0.6) +
  xlab('Date') +
  ylab('Yield (%)') + 
  ggtitle('US Treasury 10yr Bond Yield (Jan 2000 - Mar 2020)')
bond_plot

#oil price vs airline industry composite share price
oil_plotb <- ggplot() +
  geom_line(aes(x=Date, y=Close, group =1),data = CrudeOil_CLF_Monthly_DF, size = 1) +
  geom_line(aes(x=Date,y=Close,group = 1),data = AirlineAverage_Monthly_DF, size = 1, color = 'red') +
  xlab('Date') +
  ylab('($USD)') + 
  ggtitle('NYMEX Crude Oil Futures/Airline Composite Share Price (Jan 2000 - Mar 2020)')
oil_plotb 

#### DATA CORRELATION EXPLORATION ####

##correlation
correlation_data <- merged_data[, sapply(merged_data, is.numeric)]
full_correlation<-cor(correlation_data, method = "pearson", use = "complete.obs")

#### LINEAR REGRESSION MODELLING ####

#### AIRCANADA LINEAR ####

acmodel2<-lm(AirCanada ~ SP500 + Treasury10 + JetFuel, merged_data)
multicolacmodel2<-vif(acmodel2)

summary(acmodel2)
par(mfrow = c(2,2))
plot(acmodel2)

plot(density(resid(acmodel2)))

#### ALASKA AIR LINEAR ####

#- need to examine plot - Residuals vs Fitted (heteroskedasticity? - not as much variance in the lower values)
alaskamodel4<-lm(AlaskaAir ~ DowJ + Treasury10 + JetFuel + Volatility, merged_data)
multicolalaskamodel4<-vif(alaskamodel4)

summary(alaskamodel4)
par(mfrow = c(2,2))
plot(alaskamodel4)

plot(density(resid(alaskamodel4)))

##alaskasubset data
AlaskaAirsubsetdata<-merged_data[ c(4,7,8,17,23)]
alaskaplot<-ggpairs(AlaskaAirsubsetdata)

##correlation testing
alaska_correlation<-cor(AlaskaAirsubsetdata, method = "pearson", use = "complete.obs")

##highlights which correlations are significant
alaskasignificance<-rcorr(as.matrix(AlaskaAirsubsetdata))

alaska_sig_corr_plot<-corrplot(alaskasignificance$r, type="upper", order="hclust", 
                               p.mat = alaskasignificance$P, sig.level = 0.01, insig = "blank")

#### AMERICAN AIR LINEAR ####

#significant - 0.766
AmericanAirmodel5<-lm(AmericanAir ~ JetFuel + Volatility + Unemployment, merged_data)

summary(AmericanAirmodel5)
par(mfrow = c(2,2))
plot(AmericanAirmodel5)

plot(density(resid(AmericanAirmodel5)))

#### CARGOJET LINEAR ####

##significant 0.96
Cargojetmodel1<-lm(CargoJet ~ SP500 + Treasury10 + JetFuel + Volatility, merged_data)
vif(Cargojetmodel1)

summary(Cargojetmodel1)

##the 239th case has leverage on the model - residuals vs. leverage 
par(mfrow = c(2,2))
plot(Cargojetmodel1)

plot(density(resid(Cargojetmodel1)))

#### CHORUS LINEAR ####

## r-squared - 0.62
chorusmodel6<-lm(Chorus ~ DowJ + Volatility, merged_data)
summary(chorusmodel6)

##plots - likely need to remodel
par(mfrow = c(2,2))
plot(chorusmodel6)

#### DELTA LINEAR ####

Deltamodel3<-lm(Delta ~ DowJ + JetFuel + Volatility + Unemployment, merged_data)

##some collinearity between DowJ and Unemployment
vif(Deltamodel3)

##plots
par(mfrow = c(2,2))
plot(Deltamodel3)

#### JETBLUE LINEAR ####

##Two potential models
##0.82 R-squared - no multicollineratity
JetBluemodel1<-lm(JetBlue ~ Treasury10 + JetFuel + Volatility + Unemployment, merged_data)

##0.84 R-squared - slight multicollinearity between DowJ and Employment
JetBluemodel2<-lm(JetBlue ~ DowJ + Treasury10 + JetFuel + Volatility + Unemployment, merged_data)


par(mfrow = c(2,2))
plot(JetBluemodel1)

#### SOUTHWEST LINEAR ####

southwestmodel1<-lm(Southwest ~ DowJ + Treasury10 + JetFuel + Unemployment, merged_data)
multicolsouthwest1<-vif(southwestmodel1)

##need to review residuals vs fitted (heteroskedasticity? - not as much variance in the lower values)
summary(southwestmodel1)
par(mfrow = c(2,2))
plot(southwestmodel1)
vif(southwestmodel1)

plot(density(resid(southwestmodel1)))

#### SPIRITSAVE LINEAR ####

#R-squared - 0.35
Spiritsavemodel6<-lm(SpiritSAVE ~ JetFuel + Treasury10 + Volatility, merged_data)

par(mfrow = c(2,2))
plot(Spiritsavemodel6)

plot(density(resid(Spiritsavemodel6)))

#### UNITED AIR LINEAR ####

Unitedairmodel1<-lm(UnitedAir ~ DowJ + Treasury10 + JetFuel + Volatility, merged_data)

par(mfrow = c(2,2))
plot(Unitedairmodel1)

plot(density(resid(Unitedairmodel1)))

#### ARIMA MODELLING ####

#ARIMA stands for AutoRegressive Integrated Moving Average,

#AR terms refer to the lags of the differenced series

#MA terms refer to the lags of errors

#I is the number of difference used to make the time series stationary

#Assumptions: 
#i) data is stationary (properties of the series don't depend on the time when captured)  
#ii) data is univariate (ARIMA works on a single variable)

#### AIRLINE ARIMA ####

#citation for much of this section: https://datascienceplus.com/time-series-analysis-using-arima-model-in-r/
#https://github.com/SubhasreeUC/Master/blob/master/TimeSeriesExample.R

#converting data into workable format 

##extracting needed columns from DF (close and date)
airline_extract <- select(AirlineAverage_Monthly_DF,1,5)

#conversion to ts format, (freq = 12 for monthly data)
airline_ts <- ts(airline_extract$Close, frequency = 12, start = c(2000,4))

#subsetting data into smaller time windows
airline_ts_2000s <- window(airline_ts, end = 2010)
airline_ts_2010s <- window(airline_ts,start = 2010)

#decompose into time series components
airline_ts_components <-  decompose(airline_ts)

#visualizing components
plot(airline_ts_components)

#checking for stationarity 
urkpssTest(airline_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
airline_ts_stationary = diff(airline_ts, differences=)
plot(airline_ts_stationary)

#removing seasonality
airline_ts_season_adjusted <- airline_ts_stationary - airline_ts_components$seasonal
airline_ts_seasonal_differenced <- diff(airline_ts_season_adjusted, differences=1)
urkpssTest(airline_ts_seasonal_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(airline_ts_seasonal_differenced)

#identifying trends using auto & partial autocorrelation functions
#trends are used to identify the ARMIA values for p,d & q for comparison with auto.arima()
#p is # of autoregressive terms
#d is the # of non-seasonal differences needed for stationarity
#q is the # of lagged forecast errors in the prediction equation
acf(airline_ts_seasonal_differenced)
pacf(airline_ts_seasonal_differenced)

#various nonseasonal ARIMA model types:
#ARIMA(1,1,0) = differenced first-order autoregressive model
#ARIMA(0,1,1) without constant = simple exponential smoothing
#ARIMA(0,1,1) with constant = simple exponential smoothing with growth
#ARIMA(0,2,1) or (0,2,2) without constant = linear exponential smoothing
#ARIMA(1,1,2) with constant = damped-trend linear exponential smoothing
#the seasonal components of the model P,D,Q are the same as the nonseasonal but
#but involves backshifts of the seasonal data  (ie non-seasonal terms are multiplied by seasonal terms)

#n diffs estimates the number of differences that should be used, nsdfiffs the number of seasonal diffs
ndiffs(airline_ts)
nsdiffs(airline_ts)

#auto.arima evaluates various ARIMA models and then re-fits and reccomends the best model without approximations
#D = 1 ensures this evaulation takes into account the seasonality of our data seen above
auto.arima(airline_ts,trace = TRUE,D=1)

#fitting the reccomended ARIMA model from auto.arima 
#method CSS minimizes the sum of squared residuals
#method ML maximizes the log-likelihood of the ARIMA model
#CSS-ML first sets starting params to 0 (CSS), then applies ML to pass the CSS parameter estimates for the optimization

#ML is used when it can be as it sets the initial state using sample data instead of setting them to 0
airline_ARIMA <- arima(airline_ts, order=c(2,1,2),seasonal = list(order = c(1,1,1), period =12), method='ML')

#testing statistical significance of ARIMA model
coeftest(airline_ARIMA) 
confint(airline_ARIMA)

#forecasting future values using fitted ARIMA
airline_futurVal <- forecast(airline_ARIMA,h=12)
autoplot(airline_futurVal)

#### OIL ARIMA ####

##extracting needed columns from WTI Spot Price DF (Value and Date)
oil_extract <- select(CrudeOil_CLF_Monthly_DF,1,5)
oil_ts <- ts(oil_extract$Close,frequency = 12,start = c(2000,4))

#subsetting data into smaller windows
oil_ts_2000s <- window(oil_ts,end = 2010)
oil_ts_2010s <- window(oil_ts,start =2010)

#decompose into time series components
oil_components<-  decompose(oil_ts)

#visualizing components
plot(oil_components)

#checking for stationarity 
urkpssTest(oil_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
oil_ts_stationary = diff(oil_ts, differences=3)
plot(oil_ts_stationary)

#removing seasonality
oil_ts_season_adjusted <- oil_ts_stationary - oil_components$seasonal
oil_ts_season_differenced <- diff(oil_ts_season_adjusted, differences=1)
urkpssTest(oil_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(oil_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(oil_ts_season_differenced)
pacf(oil_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(oil_ts)
nsdiffs(oil_ts)
auto.arima(oil_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

oil_fit_ARIMA <- arima(oil_ts, order=c(2,0,0),seasonal = list(order = c(2,1,0), period =12),  method='CSS-ML')

#testing statistical significance of ARIMA model
coeftest(oil_fit_ARIMA) 
confint(oil_fit_ARIMA)

#forecasting future values using ARIMA model
oil_futurVal <- forecast(oil_fit_ARIMA,h=12)
autoplot(forecast(oil_futurVal))

#### T-BOND ARIMA ####

##extracting needed columns from DF (close and date)
tbond_extract <- select(Treasury10yrBond_TNX_Monthly_DF,1,5)
tbond_ts <- ts(tbond_extract$Close,frequency = 12,start = c(2000,4))

#subsetting data into smaller windows
tbond_ts_2000s <- window(tbond_ts,end = 2010)
tbond_ts_2010s <- window(tbond_ts,start =2010)

#decomposing ts into time series components
tbond_components <-  decompose(tbond_ts)

#visualizing components
plot(tbond_components)

#checking for stationarity 
urkpssTest(tbond_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tbond_ts_stationary = diff(tbond_ts, differences=1)
plot(tbond_ts_stationary)

#removing seasonality
tbond_ts_season_adjusted <- tbond_ts_stationary - tbond_components$seasonal
tbond_ts_season_differenced <- diff(tbond_ts_season_adjusted, differences=1)
urkpssTest(tbond_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(tbond_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(tbond_ts_season_differenced)
pacf(tbond_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(tbond_ts)
nsdiffs(tbond_ts)
auto.arima(tbond_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

tbond_fit_ARIMA <- arima(tbond_ts, order=c(1,0,0),seasonal = list(order = c(2,1,0), period =12),method='ML')

#testing statistical significance of ARIMA model
coeftest(tbond_fit_ARIMA) 
confint(tbond_fit_ARIMA)

#forecasting future values using ARIMA model
tbond_futurVal <- forecast(tbond_fit_ARIMA,h=12)
autoplot(tbond_futurVal)

#### JET FUEL ARIMA ####

##extracting needed columns from DF (close and date)
jetfuel_extract <- select(JetFuelSpotPrice_US_Monthly_DF,1,2)

#reordering (2000 to 2020) not (2020 to 2000)
jetfuel_extract <- jetfuel_extract[nrow(jetfuel_extract):1,]
jetfuel_ts <- ts(jetfuel_extract$JetFuel,frequency = 12,start = c(2000,4))

#decomposing ts into time series components
jetfuel_components <-  decompose(jetfuel_ts)

#visualizing components
plot(jetfuel_components)

#checking for stationarity 
urkpssTest(jetfuel_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
jetfuel_ts_stationary = diff(jetfuel_ts, differences=1)
plot(jetfuel_ts_stationary)

#removing seasonality
jetfuel_ts_season_adjusted <- jetfuel_ts_stationary - jetfuel_components$seasonal
jetfuel_ts_season_differenced <- diff(jetfuel_ts_season_adjusted, differences=1)
urkpssTest(jetfuel_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(jetfuel_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(jetfuel_ts_season_differenced)
pacf(jetfuel_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(jetfuel_ts)
nsdiffs(jetfuel_ts)
auto.arima(jetfuel_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

jetfuel_fit_ARIMA <- arima(jetfuel_ts, order=c(1,0,2),seasonal = list(order = c(2,1,0), period =12),method='ML')

#testing statistical significance of ARIMA model
coeftest(jetfuel_fit_ARIMA) 
confint(jetfuel_fit_ARIMA)

#forecasting future values using ARIMA model
jetfuel_futurVal <- forecast(jetfuel_fit_ARIMA,h=12)
autoplot(jetfuel_futurVal)

#### UNEMPLOYMENT ARIMA ####

##extracting needed columns from DF (close and date)
unemployment_extract <- select(Unemployment_US_Monthly_DF,1,2)

#reordering data (2000 to 2020) not (2020 to 2000)
unemployment_extract <- unemployment_extract[nrow(unemployment_extract):1,]
unemployment_ts <- ts(unemployment_extract$Unemployment,frequency = 12, start = c(2000,4))

#decomposing ts into time series components
unemployment_components <-  decompose(unemployment_ts)

#visualizing components
plot(unemployment_components)

#checking for stationarity 
urkpssTest(unemployment_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
unemployment_ts_stationary = diff(unemployment_ts, differences=1)
plot(unemployment_ts_stationary)

#removing seasonality
unemployment_ts_season_adjusted <- unemployment_ts_stationary - unemployment_components$seasonal
unemployment_ts_season_differenced <- diff(unemployment_ts_season_adjusted, differences=1)
urkpssTest(unemployment_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(unemployment_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(unemployment_ts_season_differenced)
pacf(unemployment_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(unemployment_ts)
nsdiffs(unemployment_ts)
auto.arima(unemployment_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

unemployment_fit_ARIMA <- arima(unemployment_ts, order=c(0,1,5),seasonal = list(order = c(2,1,2), period =12),method='ML')

#testing statistical significance of ARIMA model
coeftest(unemployment_fit_ARIMA) 
confint(unemployment_fit_ARIMA)

#forecasting future values using ARIMA model
unemployment_futurVal <- forecast(unemployment_fit_ARIMA,h=12)
autoplot(unemployment_futurVal)

#### VIX ARIMA ####

##extracting needed columns from DF (close and date)
vix_extract <- select(Volatility_VIX_Monthly_DF,1,5)
vix_ts <- ts(vix_extract$Close,frequency = 12,start = c(2000,4))

#decomposing ts into time series components
vix_components <-  decompose(vix_ts)

#visualizing components
plot(vix_components)

#checking for stationarity 
urkpssTest(vix_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
vix_ts_stationary = diff(vix_ts, differences=1)
plot(vix_ts_stationary)

#removing seasonality
vix_ts_season_adjusted <- vix_ts_stationary - vix_components$seasonal
vix_ts_season_differenced <- diff(vix_ts_season_adjusted, differences=1)
urkpssTest(vix_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(vix_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(vix_ts_season_differenced)
pacf(vix_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(vix_ts)
nsdiffs(vix_ts)
auto.arima(vix_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

vix_fit_ARIMA <- arima(vix_ts, order=c(1,0,0),seasonal = list(order = c(1,1,0), period =12),method='ML')

#testing statistical significance of ARIMA model
coeftest(vix_fit_ARIMA) 
confint(vix_fit_ARIMA)

#forecasting future values using ARIMA model
vix_futurVal <- forecast(vix_fit_ARIMA,h=12)
autoplot(vix_futurVal)

#### CPI ARIMA ####

##extracting needed columns from DF (close and date)
CPI_extract <- select(CPI_US_Monthly_DF,1,2)

#reordering columns (CPI data was upside down)
CPI_extract_reorder <- CPI_extract[nrow(CPI_extract):1,]
CPI_ts <- ts(CPI_extract_reorder$CPI,frequency = 12,start = c(2000,4),end = c(2020,3))

#decomposing ts into time series components
CPI_components <-  decompose(CPI_ts)

#visualizing components
plot(CPI_components)

#checking for stationarity 
urkpssTest(CPI_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
CPI_ts_stationary = diff(CPI_ts, differences=1)
plot(CPI_ts_stationary)

#removing seasonality
CPI_ts_season_adjusted <- CPI_ts_stationary - CPI_components$seasonal
CPI_ts_season_differenced <- diff(CPI_ts_season_adjusted, differences=1)
urkpssTest(CPI_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(CPI_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(CPI_ts_season_differenced)
pacf(CPI_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(CPI_ts)
nsdiffs(CPI_ts)
auto.arima(CPI_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

CPI_fit_ARIMA <- arima(CPI_ts, order=c(2,0,2),seasonal = list(order = c(1,1,2), period =12),method='CSS-ML')

#testing statistical significance of ARIMA model
coeftest(CPI_fit_ARIMA) 
confint(CPI_fit_ARIMA)

#forecasting future values using ARIMA model
CPI_futurVal <- forecast(CPI_fit_ARIMA,h=12)
autoplot(CPI_futurVal)

#### DOWJ ARIMA ####

##extracting needed columns from DF (close and date)
dowj_extract <- select(DowJones_DJI_Monthly_DF,1,5)

#reordering columns (CPI data was upside down)
dowj_ts <- ts(dowj_extract$Close,frequency = 12,start = c(2000,4),end = c(2020,3))

#decomposing ts into time series components
dowj_components <-  decompose(dowj_ts)

#visualizing components
plot(dowj_components)

#checking for stationarity 
urkpssTest(dowj_ts, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
dowj_ts_stationary = diff(dowj_ts, differences=1)
plot(dowj_ts_stationary)

#removing seasonality
dowj_ts_season_adjusted <- dowj_ts_stationary - dowj_components$seasonal
dowj_ts_season_differenced <- diff(dowj_ts_season_adjusted, differences=1)
urkpssTest(dowj_ts_season_differenced, type = c("tau"), lags = c("long"),use.lag = NULL, doplot = TRUE)
plot(dowj_ts_season_differenced)

#visually exploring trends to identify values for p,d & q
acf(dowj_ts_season_differenced)
pacf(dowj_ts_season_differenced)

#using auto.arima to evaluate various models for us and determine our best ARIMA params
ndiffs(dowj_ts)
nsdiffs(dowj_ts)
auto.arima(CPI_ts,trace = TRUE,D=1)

#fitting our data according to the optimal ARIMA params identified above
#in this case, algorithm convergence problems occur when using a ML method directly, so I set the initial parameter values
#for the ML objective function to be the CSS solution params ie, method = CSS-ML

dowj_fit_ARIMA <- arima(dowj_ts, order=c(2,0,2),seasonal = list(order = c(1,1,2), period =12),method='CSS-ML')

#testing statistical significance of ARIMA model
coeftest(dowj_fit_ARIMA) 
confint(dowj_fit_ARIMA)

#forecasting future values using ARIMA model
dowj_futurVal <- forecast(dowj_fit_ARIMA,h=12)
autoplot(dowj_futurVal)

#### MULTIVARIATE ARIMAX MODELLING ####

#### FORECASTED PREDICATOR DATA ####

#pulling in forecasted values for predicator values 

#making df's out of oil & tbond forecasts from above
futur_dowj_DF <- data.frame(dowj_futurVal)
futur_tbond_DF <- data.frame(tbond_futurVal)
futur_vix_DF <- data.frame(vix_futurVal)
futur_unemployment_DF <- data.frame(unemployment_futurVal)
futur_jetfuel_DF <- data.frame(jetfuel_futurVal)

#extracting desired values
Close<- futur_dowj_DF$Point.Forecast
Close.1 <- futur_tbond_DF$Point.Forecast
Close.2 <- futur_vix_DF$Point.Forecast
Unemployment <-futur_unemployment_DF$Point.Forecast
JetFuel <- futur_jetfuel_DF$Point.Forecast

#converting desired vals to data frames
dowj_forecast_DF <- data.frame(Close)
tbond_forecast_DF <- data.frame(Close.1)
vix_forecast_DF <- data.frame(Close.2)
unemployment_forecast_DF <- data.frame(Unemployment)
jetfuel_forecast_DF <- data.frame(JetFuel)

#creating data matrices of forecasted vals

##all variables
forecast_vals_default <- cbind(dowj_forecast_DF,tbond_forecast_DF,vix_forecast_DF,unemployment_forecast_DF,jetfuel_forecast_DF)
forecast_vals_DF_default <- tibble(forecast_vals_default)
forecast_xreg_default <-data.matrix(forecast_vals_DF_default)

##dowj
forecast_xreg_dowj <-data.matrix(dowj_forecast_DF)

##dowj + jetfuel
forecast_vals_dowj_jetfuel <- cbind(dowj_forecast_DF,jetfuel_forecast_DF)
forecast_vals_DF_dowj_jetfuel <- tibble(forecast_vals_dowj_jetfuel)
forecast_xreg_dowj_jetfuel <-data.matrix(forecast_vals_DF_dowj_jetfuel)

##dowj + unemployment + jetfuel
forecast_vals_dowj_unemployment_jetfuel <- cbind(dowj_forecast_DF,unemployment_forecast_DF,jetfuel_forecast_DF)
forecast_vals_DF_dowj_unemployment_jetfuel <- tibble(forecast_vals_dowj_unemployment_jetfuel)
forecast_xreg_dowj_unemployment_jetfuel <-data.matrix(forecast_vals_DF_dowj_unemployment_jetfuel)

##vix + unemployment + jetfuel
forecast_vals_vix_unemployment_jetfuel <- cbind(vix_forecast_DF,unemployment_forecast_DF,jetfuel_forecast_DF)
forecast_vals_DF_vix_unemployment_jetfuel <- tibble(forecast_vals_vix_unemployment_jetfuel)
forecast_xreg_vix_unemployment_jetfuel <-data.matrix(forecast_vals_DF_vix_unemployment_jetfuel)

#### AGGREGATE ARIMAX #####

#predicators are DowJ + Treasury10 + JetFuel + Volatility + Unemployment

#creating a dataframe with predicator variables
predicators <- data.frame(dowj_extract,tbond_extract,vix_extract,unemployment_extract,jetfuel_extract)
predicator_extract_default <- select(predicators,2,4,6,8,9)

#selecting significant predicator variables, Dowj + Unemployment + Jet Fuel
predicator_extract_aggregate <- select(predicators,2,8,9)

#converting predicator df to a data matrix for modelling
xreg_default <- data.matrix(predicator_extract_default)
xreg_aggregate <- data.matrix(predicator_extract_aggregate)

#determining arimax parameters with auto.arima
auto.arima(airline_ts,xreg = xreg_aggregate, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax <- Arima(airline_ts, order=c(2,0,0),seasonal = list(order = c(2,1,0), period =12), method='ML',xreg = xreg_aggregate)

#testing statistical significance of ARIMAX model
coeftest(arimax) 
confint(arimax)

#forecasting future values using fitted ARIMAX model
arimax_val <- forecast(arimax,xreg = forecast_xreg_dowj_unemployment_jetfuel)
autoplot(arimax_val)

#### AIRCANADA ARIMAX #####

#significant predicators are DowJ + Unemployment + JetFuel 

#converting to .ts object
##extracting needed columns from DF (close and date)
aircanada_extract <- select(AirCanada_AC.TO_Monthly_DF,1,2)
aircanada_ts <- ts(aircanada_extract$AirCanada,frequency = 12,start = c(2012,6))

#creating a dataframe with the predicator variables
#predicator_extract_default <- select(predicators,2,4,6,8,9)
predicator_extract_aircanada <- select(predicators,2,8,9)

#chopping x_reg american to be same length as american ts data
predicator_extract_aircanada <- predicator_extract_aircanada[147:240,]

#converting predicator df to a data matrix for modelling
#xreg_default <- data.matrix(predicator_extract_default)
xreg_aircanada <- data.matrix(predicator_extract_aircanada)

#dermining arimax parameters with auto.arima
auto.arima(aircanada_ts,xreg = xreg_aircanada, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_aircanada <- Arima(aircanada_ts, order=c(0,1,0),seasonal = list(order = c(1,1,1), period =12), method='ML',xreg = xreg_aircanada)

#testing statistical significance of ARIMAX model
coeftest(arimax_aircanada) 
confint(arimax_aircanada)

#forecasting future values using fitted ARIMAX model
arimax_val_aircanada <- forecast(arimax_aircanada,xreg = forecast_xreg_dowj_unemployment_jetfuel)
autoplot(arimax_val_aircanada)

#### ALASKA AIR ARIMAX #####

#significant predicators are DowJ + Unemployment + JetFuel 

#converting to .ts object
##extracting needed columns from DF (close and date)
alaska_extract <- select(AlaskaAir_ALK_Mothnly_DF,1,2)
alaska_ts <- ts(alaska_extract$AlaskaAir,frequency = 12,start = c(2000,4))

#creating a dataframe with the predicator variables
predicator_extract_alaska <- select(predicators,2,8,9)

#converting predicator df to a data matrix for modelling
xreg_alaska <- data.matrix(predicator_extract_alaska)

#determining arimax parameters with auto.arima
auto.arima(alaska_ts,xreg = xreg_alaska, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_alaska <- Arima(alaska_ts, order=c(0,1,0),seasonal = list(order = c(2,1,0), period =12), method='ML',xreg = xreg_alaska)

#testing statistical significance of ARIMAX model
coeftest(arimax_alaska) 
confint(arimax_alaska)

#forecasting future values using fitted ARIMAX model
arimax_val_alaska <- forecast(arimax_alaska,xreg = forecast_xreg_dowj_unemployment_jetfuel)
autoplot(arimax_val_alaska)

#### AMERICAN AIR ARIMAX #####

#significant predicators are DowJ + JetFuel 

#converting to .ts object
##extracting needed columns from DF (close and date)
american_extract <- select(AmericanAir_AAL_Monthly_DF,1,2)
american_ts <- ts(american_extract$AmericanAir,frequency = 12,start = c(2005,9))

#creating a dataframe with the predicator variables
#predicator_extract_default <- select(predicators,2,4,6,8,9)
predicator_extract_american <- select(predicators,2,9)

#chopping x_reg american to be same length as american ts data
predicator_extract_american <- predicator_extract_american[66:240,]

#converting predicator df to a data matrix for modelling
#xreg_default <- data.matrix(predicator_extract_default)
xreg_american <- data.matrix(predicator_extract_american)

#dermining arimax parameters with auto.arima
auto.arima(american_ts,xreg = xreg_american, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_american <- Arima(american_ts, order=c(1,0,0),seasonal = list(order = c(1,1,0), period =12), method='ML',xreg = xreg_american)

#testing statistical significance of ARIMAX model
coeftest(arimax_american) 
confint(arimax_american)

#forecasting future values using fitted ARIMAX model
arimax_val_american <- forecast(arimax_american,xreg = forecast_xreg_dowj_jetfuel)
dev.off()
autoplot(arimax_val_american)


#### CARGOJET ARIMAX #####

#significant predicators is DowJ 

#converting to .ts object
##extracting needed columns from DF (close and date)
cargojet_extract <- select(CargoJet_CJT.TO_Monthly_DF,1,2)
cargojet_ts <- ts(cargojet_extract$CargoJet,frequency = 12,start = c(2012,2))

#creating a dataframe with the predicator variables
predicator_extract_cargojet <- select(predicators,2)

#chopping predicator data to be same length as ts data
predicator_extract_cargojet <- predicator_extract_cargojet[143:240,]

#converting predicator df to a data matrix for modelling
xreg_cargojet <- data.matrix(predicator_extract_cargojet)
colnames(xreg_cargojet) <- "Close"

#dermining arimax parameters with auto.arima
auto.arima(cargojet_ts,xreg = xreg_cargojet, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_cargojet <- Arima(cargojet_ts, order=c(2,1,2),seasonal = list(order = c(2,1,0), period =12), method='CSS-ML',xreg = xreg_cargojet)

#testing statistical significance of ARIMAX model
coeftest(arimax_cargojet) 
confint(arimax_cargojet)

#forecasting future values using fitted ARIMAX model
arimax_val_cargojet <- forecast(arimax_cargojet,xreg = forecast_xreg_dowj)
autoplot(arimax_val_cargojet)

#### CHORUS ARIMAX ####

#significant predicators are Vix + Unemployment + Jetfuel 

#converting to .ts object
##extracting needed columns from DF (close and date)
chorus_extract <- select(Chorus_CHR.TO_Monthly_DF,1,2)
chorus_ts <- ts(chorus_extract$Chorus,frequency = 12,start = c(2016,5))

#creating a dataframe with the predicator variables
predicator_extract_chorus <- select(predicators,6,8,9)

#chopping predicator data to be same length as ts data
predicator_extract_chorus <- predicator_extract_chorus[194:240,]

#converting predicator df to a data matrix for modelling
xreg_chorus <- data.matrix(predicator_extract_chorus)

#dermining arimax parameters with auto.arima
auto.arima(chorus_ts,xreg = xreg_chorus, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_chorus <- Arima(chorus_ts, order=c(1,0,0),seasonal = list(order = c(1,1,0), period =12), method='ML',xreg = xreg_chorus)

#testing statistical significance of ARIMAX model
coeftest(arimax_chorus) 
confint(arimax_chorus)

#forecasting future values using fitted ARIMAX model
arimax_val_chorus <- forecast(arimax_chorus,xreg = forecast_xreg_vix_unemployment_jetfuel)
autoplot(arimax_val_chorus)

#### DELTA ARIMAX ####

#significant predicators are Dowj + Unemployment + Jetfuel 

#converting to .ts object
##extracting needed columns from DF (close and date)
delta_extract <- select(Delta_DAL_Monthly_DF,1,2)
delta_ts <- ts(delta_extract$Delta,frequency = 12,start = c(2007,5))

#creating a dataframe with the predicator variables
predicator_extract_delta <- select(predicators,2,8,9)

#chopping predicator data to be same length as ts data
predicator_extract_delta <- predicator_extract_delta[86:240,]

#converting predicator df to a data matrix for modelling
xreg_delta <- data.matrix(predicator_extract_delta)

#dermining arimax parameters with auto.arima
auto.arima(delta_ts,xreg = xreg_delta, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_delta <- Arima(delta_ts, order=c(1,1,0),seasonal = list(order = c(1,1,0), period =12), method='ML',xreg = xreg_delta)

#testing statistical significance of ARIMAX model
coeftest(arimax_delta) 
confint(arimax_delta)

#forecasting future values using fitted ARIMAX model
arimax_val_delta <- forecast(arimax_delta,xreg = forecast_xreg_dowj_unemployment_jetfuel)
autoplot(arimax_val_delta)

#### JETBLUE ARIMAX ####

#significant predicators are Dowj + Jetfuel 

#converting to .ts object

##extracting needed columns from DF (close and date)
jetblue_extract <- select(JetBlue_JBLU_Monthly_DF,1,2)
jetblue_ts <- ts(jetblue_extract$JetBlue,frequency = 12,start = c(2002,4))

#creating a dataframe with the predicator variables
predicator_extract_jetblue <- select(predicators,2,9)

#chopping predicator data to be same length as ts data
predicator_extract_jetblue <- predicator_extract_jetblue[25:240,]

#converting predicator df to a data matrix for modelling
xreg_jetblue <- data.matrix(predicator_extract_jetblue)

#dermining arimax parameters with auto.arima
auto.arima(jetblue_ts,xreg = xreg_jetblue, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_jetblue <- Arima(jetblue_ts, order=c(1,0,0),seasonal = list(order = c(2,1,1), period =12), method='ML',xreg = xreg_jetblue)

#testing statistical significance of ARIMAX model
coeftest(arimax_jetblue) 
confint(arimax_jetblue)

#forecasting future values using fitted ARIMAX model
arimax_val_jetblue <- forecast(arimax_jetblue,xreg = forecast_xreg_dowj_jetfuel)
autoplot(arimax_val_jetblue)

#### SOUTHWEST ARIMAX #####

#significant predicators are DowJ + JetFuel 

#converting to .ts object
##extracting needed columns from DF (close and date)
southwest_extract <- select(SouthwestAir_LUV_Monthly_DF,1,2)
southwest_ts <- ts(southwest_extract$Southwest,frequency = 12,start = c(2000,4))

#creating a dataframe with the predicator variables
predicator_extract_southwest <- select(predicators,2,9)

#converting predicator df to a data matrix for modelling
xreg_southwest <- data.matrix(predicator_extract_southwest)

#determining arimax parameters with auto.arima
auto.arima(southwest_ts,xreg = xreg_southwest, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_southwest <- Arima(southwest_ts, order=c(1,0,0),seasonal = list(order = c(2,1,0), period =12), method='ML',xreg = xreg_southwest)

#testing statistical significance of ARIMAX model
coeftest(arimax_southwest) 
confint(arimax_southwest)

#forecasting future values using fitted ARIMAX model
arimax_val_southwest <- forecast(arimax_southwest,xreg = forecast_xreg_dowj_jetfuel)
autoplot(arimax_val_southwest)

#### SPIRITSAVE ARIMAX ####

#significant predicators is Dowj 

#converting to .ts object

##extracting needed columns from DF (close and date)
spirit_extract <- select(Spirit_SAVE_Monthly_DF,1,2)
spirit_ts <- ts(spirit_extract$SpiritSAVE,frequency = 12,start = c(2011,5))

#creating a dataframe with the predicator variables
predicator_extract_spirit <- select(predicators,2)

#chopping predicator data to be same length as ts data
predicator_extract_spirit <- predicator_extract_spirit[134:240,]

#converting predicator df to a data matrix for modelling
xreg_spirit <- data.matrix(predicator_extract_spirit)
colnames(xreg_spirit) <- "Close"

#dermining arimax parameters with auto.arima
auto.arima(spirit_ts,xreg = xreg_spirit, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_spirit <- Arima(spirit_ts, order=c(0,1,1),seasonal = list(order = c(2,1,0), period =12), method='ML',xreg = xreg_spirit)

#testing statistical significance of ARIMAX model
coeftest(arimax_spirit) 
confint(arimax_spirit)

#forecasting future values using fitted ARIMAX model
arimax_val_spirit <- forecast(arimax_spirit,xreg = forecast_xreg_dowj)
autoplot(arimax_val_spirit)

#### UNITED AIR ARIMAX ####

#significant predicators are Dowj + unemployment + jetfuel

#converting to .ts object

##extracting needed columns from DF (close and date)
united_extract <- select(UnitedAir_UAL_Monthly_DF,1,2)
united_ts <- ts(united_extract$UnitedAir,frequency = 12,start = c(2006,2))

#creating a dataframe with the predicator variables
predicator_extract_united <- select(predicators,2,8,9)

#chopping predicator data to be same length as ts data
predicator_extract_united <- predicator_extract_united[71:240,]

#converting predicator df to a data matrix for modelling
xreg_united <- data.matrix(predicator_extract_united)

#dermining arimax parameters with auto.arima
auto.arima(united_ts,xreg = xreg_united, D=1,trace = TRUE)

#modelling arimax using Arima() from forecast package NOT arima(0 from stats package
arimax_united <- Arima(united_ts, order=c(1,0,0),seasonal = list(order = c(1,1,2), period =12), method='ML',xreg = xreg_united)

#testing statistical significance of ARIMAX model
coeftest(arimax_united) 
confint(arimax_united)

#forecasting future values using fitted ARIMAX model
arimax_val_united <- forecast(arimax_united,xreg = forecast_xreg_dowj_unemployment_jetfuel)
autoplot(arimax_val_united)

#### EXPORTING FORECAST DATA FOR DASHBOARDING ####

#exporting airline forecasts to excel files
write.xlsx(arimax_val_aircanada, "AC_forecast_results.xlsx")
write.xlsx(arimax_val_alaska, "ALK_forecast_results.xlsx")
write.xlsx(arimax_val_american, "AAL_forecast_results_fixed.xlsx")
write.xlsx(arimax_val_cargojet, "CJT_forecast_results.xlsx")
write.xlsx(arimax_val_chorus, "CHR_forecast_results.xlsx")
write.xlsx(arimax_val_delta, "DAL_forecast_results.xlsx")
write.xlsx(arimax_val_jetblue, "JBLU_forecast_results.xlsx")
write.xlsx(arimax_val_southwest, "LUV_forecast_results.xlsx")
write.xlsx(arimax_val_spirit, "SAVE_forecast_results.xlsx")
write.xlsx(arimax_val_united, "UAL_forecast_results.xlsx")

#exporting predicator forecasts to excel files
write.xlsx(forecast_xreg_default,"forecasted_predicators.xlsx")
