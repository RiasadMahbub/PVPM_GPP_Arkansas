library(dplyr)

# ---- EVI-based fAPAR calculation ----
calculate_fapar_evi <- function(evi) {
  return(1.25 * (evi - 0.1))  # Standard EVI to fAPAR conversion
}

# ---- VPM scalar functions ----
calculate_Ts <- function(T, Tmin, Tmax, Topt) {
  Ts <- ((T - Tmin) * (T - Tmax)) / (((T - Tmin) * (T - Tmax)) - (T - Topt)^2)
  return(Ts)
}

calculate_GPP <- function(Ts, LUEmax, Ws, PAR, FPAR) {
  GPP <- Ts * Ws * PAR * FPAR * LUEmax * 12.011
  return(GPP)
}

# ---- Constants ----
Tmin <- -1 
Tmax <- 48 
Topt <- 30.02308
LUEmax <- 0.06038462

# ---- Step 1: Compute LSWImax per siteyear ----
LSWImax_df <- rf_data %>%
  group_by(siteyear) %>%
  summarise(LSWImax = max(LSWI, na.rm = TRUE), .groups = "drop")

# ---- Step 2: Join LSWImax into main data ----
rf_data <- rf_data %>%
  left_join(LSWImax_df, by = "siteyear")

# ---- Step 3: Calculate fAPAR from EVI ----
rf_data <- rf_data %>%
  mutate(fAPAR_evi = calculate_fapar_evi(EVI))

# ---- Step 4: Extract required variables ----
T      <- rf_data$Tair_site
PAR    <- rf_data$PAR_site
FPAR   <- rf_data$fAPAR
LSWI   <- rf_data$LSWI
LSWImax <- rf_data$LSWImax

# ---- Step 5: Calculate Ts, Ws, and GPP ----
Ts <- calculate_Ts(T, Tmin, Tmax, Topt)
Ws <- (1 + LSWI) / (1 + LSWImax)
GPPpredictedVPM_EVI <- calculate_GPP(Ts, LUEmax, Ws, PAR, FPAR)

# ---- Step 6: Store in dataframe ----
rf_data$GPPpredictedVPM_EVI <- GPPpredictedVPM_EVI


range(rf_data$GPP_site)
range(rf_data$GPPpredictedVPM)
head(rf_data[order(-rf_data$GPPpredictedVPM_EVI), ], 200)

# First add all calculated variables to the dataframe
rf_data <- rf_data %>%
  mutate(
    Ts = Ts,
    Ws = Ws,
    PAR = PAR,
    FPAR = FPAR
  )

# Now select all relevant columns
rf_data %>%
  select(
    siteyear,
    LSWI, LSWImax,
    EVI, fAPAR_evi,
    Tair_site, 
    PAR_site, 
    Ts, Ws,
    GPP_site,
    GPPpredictedVPM_EVI
  ) %>%
  arrange(desc(GPPpredictedVPM_EVI)) %>%
  head(200)

hist(rf_data$GPPpredictedVPM_EVI)



# ======================================================================
# SECTION 2: VPM GPP Calculation Using Provided fAPAR Values (rf_data$fAPAR)
# ======================================================================

# ---- Step 1: Extract required variables ----
T       <- rf_data$Tair_site
PAR     <- rf_data$PAR_site
FPAR    <- rf_data$fAPAR  # using direct fAPAR from dataset
LSWI    <- rf_data$LSWI
LSWImax <- rf_data$LSWImax

# ---- Step 2: Calculate Ts, Ws, and GPP ----
Ts <- calculate_Ts(T, Tmin, Tmax, Topt)
Ws <- (1 + LSWI) / (1 + LSWImax)
GPPpredictedVPM <- calculate_GPP(Ts, LUEmax, Ws, PAR, FPAR)

# ---- Step 3: Store in dataframe ----
rf_data$GPPpredictedVPM <- GPPpredictedVPM

# ---- Step 4: Add calculated variables for direct fAPAR version ----
rf_data <- rf_data %>%
  mutate(
    Ts = Ts,
    Ws = Ws
  )

# ---- Step 5: View results ----
rf_data %>%
  select(
    siteyear,
    LSWI, LSWImax,
    EVI, fAPAR,
    Tair_site, 
    PAR_site, 
    Ts, Ws,
    GPP_site,
    GPPpredictedVPM,
    GPPpredictedVPM_EVI
  ) %>%
  arrange(desc(GPPpredictedVPM)) %>%
  head(200)
summary(rf_data$GPPpredictedVPM)
summary(rf_data$GPPpredictedVPM_EVI)
hist(rf_data$fAPAR)
hist(rf_data$fAPAR_evi)
