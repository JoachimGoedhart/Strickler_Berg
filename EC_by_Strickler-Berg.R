# R-script that calculates spectroscopic parameters of a fluorophore using the Strickler-Berg equation 
# Strickler and Berg, 1992: https://doi.org/10.1063/1.1733166
# This script can be used for calculation of Extinction coefficient (EC), for the case where
# Quantum Yield (QY) and fluorescence lifetime (tau) are known
# By: Joachim Goedhart and Marten Postma - 2019

require(caTools)  #Needed for trapz()

# Read a csv file where the columns have the data of respectively 1:Wavelength [nm], 2:Absorbance and 3:Emission
df <- read.csv("mTurquoise2_spectra.csv")

#Set the known spectroscopic parameters for the fluorophore
EC = NA
QY = 0.93
tau = 4.00

#Set some physical parameters
#Refractive Index
n_cells = 1.365
n_H2O = 1.33
Pi = 3.141593
c = 3e10 #speed of light in cm/s
Navogadro = 6.022e23

#Calculate constant
constant = 8*2303*Pi*c/Navogadro

#Read wavelength data
lambda <- df$lambda

#Convert wavelength (nm) to wavenumber (cm-1)
wv <- 1/(lambda*1e-7)

#Replace missing values by zero
df[is.na(df)] <- 0

#Read Abs/Em data
Abs <- df$Abs_mTq2
Em <- df$Em_mTq2

#Calculate integrals
Em3 <- Em/wv^3
Int_I <-  trapz(wv,Em)/trapz(wv,(Em/wv^3))
Int_A <-  abs(trapz(wv,(Abs/wv)))

#Calculate the kr/EC
k <- constant*n_H2O^2*Int_I*Int_A

# Uncomment to calculate Quantum Yield, requires EC and tau to be known
# QY <- EC*k*tau*1e-9
# QY

# Uncomment to calculate the fluorescence lifetime [ns], requires EC and QY to be known
# tau <- QY * 1e9/k/EC
# tau

# Calculate Extinction Coefficient [M-1 cm-1]
EC <-  QY * 1e9/k/tau
EC


