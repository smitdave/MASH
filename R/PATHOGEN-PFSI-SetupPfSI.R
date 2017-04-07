########################################
#  PFSI
#  Sean Wu
#  April 5, 2017
########################################



PFSI.SETUP <- function(

    ########################################
    #  Parameters
    ########################################

    # Transmission efficiency
    Pf_c   = 0.15,
    Pf_b   = 0.55,

    # Duration of Infection
    # (How many days does the infection last?)
    DurationPf = 200,

    # Latency:
    # (How many days after the infectious
    #  bite does the infection start?)
    LatentPf = 10,

    # Probability of Fever
    FeverPf = 0.3,

    # Timing of Fever Incident
    # (relative to start of the infection)
    mnFeverPf = 10,
    vrFeverPf = .1,

    # Probability of Treating
    TreatPf = .5,

    # Timing of Treatment
    # (relative to start of the fever)
    mnTreatPf = 1,

    # Prophylaxis, time to susceptibility
    mnChemoprophylaxisPf = 32,

    # Proportion Protected by PE Vaccination
    PEProtectPf = .99,

    # Proportion of infections Blocked
    peBlockPf = 1,
    mnPEPf = 270,
    vrPEPf = 50,

    # Proportion Protected by GS Vaccination
    GSProtectPf = 1,

    # Proportion of infections Blocked
    gsBlockPf = .9,
    mnGSPf = 180,
    vrGSPf = 20,

    #  RDT Probe and Accuracy
    rdtSensPf  = .9,
    rdtSpecPf  = .1,

    #  Light Microscopy Probe and Sensitivity
    lmSensPf  = .9,
    lmSpecPf  = .1,

    # Pedigree
    KeepPfHistory = TRUE

  ){

  # Define functions
  probeHost <<- probeHost_PfSI
  infectMosquito <<- infectMosquito_PfSI

  # Transmission efficiency
  Pf_c   <<- Pf_c
  Pf_b   <<- Pf_b

  # Duration of Infection
  # (How many days does the infection last?)
  DurationPf <<- DurationPf
  ttClearPf <<- function(){rexp(1, 1/DurationPf)}

  # Latency:
  # (How many days after the infectious
  #  bite does the infection start?)
  LatentPf <<- LatentPf
  ttInfectionPf <<- function(){LatentPf}

  # Probability of Fever
  FeverPf <<- FeverPf

  # Timing of Fever Incident
  # (relative to start of the infection)
  mnFeverPf <<- mnFeverPf
  vrFeverPf <<- vrFeverPf
  ttFeverPf <<- function(){mnFeverPf}

  # Probability of Treating
  TreatPf <<- TreatPf

  # Timing of Treatment
  # (relative to start of the fever)
  mnTreatPf <<- mnTreatPf
  ttTreatPf <<- function(){mnTreatPf}

  # Prophylaxis, time to susceptibility
  mnChemoprophylaxisPf <<- mnChemoprophylaxisPf
  ttSusceptiblePf <<- function(){mnChemoprophylaxisPf}

  # Proportion Protected by PE Vaccination
  PEProtectPf <<- PEProtectPf

  # Proportion of infections Blocked
  peBlockPf <<- peBlockPf
  mnPEPf <<- mnPEPf
  vrPEPf <<- vrPEPf
  ttPEWanePf <<- function(){
    rnorm(1, mnPEPf, vrPEPf)
  }

  # Proportion Protected by GS Vaccination
  GSProtectPf <<- GSProtectPf

  # Proportion of infections Blocked
  gsBlockPf <<- gsBlockPf
  mnGSPf <<- mnGSPf
  vrGSPf <<- vrGSPf
  ttGSWanePf <<- function(){
    rnorm(1, PfmeanGSProtect, PfvarGSprotect)
  }

  #  RDT Probe and Accuracy
  rdtSensPf <<- rdtSensPf
  rdtSpecPf <<- rdtSpecPf
  rdtTest <<- rdtTest_PfSI

  #  Light Microscopy Probe and Sensitivity
  lmSensPf <<- lmSensPf
  lmSpecPf <<- lmSpecPf
  lmTest <<- lmTest_PfSI

  # Mosquito Pf Object
  makePfM <<- makePfM_full

  # Pedigree
  # PfPedigree_XX <<- PfPedigree_full
  # makePfPedigree <<- makePfPedigree_full
  # makePfM <<- makePfM_full
  getPfParent <<- getPfParent_SI
  KeepPfHistory <<- KeepPfHistory

}
