df <- df %>% 
  dplyr::mutate(
    # Log + 1 transformation for fraction
    Log1p_Fraction_Loss_ha_0 = log1p(Fraction_Loss_ha_0),
    
    # Inverse hyperbolic sine transformation for fraction
    IHS_Fraction_Loss_ha_0 = asinh(Fraction_Loss_ha_0),
    
    # Extensive margin for fraction
    Binary_Fraction_Loss_ha_0 = as.numeric(Fraction_Loss_ha_0 > 0),
    
    # Log-linear hybrid transformation for fraction
    LogLinear_Fraction_Loss_ha_0 = ifelse(Fraction_Loss_ha_0 > 0, 
                                          log(Fraction_Loss_ha_0), 
                                          -min(Fraction_Loss_ha_0[Fraction_Loss_ha_0 > 0], na.rm = TRUE)))