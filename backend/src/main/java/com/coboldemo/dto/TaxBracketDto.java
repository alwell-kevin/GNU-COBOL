package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * Recreates the bracket table the COBOL tax view renders line-by-line.
 */
public final class TaxBracketDto {
  private final String name;
  private final BigDecimal ratePercent;
  private final BigDecimal taxAmount;

  public TaxBracketDto(String name, BigDecimal ratePercent, BigDecimal taxAmount) {
    this.name = Objects.requireNonNull(name);
    this.ratePercent = NumericContract.enforceScale(ratePercent);
    this.taxAmount = NumericContract.enforceScale(taxAmount);
  }

  public String getName() {
    return name;
  }

  public BigDecimal getRatePercent() {
    return ratePercent;
  }

  public BigDecimal getTaxAmount() {
    return taxAmount;
  }
}
