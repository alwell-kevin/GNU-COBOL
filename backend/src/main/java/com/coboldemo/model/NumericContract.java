package com.coboldemo/model;

import java.math.BigDecimal;
import java.math.RoundingMode;

/**
 * Shared numeric constraints derived from the COBOL copybooks.
 */
public final class NumericContract {
  public static final int SCALE = 2;
  public static final RoundingMode ROUNDING_MODE = RoundingMode.HALF_EVEN;

  private NumericContract() {}

  public static BigDecimal enforceScale(BigDecimal value) {
    if (value == null) {
      return BigDecimal.ZERO.setScale(SCALE, ROUNDING_MODE);
    }
    return value.setScale(SCALE, ROUNDING_MODE);
  }
}
