package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * Single data point used to build the ASCII chart that the COBOL `chart.cob` renders.
 */
public final class PayrollChartPointDto {
  private final String label;
  private final BigDecimal value;

  public PayrollChartPointDto(String label, BigDecimal value) {
    this.label = Objects.requireNonNull(label);
    this.value = NumericContract.enforceScale(value);
  }

  public String getLabel() {
    return label;
  }

  public BigDecimal getValue() {
    return value;
  }
}
