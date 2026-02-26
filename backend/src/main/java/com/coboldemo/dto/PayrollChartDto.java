package com.coboldemo.dto;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Serializes the same normalized chart data that `chart.cob` renders on the terminal.
 */
public final class PayrollChartDto {
  private final String employeeId;
  private final String normalizedTo;
  private final List<PayrollChartPointDto> dataPoints;

  public PayrollChartDto(String employeeId, String normalizedTo, List<PayrollChartPointDto> dataPoints) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.normalizedTo = Objects.requireNonNull(normalizedTo);
    this.dataPoints = Collections.unmodifiableList(Objects.requireNonNull(dataPoints));
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public String getNormalizedTo() {
    return normalizedTo;
  }

  public List<PayrollChartPointDto> getDataPoints() {
    return dataPoints;
  }
}
