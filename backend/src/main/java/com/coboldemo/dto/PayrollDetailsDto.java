package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * `GET /api/payroll/{id}/details` response.
 */
public final class PayrollDetailsDto {
  private final String employeeId;
  private final BigDecimal hourlyRate;
  private final BigDecimal hoursWorked;
  private final BigDecimal grossPay;
  private final BigDecimal taxAmount;
  private final BigDecimal netPay;
  private final BigDecimal effectiveTaxRate;

  public PayrollDetailsDto(
      String employeeId,
      BigDecimal hourlyRate,
      BigDecimal hoursWorked,
      BigDecimal grossPay,
      BigDecimal taxAmount,
      BigDecimal netPay,
      BigDecimal effectiveTaxRate) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.hourlyRate = NumericContract.enforceScale(hourlyRate);
    this.hoursWorked = NumericContract.enforceScale(hoursWorked);
    this.grossPay = NumericContract.enforceScale(grossPay);
    this.taxAmount = NumericContract.enforceScale(taxAmount);
    this.netPay = NumericContract.enforceScale(netPay);
    this.effectiveTaxRate = NumericContract.enforceScale(effectiveTaxRate);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public BigDecimal getHourlyRate() {
    return hourlyRate;
  }

  public BigDecimal getHoursWorked() {
    return hoursWorked;
  }

  public BigDecimal getGrossPay() {
    return grossPay;
  }

  public BigDecimal getTaxAmount() {
    return taxAmount;
  }

  public BigDecimal getNetPay() {
    return netPay;
  }

  public BigDecimal getEffectiveTaxRate() {
    return effectiveTaxRate;
  }
}
