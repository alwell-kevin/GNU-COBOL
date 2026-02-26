package com.coboldemo/model;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * Mirrors {@code copybooks/payroll_calc.cpy} fields used in payroll detail screens.
 */
public final class PayrollCalculation {
  private final String employeeId;
  private final BigDecimal grossPay;
  private final BigDecimal taxAmount;
  private final BigDecimal netPay;
  private final BigDecimal effectiveTaxRate;

  public PayrollCalculation(
      String employeeId,
      BigDecimal grossPay,
      BigDecimal taxAmount,
      BigDecimal netPay,
      BigDecimal effectiveTaxRate) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.grossPay = NumericContract.enforceScale(grossPay);
    this.taxAmount = NumericContract.enforceScale(taxAmount);
    this.netPay = NumericContract.enforceScale(netPay);
    this.effectiveTaxRate = NumericContract.enforceScale(effectiveTaxRate);
  }

  public String getEmployeeId() {
    return employeeId;
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    PayrollCalculation that = (PayrollCalculation) o;
    return employeeId.equals(that.employeeId)
        && grossPay.equals(that.grossPay)
        && taxAmount.equals(that.taxAmount)
        && netPay.equals(that.netPay)
        && effectiveTaxRate.equals(that.effectiveTaxRate);
  }

  @Override
  public int hashCode() {
    return Objects.hash(employeeId, grossPay, taxAmount, netPay, effectiveTaxRate);
  }
}
