package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Response body for `GET /api/payroll/{id}/tax`.
 */
public final class PayrollTaxDto {
  private final String employeeId;
  private final BigDecimal totalTax;
  private final List<TaxBracketDto> brackets;

  public PayrollTaxDto(String employeeId, BigDecimal totalTax, List<TaxBracketDto> brackets) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.totalTax = NumericContract.enforceScale(totalTax);
    this.brackets = Collections.unmodifiableList(Objects.requireNonNull(brackets));
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public BigDecimal getTotalTax() {
    return totalTax;
  }

  public List<TaxBracketDto> getBrackets() {
    return brackets;
  }
}
