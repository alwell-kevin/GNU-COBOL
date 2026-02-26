package com.coboldemo.persistence.model;

import java.math.BigDecimal;
import java.util.Objects;

public final class AccountRecord {
  private final String employeeId;
  private final BigDecimal checking;
  private final BigDecimal savings;
  private final BigDecimal loan;

  public AccountRecord(String employeeId, BigDecimal checking, BigDecimal savings, BigDecimal loan) {
    this.employeeId = employeeId;
    this.checking = checking;
    this.savings = savings;
    this.loan = loan;
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public BigDecimal getChecking() {
    return checking;
  }

  public BigDecimal getSavings() {
    return savings;
  }

  public BigDecimal getLoan() {
    return loan;
  }

  @Override
  public int hashCode() {
    return Objects.hash(employeeId, checking, savings, loan);
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null || getClass() != obj.getClass()) {
      return false;
    }
    AccountRecord that = (AccountRecord) obj;
    return Objects.equals(employeeId, that.employeeId)
        && Objects.equals(checking, that.checking)
        && Objects.equals(savings, that.savings)
        && Objects.equals(loan, that.loan);
  }

  @Override
  public String toString() {
    return "AccountRecord{" +
        "employeeId='" + employeeId + '\'' +
        ", checking=" + checking +
        ", savings=" + savings +
        ", loan=" + loan +
        '}';
  }
}
