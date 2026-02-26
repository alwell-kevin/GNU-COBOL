package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;
import java.util.Objects;

public final class BankingBalancesDto {
  private final BigDecimal checking;
  private final BigDecimal savings;
  private final BigDecimal loan;

  public BankingBalancesDto(BigDecimal checking, BigDecimal savings, BigDecimal loan) {
    this.checking = NumericContract.enforceScale(checking);
    this.savings = NumericContract.enforceScale(savings);
    this.loan = NumericContract.enforceScale(loan);
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
}
