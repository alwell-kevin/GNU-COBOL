package com.coboldemo/model;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

/**
 * Captures balances from {@code data/accounts.dat} and preserves COBOL scale + signed values for checking.
 */
public final class BankingAccount {
  private final String employeeId;
  private final BigDecimal checkingBalance;
  private final BigDecimal savingsBalance;
  private final BigDecimal loanBalance;
  private final Instant lastUpdated;

  public BankingAccount(
      String employeeId,
      BigDecimal checkingBalance,
      BigDecimal savingsBalance,
      BigDecimal loanBalance,
      Instant lastUpdated) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.checkingBalance = NumericContract.enforceScale(checkingBalance);
    this.savingsBalance = NumericContract.enforceScale(savingsBalance);
    this.loanBalance = NumericContract.enforceScale(loanBalance);
    this.lastUpdated = Objects.requireNonNull(lastUpdated);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public BigDecimal getCheckingBalance() {
    return checkingBalance;
  }

  public BigDecimal getSavingsBalance() {
    return savingsBalance;
  }

  public BigDecimal getLoanBalance() {
    return loanBalance;
  }

  public Instant getLastUpdated() {
    return lastUpdated;
  }

  public boolean isImbalanced(BankingAccount other) {
    return !checkingBalance.add(savingsBalance).equals(other.checkingBalance.add(other.savingsBalance));
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    BankingAccount that = (BankingAccount) o;
    return employeeId.equals(that.employeeId)
        && checkingBalance.equals(that.checkingBalance)
        && savingsBalance.equals(that.savingsBalance)
        && loanBalance.equals(that.loanBalance)
        && lastUpdated.equals(that.lastUpdated);
  }

  @Override
  public int hashCode() {
    return Objects.hash(employeeId, checkingBalance, savingsBalance, loanBalance, lastUpdated);
  }
}
