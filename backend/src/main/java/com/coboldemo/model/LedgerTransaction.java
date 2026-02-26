package com.coboldemo/model;

import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

/**
 * Domain model for ledger rows (double-entry, sequential tx_id, timestamp formatting).
 */
public final class LedgerTransaction {
  public enum EntryType {
    DEBIT,
    CREDIT;
  }

  private final long transactionId;
  private final Instant timestamp;
  private final String employeeId;
  private final EntryType entryType;
  private final String account;
  private final BigDecimal amount;
  private final String memo;

  public LedgerTransaction(
      long transactionId,
      Instant timestamp,
      String employeeId,
      EntryType entryType,
      String account,
      BigDecimal amount,
      String memo) {
    this.transactionId = transactionId;
    this.timestamp = Objects.requireNonNull(timestamp);
    this.employeeId = Objects.requireNonNull(employeeId);
    this.entryType = Objects.requireNonNull(entryType);
    this.account = Objects.requireNonNull(account);
    this.amount = NumericContract.enforceScale(amount);
    this.memo = Objects.requireNonNull(memo);
  }

  public long getTransactionId() {
    return transactionId;
  }

  public Instant getTimestamp() {
    return timestamp;
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public EntryType getEntryType() {
    return entryType;
  }

  public String getAccount() {
    return account;
  }

  public BigDecimal getAmount() {
    return amount;
  }

  public String getMemo() {
    return memo;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    LedgerTransaction that = (LedgerTransaction) o;
    return transactionId == that.transactionId
        && timestamp.equals(that.timestamp)
        && employeeId.equals(that.employeeId)
        && entryType == that.entryType
        && account.equals(that.account)
        && amount.equals(that.amount)
        && memo.equals(that.memo);
  }

  @Override
  public int hashCode() {
    return Objects.hash(transactionId, timestamp, employeeId, entryType, account, amount, memo);
  }
}
