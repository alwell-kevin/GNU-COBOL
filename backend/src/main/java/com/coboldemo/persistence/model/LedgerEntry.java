package com.coboldemo.persistence.model;

import java.math.BigDecimal;
import java.util.Objects;

public final class LedgerEntry {
  private final long transactionId;
  private final String timestamp;
  private final String employeeId;
  private final EntryType type;
  private final String account;
  private final BigDecimal amount;
  private final String memo;

  public LedgerEntry(
      long transactionId,
      String timestamp,
      String employeeId,
      EntryType type,
      String account,
      BigDecimal amount,
      String memo) {
    this.transactionId = transactionId;
    this.timestamp = timestamp;
    this.employeeId = employeeId;
    this.type = type;
    this.account = account;
    this.amount = amount;
    this.memo = memo;
  }

  public long getTransactionId() {
    return transactionId;
  }

  public String getTimestamp() {
    return timestamp;
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public EntryType getType() {
    return type;
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
    LedgerEntry entry = (LedgerEntry) o;
    return transactionId == entry.transactionId
        && Objects.equals(timestamp, entry.timestamp)
        && Objects.equals(employeeId, entry.employeeId)
        && type == entry.type
        && Objects.equals(account, entry.account)
        && Objects.equals(amount, entry.amount)
        && Objects.equals(memo, entry.memo);
  }

  @Override
  public int hashCode() {
    return Objects.hash(transactionId, timestamp, employeeId, type, account, amount, memo);
  }

  @Override
  public String toString() {
    return "LedgerEntry{" +
        "transactionId=" + transactionId +
        ", timestamp='" + timestamp + '\'' +
        ", employeeId='" + employeeId + '\'' +
        ", type=" + type +
        ", account='" + account + '\'' +
        ", amount=" + amount +
        ", memo='" + memo + '\'' +
        '}';
  }

  public enum EntryType {
    DEBIT,
    CREDIT
  }
}
