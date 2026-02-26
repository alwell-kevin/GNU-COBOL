package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

public final class LedgerEntryDto {
  private final long transactionId;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
  private final Instant timestamp;
  private final String employeeId;
  private final String entryType;
  private final String account;
  private final BigDecimal amount;
  private final String memo;

  public LedgerEntryDto(
      long transactionId,
      Instant timestamp,
      String employeeId,
      String entryType,
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

  public String getEntryType() {
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
}
