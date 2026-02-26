package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

public final class EodResultDto {
  private final String employeeId;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
  private final Instant processedAt;
  private final BigDecimal interestApplied;
  private final BigDecimal feesApplied;
  private final int ledgerEntriesPosted;

  public EodResultDto(
      String employeeId,
      Instant processedAt,
      BigDecimal interestApplied,
      BigDecimal feesApplied,
      int ledgerEntriesPosted) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.processedAt = Objects.requireNonNull(processedAt);
    this.interestApplied = NumericContract.enforceScale(interestApplied);
    this.feesApplied = NumericContract.enforceScale(feesApplied);
    this.ledgerEntriesPosted = ledgerEntriesPosted;
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public Instant getProcessedAt() {
    return processedAt;
  }

  public BigDecimal getInterestApplied() {
    return interestApplied;
  }

  public BigDecimal getFeesApplied() {
    return feesApplied;
  }

  public int getLedgerEntriesPosted() {
    return ledgerEntriesPosted;
  }
}
