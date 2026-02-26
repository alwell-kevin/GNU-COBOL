package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import com.fasterxml.jackson.annotation.JsonFormat;
import java.math.BigDecimal;
import java.time.Instant;
import java.util.Objects;

public final class LedgerAuditDto {
  private final String employeeId;
  private final int imbalanceCount;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
  private final Instant lastCheckedAt;
  private final BigDecimal imbalanceDelta;

  public LedgerAuditDto(String employeeId, int imbalanceCount, Instant lastCheckedAt, BigDecimal imbalanceDelta) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.imbalanceCount = imbalanceCount;
    this.lastCheckedAt = Objects.requireNonNull(lastCheckedAt);
    this.imbalanceDelta = NumericContract.enforceScale(imbalanceDelta);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public int getImbalanceCount() {
    return imbalanceCount;
  }

  public Instant getLastCheckedAt() {
    return lastCheckedAt;
  }

  public BigDecimal getImbalanceDelta() {
    return imbalanceDelta;
  }
}
