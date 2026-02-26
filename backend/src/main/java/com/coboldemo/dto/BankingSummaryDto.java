package com.coboldemo.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import java.time.Instant;
import java.util.Objects;

public final class BankingSummaryDto {
  private final String employeeId;
  private final BankingBalancesDto balances;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
  private final Instant lastUpdated;

  public BankingSummaryDto(String employeeId, BankingBalancesDto balances, Instant lastUpdated) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.balances = Objects.requireNonNull(balances);
    this.lastUpdated = Objects.requireNonNull(lastUpdated);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public BankingBalancesDto getBalances() {
    return balances;
  }

  public Instant getLastUpdated() {
    return lastUpdated;
  }
}
