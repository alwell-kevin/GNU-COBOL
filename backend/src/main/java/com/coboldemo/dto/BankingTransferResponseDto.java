package com.coboldemo.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import java.time.Instant;
import java.util.Objects;

public final class BankingTransferResponseDto {
  private final long transactionId;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
  private final Instant postedAt;
  private final BankingBalancesDto balances;

  public BankingTransferResponseDto(long transactionId, Instant postedAt, BankingBalancesDto balances) {
    this.transactionId = transactionId;
    this.postedAt = Objects.requireNonNull(postedAt);
    this.balances = Objects.requireNonNull(balances);
  }

  public long getTransactionId() {
    return transactionId;
  }

  public Instant getPostedAt() {
    return postedAt;
  }

  public BankingBalancesDto getBalances() {
    return balances;
  }
}
