package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;

public class BankingTransferRequestDto {
  private String fromAccount;
  private String toAccount;
  private BigDecimal amount;
  private String memo;

  public BankingTransferRequestDto() {}

  public String getFromAccount() {
    return fromAccount;
  }

  public void setFromAccount(String fromAccount) {
    this.fromAccount = fromAccount;
  }

  public String getToAccount() {
    return toAccount;
  }

  public void setToAccount(String toAccount) {
    this.toAccount = toAccount;
  }

  public BigDecimal getAmount() {
    return amount;
  }

  public void setAmount(BigDecimal amount) {
    this.amount = NumericContract.enforceScale(amount);
  }

  public String getMemo() {
    return memo;
  }

  public void setMemo(String memo) {
    this.memo = memo;
  }
}
