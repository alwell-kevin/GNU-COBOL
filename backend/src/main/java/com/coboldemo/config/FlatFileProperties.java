package com.coboldemo.config;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.math.RoundingMode;
import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "cobol.data")
public class FlatFileProperties {
  private String employees = "data/employees.dat";
  private String accounts = "data/accounts.dat";
  private String ledger = "data/ledger.dat";
  private String alerts = "data/alerts.dat";
  private String encoding = StandardCharsets.UTF_8.name();
  private int scale = 2;
  private RoundingMode roundingMode = RoundingMode.HALF_EVEN;

  public String getEmployees() {
    return employees;
  }

  public void setEmployees(String employees) {
    this.employees = employees;
  }

  public String getAccounts() {
    return accounts;
  }

  public void setAccounts(String accounts) {
    this.accounts = accounts;
  }

  public String getLedger() {
    return ledger;
  }

  public void setLedger(String ledger) {
    this.ledger = ledger;
  }

  public String getAlerts() {
    return alerts;
  }

  public void setAlerts(String alerts) {
    this.alerts = alerts;
  }

  public String getEncoding() {
    return encoding;
  }

  public void setEncoding(String encoding) {
    this.encoding = encoding;
  }

  public int getScale() {
    return scale;
  }

  public void setScale(int scale) {
    this.scale = scale;
  }

  public RoundingMode getRoundingMode() {
    return roundingMode;
  }

  public void setRoundingMode(RoundingMode roundingMode) {
    this.roundingMode = roundingMode;
  }

  public Charset charset() {
    return Charset.forName(encoding);
  }
}
