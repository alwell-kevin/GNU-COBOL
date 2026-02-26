package com.coboldemo.persistence.model;

import java.math.BigDecimal;
import java.util.Objects;

public final class EmployeeRecord {
  private final String id;
  private final String username;
  private final String password;
  private final String fullName;
  private final BigDecimal hourlyRate;
  private final BigDecimal hours;
  private final BigDecimal ytdGross;

  public EmployeeRecord(
      String id,
      String username,
      String password,
      String fullName,
      BigDecimal hourlyRate,
      BigDecimal hours,
      BigDecimal ytdGross) {
    this.id = id;
    this.username = username;
    this.password = password;
    this.fullName = fullName;
    this.hourlyRate = hourlyRate;
    this.hours = hours;
    this.ytdGross = ytdGross;
  }

  public String getId() {
    return id;
  }

  public String getUsername() {
    return username;
  }

  public String getPassword() {
    return password;
  }

  public String getFullName() {
    return fullName;
  }

  public BigDecimal getHourlyRate() {
    return hourlyRate;
  }

  public BigDecimal getHours() {
    return hours;
  }

  public BigDecimal getYtdGross() {
    return ytdGross;
  }

  @Override
  public String toString() {
    return "EmployeeRecord{" +
        "id='" + id + '\'' +
        ", username='" + username + '\'' +
        ", fullName='" + fullName + '\'' +
        ", hourlyRate=" + hourlyRate +
        ", hours=" + hours +
        ", ytdGross=" + ytdGross +
        '}';
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    EmployeeRecord that = (EmployeeRecord) o;
    return Objects.equals(id, that.id)
        && Objects.equals(username, that.username)
        && Objects.equals(password, that.password)
        && Objects.equals(fullName, that.fullName)
        && Objects.equals(hourlyRate, that.hourlyRate)
        && Objects.equals(hours, that.hours)
        && Objects.equals(ytdGross, that.ytdGross);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, username, password, fullName, hourlyRate, hours, ytdGross);
  }
}
