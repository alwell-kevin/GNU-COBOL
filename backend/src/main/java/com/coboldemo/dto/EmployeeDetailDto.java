package com.coboldemo.dto;

import com.coboldemo.model.NumericContract;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * Payload returned by `GET /api/employees/{id}`.
 */
public final class EmployeeDetailDto {
  private final String employeeId;
  private final String username;
  private final String fullName;
  private final BigDecimal hourlyRate;
  private final BigDecimal hoursCurrentPeriod;
  private final BigDecimal ytdGross;

  public EmployeeDetailDto(
      String employeeId,
      String username,
      String fullName,
      BigDecimal hourlyRate,
      BigDecimal hoursCurrentPeriod,
      BigDecimal ytdGross) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.username = Objects.requireNonNull(username);
    this.fullName = Objects.requireNonNull(fullName);
    this.hourlyRate = NumericContract.enforceScale(hourlyRate);
    this.hoursCurrentPeriod = NumericContract.enforceScale(hoursCurrentPeriod);
    this.ytdGross = NumericContract.enforceScale(ytdGross);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public String getUsername() {
    return username;
  }

  public String getFullName() {
    return fullName;
  }

  public BigDecimal getHourlyRate() {
    return hourlyRate;
  }

  public BigDecimal getHoursCurrentPeriod() {
    return hoursCurrentPeriod;
  }

  public BigDecimal getYtdGross() {
    return ytdGross;
  }
}
