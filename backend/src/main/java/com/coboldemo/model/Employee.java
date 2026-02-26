package com.coboldemo/model;

import java.math.BigDecimal;
import java.util.Objects;

/**
 * Domain representation of {@code copybooks/employee_record.cpy}. Fields keep the same PIC widths/decimal
 * constraints (e.g., hourly rate is 9(3)V99) so scaling works the same as the COBOL record.
 */
public final class Employee {
  private final String id;
  private final String username;
  private final String password;
  private final String fullName;
  private final BigDecimal hourlyRate;
  private final BigDecimal hoursCurrentPeriod;
  private final BigDecimal ytdWages;

  public Employee(
      String id,
      String username,
      String password,
      String fullName,
      BigDecimal hourlyRate,
      BigDecimal hoursCurrentPeriod,
      BigDecimal ytdWages) {
    this.id = Objects.requireNonNull(id);
    this.username = Objects.requireNonNull(username);
    this.password = Objects.requireNonNull(password);
    this.fullName = Objects.requireNonNull(fullName);
    this.hourlyRate = NumericContract.enforceScale(hourlyRate);
    this.hoursCurrentPeriod = NumericContract.enforceScale(hoursCurrentPeriod);
    this.ytdWages = NumericContract.enforceScale(ytdWages);
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

  public BigDecimal getHoursCurrentPeriod() {
    return hoursCurrentPeriod;
  }

  public BigDecimal getYtdWages() {
    return ytdWages;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Employee employee = (Employee) o;
    return id.equals(employee.id)
        && username.equals(employee.username)
        && password.equals(employee.password)
        && fullName.equals(employee.fullName)
        && hourlyRate.equals(employee.hourlyRate)
        && hoursCurrentPeriod.equals(employee.hoursCurrentPeriod)
        && ytdWages.equals(employee.ytdWages);
  }

  @Override
  public int hashCode() {
    return Objects.hash(id, username, password, fullName, hourlyRate, hoursCurrentPeriod, ytdWages);
  }

  @Override
  public String toString() {
    return "Employee{"
        + "id='"
        + id
        + '\''
        + ", username='"
        + username
        + '\''
        + ", fullName='"
        + fullName
        + '\''
        + '}';
  }
}
