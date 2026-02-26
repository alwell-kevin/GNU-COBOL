package com.coboldemo.dto;

import java.util.Objects;

/**
 * Login response mirrors the COBOL menu confirmation text while keeping a simple JSON contract.
 */
public final class LoginResponseDto {
  private final String employeeId;
  private final String fullName;
  private final String token;
  private final String message;

  public LoginResponseDto(String employeeId, String fullName, String token, String message) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.fullName = Objects.requireNonNull(fullName);
    this.token = Objects.requireNonNull(token);
    this.message = Objects.requireNonNull(message);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public String getFullName() {
    return fullName;
  }

  public String getToken() {
    return token;
  }

  public String getMessage() {
    return message;
  }
}
