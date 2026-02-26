package com.coboldemo.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import java.time.Instant;
import java.util.Objects;

public final class AlertDto {
  private final String employeeId;
  private final String severity;
  private final String message;
  @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd HH:mm:ss")
  private final Instant raisedAt;

  public AlertDto(String employeeId, String severity, String message, Instant raisedAt) {
    this.employeeId = Objects.requireNonNull(employeeId);
    this.severity = Objects.requireNonNull(severity);
    this.message = Objects.requireNonNull(message);
    this.raisedAt = Objects.requireNonNull(raisedAt);
  }

  public String getEmployeeId() {
    return employeeId;
  }

  public String getSeverity() {
    return severity;
  }

  public String getMessage() {
    return message;
  }

  public Instant getRaisedAt() {
    return raisedAt;
  }
}
