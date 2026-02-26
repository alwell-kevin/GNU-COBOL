package com.coboldemo/model;

import java.time.Instant;
import java.util.Objects;

/**
 * Represents an alert/risk signal that may be written back to disk (alerts.dat) or surfaced to the UI.
 */
public final class AlertEvent {
  private final String employeeId;
  private final String severity;
  private final String message;
  private final Instant raisedAt;

  public AlertEvent(String employeeId, String severity, String message, Instant raisedAt) {
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

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    AlertEvent that = (AlertEvent) o;
    return employeeId.equals(that.employeeId)
        && severity.equals(that.severity)
        && message.equals(that.message)
        && raisedAt.equals(that.raisedAt);
  }

  @Override
  public int hashCode() {
    return Objects.hash(employeeId, severity, message, raisedAt);
  }
}
