package com.coboldemo.persistence.model;

import java.time.Instant;
import java.util.Objects;

public final class AlertRecord {
  private final Instant timestamp;
  private final String level;
  private final String message;

  public AlertRecord(Instant timestamp, String level, String message) {
    this.timestamp = timestamp;
    this.level = level;
    this.message = message;
  }

  public Instant getTimestamp() {
    return timestamp;
  }

  public String getLevel() {
    return level;
  }

  public String getMessage() {
    return message;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    AlertRecord that = (AlertRecord) o;
    return Objects.equals(timestamp, that.timestamp)
        && Objects.equals(level, that.level)
        && Objects.equals(message, that.message);
  }

  @Override
  public int hashCode() {
    return Objects.hash(timestamp, level, message);
  }

  @Override
  public String toString() {
    return "AlertRecord{" +
        "timestamp=" + timestamp +
        ", level='" + level + '\'' +
        ", message='" + message + '\'' +
        '}';
  }
}
