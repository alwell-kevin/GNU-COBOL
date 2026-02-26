package com.coboldemo.config;

import java.time.Clock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;

@Configuration
public class AppConfig {
  private static final Logger log = LoggerFactory.getLogger(AppConfig.class);

  @Bean
  public Clock utcClock() {
    return Clock.systemUTC();
  }

  @Bean
  public TermDisplayProperties termDisplayProperties(Environment env) {
    int cols = parseTermSize(env.getProperty("PAYVIEW_TERM_COLS"), 132);
    int rows = parseTermSize(env.getProperty("PAYVIEW_TERM_ROWS"), 42);
    log.info("Terminal viewport initialized (cols={}, rows={})", cols, rows);
    return new TermDisplayProperties(cols, rows);
  }

  private int parseTermSize(String value, int fallback) {
    if (value == null || value.isBlank()) {
      return fallback;
    }
    try {
      int parsed = Integer.parseInt(value.trim());
      return parsed > 0 ? parsed : fallback;
    } catch (NumberFormatException ex) {
      log.warn("Invalid terminal size value '{}', falling back to {}", value, fallback);
      return fallback;
    }
  }

  public static final class TermDisplayProperties {
    private final int cols;
    private final int rows;

    public TermDisplayProperties(int cols, int rows) {
      this.cols = cols;
      this.rows = rows;
    }

    public int getCols() {
      return cols;
    }

    public int getRows() {
      return rows;
    }
  }
}
