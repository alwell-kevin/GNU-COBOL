package com.coboldemo;

import com.coboldemo.config.FlatFileProperties;
import com.coboldemo.persistence.FlatFilePersistenceAdapter;
import java.time.Clock;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

@SpringBootApplication
@EnableConfigurationProperties(FlatFileProperties.class)
public class CobolDemoApplication {
  private static final Logger log = LoggerFactory.getLogger(CobolDemoApplication.class);

  public static void main(String[] args) {
    SpringApplication.run(CobolDemoApplication.class, args);
  }

  @Bean
  public FlatFilePersistenceAdapter persistenceAdapter(FlatFileProperties props, Clock clock) {
    log.info("Bootstrapping flat-file persistence adapter for COBOL flow");
    // Domain hooks:
    // - Auth: employee credential lookups.
    // - Payroll: gross/tax/net calculations and rounding parity.
    // - Banking: account snapshots, transfer orchestration, and balance snapshots.
    // - Ledger: double-entry recording and transaction sequencing.
    // - EOD: alerts, audit, and batch settlement notifications.
    return new FlatFilePersistenceAdapter(props, clock);
  }
}
