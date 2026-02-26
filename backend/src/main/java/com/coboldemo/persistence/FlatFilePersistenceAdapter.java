package com.coboldemo.persistence;

import com.coboldemo.config.FlatFileProperties;
import com.coboldemo.persistence.model.AccountRecord;
import com.coboldemo.persistence.model.AlertRecord;
import com.coboldemo.persistence.model.EmployeeRecord;
import com.coboldemo.persistence.model.LedgerEntry;
import com.coboldemo.persistence.model.LedgerEntry.EntryType;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FlatFilePersistenceAdapter
    implements EmployeeStore, AccountStore, LedgerStore, AlertStore {

  private static final Logger log = LoggerFactory.getLogger(FlatFilePersistenceAdapter.class);

  private final FlatFileProperties properties;
  private final Clock clock;
  private final Path employeesPath;
  private final Path accountsPath;
  private final Path ledgerPath;
  private final Path alertsPath;

  public FlatFilePersistenceAdapter(FlatFileProperties properties, Clock clock) {
    this.properties = properties;
    this.clock = clock;
    this.employeesPath = Path.of(properties.getEmployees()).toAbsolutePath().normalize();
    this.accountsPath = Path.of(properties.getAccounts()).toAbsolutePath().normalize();
    this.ledgerPath = Path.of(properties.getLedger()).toAbsolutePath().normalize();
    this.alertsPath = Path.of(properties.getAlerts()).toAbsolutePath().normalize();
  }

  @Override
  public List<EmployeeRecord> listEmployees() {
    List<String> lines = readFile(employeesPath, "employees");
    List<EmployeeRecord> employees = new ArrayList<>();
    for (String line : lines) {
      if (line.isBlank()) {
        continue;
      }
      employees.add(parseEmployee(line));
    }
    return employees;
  }

  @Override
  public Optional<EmployeeRecord> findByUsername(String username) {
    return listEmployees().stream()
        .filter(record -> record.getUsername().equalsIgnoreCase(username))
        .findFirst();
  }

  @Override
  public Optional<EmployeeRecord> findById(String employeeId) {
    return listEmployees().stream()
        .filter(record -> record.getId().equals(employeeId))
        .findFirst();
  }

  @Override
  public List<AccountRecord> listAccounts() {
    List<String> lines = readFile(accountsPath, "accounts");
    List<AccountRecord> accounts = new ArrayList<>();
    for (String line : lines) {
      if (line.isBlank()) {
        continue;
      }
      accounts.add(parseAccount(line));
    }
    return accounts;
  }

  @Override
  public Optional<AccountRecord> findByEmployeeId(String employeeId) {
    return listAccounts().stream()
        .filter(record -> record.getEmployeeId().equals(employeeId))
        .findFirst();
  }

  @Override
  public List<LedgerEntry> listLedgerEntries(int limit) {
    List<String> lines = readFile(ledgerPath, "ledger");
    List<LedgerEntry> entries = new ArrayList<>();
    for (String line : lines) {
      if (line.isBlank()) {
        continue;
      }
      entries.add(parseLedger(line));
    }
    entries.sort(Comparator.comparingLong(LedgerEntry::getTransactionId).reversed());
    int safeLimit = Math.max(0, Math.min(limit, entries.size()));
    return entries.subList(0, safeLimit);
  }

  @Override
  public List<AlertRecord> listAlerts(int limit) {
    List<String> lines = readFile(alertsPath, "alerts");
    List<AlertRecord> alerts = new ArrayList<>();
    for (String line : lines) {
      if (line.isBlank()) {
        continue;
      }
      alerts.add(parseAlert(line));
    }
    alerts.sort(Comparator.comparing(AlertRecord::getTimestamp).reversed());
    int safeLimit = Math.max(0, Math.min(limit, alerts.size()));
    return alerts.subList(0, safeLimit);
  }

  private List<String> readFile(Path path, String label) {
    try {
      Instant check = Instant.now(clock);
      log.debug("Reading {} file from {} at {}", label, path, check);
      if (!Files.exists(path)) {
        log.warn("{} file was missing at {}", label, path);
        return List.of();
      }
      return Files.readAllLines(path, properties.charset());
    } catch (IOException ex) {
      throw new PersistenceException("Unable to read " + label + " file", ex);
    }
  }

  private EmployeeRecord parseEmployee(String line) {
    String[] parts = line.split("\\|", -1);
    if (parts.length != 7) {
      throw new PersistenceException("Unexpected employee record layout: " + line);
    }
    return new EmployeeRecord(
        parts[0].trim(),
        parts[1].trim(),
        parts[2].trim(),
        parts[3].trim(),
        parseDecimal(parts[4]),
        parseDecimal(parts[5]),
        parseDecimal(parts[6]));
  }

  private AccountRecord parseAccount(String line) {
    String[] parts = line.split("\\|", -1);
    if (parts.length != 4) {
      throw new PersistenceException("Unexpected account record layout: " + line);
    }
    return new AccountRecord(
        parts[0].trim(), parseDecimal(parts[1]), parseDecimal(parts[2]), parseDecimal(parts[3]));
  }

  private LedgerEntry parseLedger(String line) {
    String[] parts = line.split("\\|", -1);
    if (parts.length != 7) {
      throw new PersistenceException("Unexpected ledger record layout: " + line);
    }
    return new LedgerEntry(
        Long.parseLong(parts[0].trim()),
        parts[1].trim(),
        parts[2].trim(),
        EntryType.valueOf(parts[3].trim().toUpperCase()),
        parts[4].trim(),
        parseDecimal(parts[5]),
        parts[6].trim());
  }

  private AlertRecord parseAlert(String line) {
    String[] parts = line.split("\\|", -1);
    if (parts.length != 3) {
      throw new PersistenceException("Unexpected alert record layout: " + line);
    }
    return new AlertRecord(Instant.parse(parts[0].trim()), parts[1].trim(), parts[2].trim());
  }

  private BigDecimal parseDecimal(String token) {
    if (token == null || token.isBlank()) {
      return BigDecimal.ZERO;
    }
    BigDecimal parsed = new BigDecimal(token.trim());
    return parsed.setScale(properties.getScale(), properties.getRoundingMode());
  }

  public Clock getClock() {
    return clock;
  }

  public Path getEmployeesPath() {
    return employeesPath;
  }

  public static final class PersistenceException extends RuntimeException {
    public PersistenceException(String message) {
      super(message);
    }

    public PersistenceException(String message, Throwable cause) {
      super(message, cause);
    }
  }
}
