package com.cobol.demo.parity;

import com.coboldemo.config.FlatFileProperties;
import com.coboldemo.persistence.FlatFilePersistenceAdapter;
import com.coboldemo.persistence.model.AccountRecord;
import com.coboldemo.persistence.model.AlertRecord;
import com.coboldemo.persistence.model.EmployeeRecord;
import com.coboldemo.persistence.model.LedgerEntry;
import com.coboldemo.persistence.model.LedgerEntry.EntryType;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class FlatFileAdapterBehaviorTests {

  private FlatFilePersistenceAdapter adapter;

  @BeforeEach
  void setUp() {
    Path dataDir = locateDataDirectory();
    FlatFileProperties properties = new FlatFileProperties();
    properties.setEmployees(dataDir.resolve("employees.dat").toString());
    properties.setAccounts(dataDir.resolve("accounts.dat").toString());
    properties.setLedger(dataDir.resolve("ledger.dat").toString());
    properties.setAlerts(dataDir.resolve("alerts.dat").toString());

    Clock deterministicClock = Clock.fixed(Instant.parse("2026-02-14T12:00:00Z"), ZoneOffset.UTC);
    adapter = new FlatFilePersistenceAdapter(properties, deterministicClock);
  }

  @Test
  void employeeLookupMatchesCobolData() {
    assertFalse(adapter.listEmployees().isEmpty(), "Employee data should load for API coverage");
    EmployeeRecord john = adapter.findByUsername("jsmith").orElseThrow();
    assertEquals("John Smith", john.getFullName());
    assertEquals(new BigDecimal("22.50"), john.getHourlyRate());
    assertEquals(new BigDecimal("80.00"), john.getHours());

    EmployeeRecord caseInsensitive = adapter.findByUsername("JSMITH").orElseThrow();
    assertEquals(john.getId(), caseInsensitive.getId(), "Username lookup should ignore case like legacy auth");
  }

  @Test
  void accountStoreHonorsNegativeCheckingAndCents() {
    AccountRecord account = adapter.findByEmployeeId("1006").orElseThrow();
    assertEquals(new BigDecimal("-20.00"), account.getChecking());
    assertEquals(new BigDecimal("150.00"), account.getSavings());
    assertEquals(BigDecimal.ZERO, account.getLoan(), "Zero balances should remain zero even after parsing");
  }

  @Test
  void ledgerStoreReturnsMostRecentTransactions() {
    List<LedgerEntry> entries = adapter.listLedgerEntries(2);
    assertEquals(2, entries.size());
    for (LedgerEntry entry : entries) {
      assertEquals(2L, entry.getTransactionId(), "Limit should return only the most recent transaction id");
      assertTrue(
          entry.getType() == EntryType.DEBIT || entry.getType() == EntryType.CREDIT,
          "Ledger entry should keep legacy sign semantics");
    }
    LedgerEntry debitRow =
        entries.stream().filter(e -> e.getType() == EntryType.DEBIT).findFirst().orElseThrow();
    assertEquals(new BigDecimal("250.00"), debitRow.getAmount());
    assertEquals("TRANSFER_TO_SAVINGS", debitRow.getMemo());
  }

  @Test
  void alertStoreDefersWhenNoAlertsExist() {
    List<AlertRecord> alerts = adapter.listAlerts(5);
    assertTrue(alerts.isEmpty(), "Empty alert disk file should translate to empty API payload");
  }

  private Path locateDataDirectory() {
    Path current = Paths.get("").toAbsolutePath();
    while (current != null) {
      Path candidate = current.resolve("data");
      if (Files.exists(candidate)) {
        return candidate;
      }
      current = current.getParent();
    }
    throw new IllegalStateException("Cannot locate data directory from " + Paths.get("").toAbsolutePath());
  }
}
