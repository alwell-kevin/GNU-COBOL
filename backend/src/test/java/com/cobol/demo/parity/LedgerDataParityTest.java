package com.cobol.demo.parity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class LedgerDataParityTest {
  private static final Path DATA_DIR = Path.of("..", "..", "data").toAbsolutePath().normalize();
  private static final Path LEDGER_FILE = DATA_DIR.resolve("ledger.dat");
  private static final DateTimeFormatter COBOL_TIMESTAMP =
      DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss");

  @Test
  void everyTransactionHonorsDoubleEntryBalance() throws IOException {
    Map<Long, List<LedgerRecord>> transactions =
        Files.readAllLines(LEDGER_FILE, StandardCharsets.UTF_8).stream()
            .filter(line -> !line.isBlank())
            .map(LedgerRecord::fromLine)
            .collect(Collectors.groupingBy(r -> r.transactionId));

    transactions.forEach(
        (transactionId, entries) -> {
          assertTrue(
              entries.size() % 2 == 0,
              "Transaction " + transactionId + " should contain an even number of entries");
          assertTrue(
              entries.stream().anyMatch(r -> r.type == EntryType.DEBIT),
              "Transaction " + transactionId + " must include a debit");
          assertTrue(
              entries.stream().anyMatch(r -> r.type == EntryType.CREDIT),
              "Transaction " + transactionId + " must include a credit");

          BigDecimal debitTotal =
              entries.stream()
                  .filter(r -> r.type == EntryType.DEBIT)
                  .map(r -> r.amount)
                  .reduce(BigDecimal.ZERO, BigDecimal::add);
          BigDecimal creditTotal =
              entries.stream()
                  .filter(r -> r.type == EntryType.CREDIT)
                  .map(r -> r.amount)
                  .reduce(BigDecimal.ZERO, BigDecimal::add);

          assertEquals(
              debitTotal,
              creditTotal,
              "Transaction "
                  + transactionId
                  + " debits and credits should net to the same numeric sum");
        });
  }

  @Test
  void ledgerTimestampsAndAmountsRespectCobolFormatting() throws IOException {
    List<LedgerRecord> records =
        Files.readAllLines(LEDGER_FILE, StandardCharsets.UTF_8).stream()
            .filter(line -> !line.isBlank())
            .map(LedgerRecord::fromLine)
            .collect(Collectors.toList());

    assertTrue(records.size() >= 4, "Ledger fixture should include at least four posted entries");

    records.forEach(
        record -> {
          LocalDateTime parsed = LocalDateTime.parse(record.timestamp, COBOL_TIMESTAMP);
          assertEquals(
              record.timestamp,
              parsed.format(COBOL_TIMESTAMP),
              "Ledger timestamp should round-trip to COBOL pattern");
          assertTrue(
              record.amount.scale() <= 2,
              "Ledger amounts follow PIC 9(7)V99 precision (two fractional digits): " + record.amount);
        });
  }

  private static final class LedgerRecord {
    final long transactionId;
    final String timestamp;
    final String employeeId;
    final EntryType type;
    final String account;
    final BigDecimal amount;
    final String description;

    LedgerRecord(
        long transactionId,
        String timestamp,
        String employeeId,
        EntryType type,
        String account,
        BigDecimal amount,
        String description) {
      this.transactionId = transactionId;
      this.timestamp = timestamp;
      this.employeeId = employeeId;
      this.type = type;
      this.account = account;
      this.amount = amount;
      this.description = description;
    }

    static LedgerRecord fromLine(String line) {
      String[] parts = line.split("\\|", -1);
      if (parts.length != 7) {
        throw new IllegalArgumentException("Unexpected ledger row: " + line);
      }
      return new LedgerRecord(
          Long.parseLong(parts[0]),
          parts[1],
          parts[2],
          EntryType.valueOf(parts[3]),
          parts[4],
          new BigDecimal(parts[5]),
          parts[6]);
    }
  }

  private enum EntryType {
    DEBIT,
    CREDIT
  }
}
