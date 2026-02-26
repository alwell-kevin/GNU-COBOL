package com.cobol.demo;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LegacyBehaviorTests {

    private static final BigDecimal BRACKET_LOW = new BigDecimal("500.00");
    private static final BigDecimal BRACKET_MID = new BigDecimal("1000.00");
    private static final BigDecimal BRACKET_RATE_LOW = new BigDecimal("0.10");
    private static final BigDecimal BRACKET_RATE_MID = new BigDecimal("0.15");
    private static final BigDecimal BRACKET_RATE_HIGH = new BigDecimal("0.22");
    private static final RoundingMode COBOL_ROUNDING = RoundingMode.HALF_UP;

    record Employee(int id, String username, String password, String fullName, BigDecimal hourlyRate, BigDecimal hoursPerPeriod) {}
    record PayrollResult(BigDecimal gross, BigDecimal tax, BigDecimal net, BigDecimal effectiveTaxRate) {}
    record LedgerEntry(String txId, String entryType, BigDecimal amount) {}

    @Test
    void loginShouldMatchEmployeeRecord() throws IOException {
        List<Employee> employees = loadEmployees();
        Optional<Employee> jsmith = authenticate(employees, "jsmith", "retail123");
        assertTrue(jsmith.isPresent(), "Demo credentials from README should resolve an employee record");
        assertEquals("John Smith", jsmith.get().fullName());

        Optional<Employee> invalid = authenticate(employees, "jsmith", "badpwd");
        assertFalse(invalid.isPresent(), "Incorrect password should be rejected");
    }

    @Test
    void payrollUsingCobolBrackets() throws IOException {
        Employee jsmith = authenticate(loadEmployees(), "jsmith", "retail123").orElseThrow();
        PayrollResult jPayroll = calculatePayroll(jsmith);
        assertEquals(new BigDecimal("1800.00"), jPayroll.gross());
        assertEquals(new BigDecimal("396.00"), jPayroll.tax());
        assertEquals(new BigDecimal("1404.00"), jPayroll.net());
        assertEquals(new BigDecimal("22.00"), jPayroll.effectiveTaxRate());

        Employee amartin = authenticate(loadEmployees(), "amartin", "store456").orElseThrow();
        PayrollResult aPayroll = calculatePayroll(amartin);
        assertEquals(new BigDecimal("500.00"), aPayroll.gross());
        assertEquals(new BigDecimal("50.00"), aPayroll.tax());
        assertEquals(new BigDecimal("450.00"), aPayroll.net());
        assertEquals(new BigDecimal("10.00"), aPayroll.effectiveTaxRate());
    }

    @Test
    void ledgerTransactionsMaintainDoubleEntry() throws IOException {
        List<LedgerEntry> entries = loadLedgerEntries();
        Map<String, List<LedgerEntry>> grouped = entries.stream()
                .collect(Collectors.groupingBy(LegacyBehaviorTests::getTxId));

        assertFalse(grouped.isEmpty(), "Ledger file should contain transactions for migration coverage");

        for (Map.Entry<String, List<LedgerEntry>> entry : grouped.entrySet()) {
            BigDecimal debitTotal = entry.getValue().stream()
                    .filter(e -> e.entryType().equals("DEBIT"))
                    .map(LegacyBehaviorTests::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);
            BigDecimal creditTotal = entry.getValue().stream()
                    .filter(e -> e.entryType().equals("CREDIT"))
                    .map(LegacyBehaviorTests::getAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            assertEquals(debitTotal.setScale(2, COBOL_ROUNDING), creditTotal.setScale(2, COBOL_ROUNDING),
                    () -> "Transaction " + entry.getKey() + " must balance in COBOL ledger snapshot");
            assertTrue(debitTotal.compareTo(BigDecimal.ZERO) > 0,
                    "Every recorded transaction should carry a non-zero debit amount");
            assertTrue(creditTotal.compareTo(BigDecimal.ZERO) > 0,
                    "Every recorded transaction should carry a non-zero credit amount");
        }
    }

    private static Optional<Employee> authenticate(List<Employee> employees, String username, String password) {
        return employees.stream()
                .filter(e -> e.username().equals(username) && e.password().equals(password))
                .findFirst();
    }

    private static List<Employee> loadEmployees() throws IOException {
        Path path = repoRoot().resolve("data").resolve("employees.dat");
        List<Employee> entries = new ArrayList<>();
        for (String line : Files.readAllLines(path)) {
            if (line.isBlank()) {
                continue;
            }
            String[] parts = line.split("\\|");
            if (parts.length != 7) {
                throw new IllegalStateException("Unexpected employee record: " + line);
            }
            entries.add(new Employee(
                    Integer.parseInt(parts[0].trim()),
                    parts[1].trim(),
                    parts[2].trim(),
                    parts[3].trim(),
                    new BigDecimal(parts[4].trim()),
                    new BigDecimal(parts[5].trim())
            ));
        }
        return entries;
    }

    private static PayrollResult calculatePayroll(Employee employee) {
        BigDecimal gross = employee.hourlyRate().multiply(employee.hoursPerPeriod()).setScale(2, COBOL_ROUNDING);
        BigDecimal bracketRate = selectBracket(gross);
        BigDecimal tax = gross.multiply(bracketRate).setScale(2, COBOL_ROUNDING);
        BigDecimal net = gross.subtract(tax).setScale(2, COBOL_ROUNDING);
        BigDecimal effective = gross.compareTo(BigDecimal.ZERO) > 0
                ? tax.divide(gross, 8, COBOL_ROUNDING).multiply(BigDecimal.valueOf(100))
                : BigDecimal.ZERO;
        BigDecimal effectiveTaxRate = effective.setScale(2, COBOL_ROUNDING);
        return new PayrollResult(gross, tax, net, effectiveTaxRate);
    }

    private static BigDecimal selectBracket(BigDecimal gross) {
        if (gross.compareTo(BRACKET_LOW) <= 0) {
            return BRACKET_RATE_LOW;
        }
        if (gross.compareTo(BRACKET_MID) <= 0) {
            return BRACKET_RATE_MID;
        }
        return BRACKET_RATE_HIGH;
    }

    private static List<LedgerEntry> loadLedgerEntries() throws IOException {
        Path path = repoRoot().resolve("data").resolve("ledger.dat");
        List<LedgerEntry> entries = new ArrayList<>();
        for (String line : Files.readAllLines(path)) {
            if (line.isBlank()) {
                continue;
            }
            String[] parts = line.split("\\|");
            if (parts.length != 7) {
                continue;
            }
            String txId = parts[0].trim().replaceFirst("^0+", "");
            if (txId.isEmpty()) {
                txId = "0";
            }
            entries.add(new LedgerEntry(txId, parts[3].trim().toUpperCase(), new BigDecimal(parts[5].trim())));
        }
        return entries;
    }

    private static String getTxId(LedgerEntry entry) {
        return entry.txId();
    }

    private static BigDecimal getAmount(LedgerEntry entry) {
        return entry.amount();
    }

    private static Path repoRoot() {
        Path current = Paths.get(System.getProperty("user.dir"));
        Path parent = current.getParent();
        if (parent == null) {
            throw new IllegalStateException("Cannot determine repository root from " + current);
        }
        return parent;
    }
}
