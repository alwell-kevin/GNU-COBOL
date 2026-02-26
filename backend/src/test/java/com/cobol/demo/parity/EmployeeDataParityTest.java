package com.cobol.demo.parity;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;

class EmployeeDataParityTest {
  private static final Path DATA_DIR = Path.of("..", "..", "data").toAbsolutePath().normalize();
  private static final Path EMPLOYEE_FILE = DATA_DIR.resolve("employees.dat");

  @Test
  void employeeRecordsMatchCobolPayrollCsvExpectations() throws IOException {
    List<EmployeeRecord> records = Files.readAllLines(EMPLOYEE_FILE, StandardCharsets.UTF_8).stream()
        .filter(line -> !line.isBlank())
        .map(EmployeeRecord::fromLine)
        .collect(Collectors.toList());

    assertEquals(6, records.size(), "employees.dat should have six canonical rows");

    Map<String, EmployeeRecord> byUsername =
        records.stream().collect(Collectors.toMap(r -> r.username, r -> r));

    assertTrue(byUsername.containsKey("jsmith"), "Payroll must include John Smith as lead cashier");
    assertEquals(new BigDecimal("22.50"), byUsername.get("jsmith").hourlyRate);
    assertEquals(new BigDecimal("18234.75"), byUsername.get("jsmith").ytdGross);

    assertTrue(
        records.stream().allMatch(EmployeeRecord::hasTwoDecimalPrecision),
        "COBOL PIC 9(5)V99 requires two decimal places for pay fields");

    BigDecimal totalHours = records.stream().map(r -> r.hours).reduce(BigDecimal.ZERO, BigDecimal::add);
    assertEquals(new BigDecimal("330.01"), totalHours.setScale(2), "Total reported employee hours should stay constant");
  }

  private static final class EmployeeRecord {
    final String id;
    final String username;
    final String password;
    final String name;
    final BigDecimal hourlyRate;
    final BigDecimal hours;
    final BigDecimal ytdGross;

    EmployeeRecord(
        String id,
        String username,
        String password,
        String name,
        BigDecimal hourlyRate,
        BigDecimal hours,
        BigDecimal ytdGross) {
      this.id = id;
      this.username = username;
      this.password = password;
      this.name = name;
      this.hourlyRate = hourlyRate;
      this.hours = hours;
      this.ytdGross = ytdGross;
    }

    static EmployeeRecord fromLine(String line) {
      String[] parts = line.split("\\|", -1);
      if (parts.length != 7) {
        throw new IllegalArgumentException("Bad employee record: " + line);
      }
      return new EmployeeRecord(
          parts[0],
          parts[1],
          parts[2],
          parts[3],
          new BigDecimal(parts[4]),
          new BigDecimal(parts[5]),
          new BigDecimal(parts[6]));
    }

    boolean hasTwoDecimalPrecision() {
      return hourlyRate.scale() <= 2 && hours.scale() <= 2 && ytdGross.scale() <= 2;
    }
  }
}
