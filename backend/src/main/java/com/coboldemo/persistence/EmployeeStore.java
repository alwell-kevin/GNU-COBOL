package com.coboldemo.persistence;

import com.coboldemo.persistence.model.EmployeeRecord;
import java.util.List;
import java.util.Optional;

public interface EmployeeStore {
  List<EmployeeRecord> listEmployees();

  Optional<EmployeeRecord> findByUsername(String username);

  Optional<EmployeeRecord> findById(String employeeId);
}
