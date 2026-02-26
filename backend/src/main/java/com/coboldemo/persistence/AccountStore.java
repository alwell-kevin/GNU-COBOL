package com.coboldemo.persistence;

import com.coboldemo.persistence.model.AccountRecord;
import java.util.List;
import java.util.Optional;

public interface AccountStore {
  List<AccountRecord> listAccounts();

  Optional<AccountRecord> findByEmployeeId(String employeeId);
}
