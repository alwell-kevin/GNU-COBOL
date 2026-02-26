package com.coboldemo.persistence;

import com.coboldemo.persistence.model.LedgerEntry;
import java.util.List;

public interface LedgerStore {
  List<LedgerEntry> listLedgerEntries(int limit);
}
