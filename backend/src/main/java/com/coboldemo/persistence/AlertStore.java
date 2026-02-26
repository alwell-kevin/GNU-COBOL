package com.coboldemo.persistence;

import com.coboldemo.persistence.model.AlertRecord;
import java.util.List;

public interface AlertStore {
  List<AlertRecord> listAlerts(int limit);
}
