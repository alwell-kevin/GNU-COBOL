const fs = require('fs');
const path = require('path');
const { renderLedgerTable } = require('../lib/ledgerView');

function loadLedgerEntries() {
  const ledgerPath = path.resolve(__dirname, '..', '..', 'data', 'ledger.dat');
  return fs
    .readFileSync(ledgerPath, 'utf-8')
    .split('\n')
    .filter(Boolean)
    .map((line) => {
      const [rawTx, timestamp, employeeId, type, account, amount, memo] = line.split('|', 7);
      const normalizedId = rawTx.replace(/^0+/, '') || '0';
      return {
        transactionId: Number.parseInt(normalizedId, 10),
        timestamp,
        employeeId,
        type: type.toUpperCase(),
        account,
        amount: Number(amount),
        memo
      };
    });
}

describe('Banking ledger route rendering', () => {
  test('orders by transaction id desc and surfaces double-entry rows', () => {
    const entries = loadLedgerEntries();
    const table = renderLedgerTable(entries);
    const rows = table.split('\n').slice(2);

    expect(rows[0]).toMatch(/^002/);
    expect(table).toContain('DEBIT');
    expect(table).toContain('CREDIT');
    const tx2Rows = rows.filter((row) => row.startsWith('002'));
    expect(tx2Rows).toHaveLength(2);
    expect(tx2Rows[0]).toContain('CHECKING');
    expect(tx2Rows[1]).toContain('SAVINGS');
    expect(table).toContain('$250.00');
  });

  test('gracefully reports when no ledger entries exist', () => {
    const table = renderLedgerTable([]);
    expect(table).toContain('No ledger entries found.');
    expect(table.split('\n')[0]).toContain('Tx ID | Account');
  });
});
