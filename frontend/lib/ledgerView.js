const CURRENCY_FORMAT = new Intl.NumberFormat('en-US', {
  style: 'currency',
  currency: 'USD',
  minimumFractionDigits: 2,
  maximumFractionDigits: 2
});

function formatAmount(value) {
  const numeric = Number(value);
  return CURRENCY_FORMAT.format(Number.isFinite(numeric) ? numeric : 0);
}

function pad(value, length) {
  const text = String(value ?? '');
  return text.length >= length ? text : text.padEnd(length, ' ');
}

function renderLedgerTable(entries) {
  const header = 'Tx ID | Account     | Type    | Amount     | Memo';
  if (!entries || entries.length == 0) {
    return [header, '-'.repeat(header.length), 'No ledger entries found.'].join('\n');
  }
  const sorted = [...entries].sort((a, b) => {
    const idDiff = b.transactionId - a.transactionId;
    if (idDiff !== 0) {
      return idDiff;
    }
    return (a.timestamp || '').localeCompare(b.timestamp || '');
  });
  const rows = sorted.map(entry => {
    const txLabel = String(entry.transactionId).padStart(3, '0');
    const accountLabel = pad(entry.account, 11);
    const typeLabel = pad(entry.type, 6);
    const amountLabel = formatAmount(entry.amount).padStart(10);
    return `${txLabel} | ${accountLabel} | ${typeLabel} | ${amountLabel} | ${entry.memo || ''}`;
  });
  return [header, '-'.repeat(header.length), ...rows].join('\n');
}

module.exports = {
  renderLedgerTable
};
