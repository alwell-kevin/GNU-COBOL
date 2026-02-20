const { renderPayrollChart, calculateBarLengths } = require('../lib/chart');

describe('Payroll chart rendering parity', () => {
  test('normalizes gross/tax/net bars to COBOL scale', () => {
    const chart = renderPayrollChart({gross: 1800, tax: 396, net: 1404});
    const lines = chart.split('\n');
    const grossLine = lines.find(line => line.includes('Gross'));
    const taxLine = lines.find(line => line.includes('Tax'));
    const netLine = lines.find(line => line.includes('Net'));

    expect(grossLine).toContain('$1,800.00');
    expect(taxLine).toContain('$396.00');
    expect(netLine).toContain('$1,404.00');

    expect(grossLine).toContain('#'.repeat(40));
    expect(taxLine).toContain('#'.repeat(8));
    expect(netLine).toContain('#'.repeat(31));
  });

  test('zero values skip bar hashes while keeping the scale narrative', () => {
    const chart = renderPayrollChart({gross: 0, tax: 0, net: 0});
    expect(chart).toContain('Scale: bars are normalized');
    expect(chart.includes('#')).toBe(false);
    const lengths = calculateBarLengths({gross: 0, tax: 0, net: 0});
    expect(lengths.gross).toBe(0);
    expect(lengths.tax).toBe(0);
    expect(lengths.net).toBe(0);
  });

  test('respects a maximum left padding cap', () => {
    const chart = renderPayrollChart({gross: 1, tax: 2, net: 3, leftPadding: 250});
    const firstLine = chart.split('\n')[0];
    expect(firstLine.startsWith(' '.repeat(200))).toBe(true);
  });
});
