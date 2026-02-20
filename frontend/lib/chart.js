const BAR_MAX_WIDTH = 40;
const LEFT_PAD_CAP = 200;
const CURRENCY_FORMAT = new Intl.NumberFormat('en-US', {
  minimumFractionDigits: 2,
  maximumFractionDigits: 2
});

function normalizeValue(value) {
  const numeric = Number(value);
  return Number.isFinite(numeric) ? numeric : 0;
}

function calculateBarLength(value, maxValue) {
  const safeValue = normalizeValue(value);
  const safeMax = Math.max(maxValue, 0);
  if (safeValue <= 0 || safeMax <= 0) {
    return 0;
  }
  const length = Math.floor((safeValue / safeMax) * BAR_MAX_WIDTH);
  return length >= 1 ? length : 1;
}

function calculateBarLengths(values) {
  const maxValue = Math.max(values.gross, values.tax, values.net, 0);
  return {
    gross: calculateBarLength(values.gross, maxValue),
    tax: calculateBarLength(values.tax, maxValue),
    net: calculateBarLength(values.net, maxValue)
  };
}

function buildBar(length) {
  return '#'.repeat(length).padEnd(BAR_MAX_WIDTH, ' ');
}

function clampLeftPadding(leftPadding) {
  const numeric = Math.max(0, Math.min(LEFT_PAD_CAP, Math.floor(normalizeValue(leftPadding))));
  return Number.isFinite(numeric) ? numeric : 0;
}

function renderPayrollChart({gross, tax, net, leftPadding = 0}) {
  const pad = ' '.repeat(clampLeftPadding(leftPadding));
  const {gross: grossLen, tax: taxLen, net: netLen} = calculateBarLengths({gross, tax, net});
  const lines = [
    'Scale: bars are normalized to the largest value in this view.',
    '------------------------------------------------------------',
    `Gross |${buildBar(grossLen)}| $${CURRENCY_FORMAT.format(normalizeValue(gross))}`,
    `Tax   |${buildBar(taxLen)}| $${CURRENCY_FORMAT.format(normalizeValue(tax))}`,
    `Net   |${buildBar(netLen)}| $${CURRENCY_FORMAT.format(normalizeValue(net))}`,
    '------------------------------------------------------------'
  ];
  return lines.map(line => pad + line).join('\n');
}

module.exports = {
  renderPayrollChart,
  calculateBarLengths
};
