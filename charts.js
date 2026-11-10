// charts.js: figuras del anexo P5 como SVG, a partir de p5_anexo.json (resultados de fase7_p5_fugas.R).
// window.P5charts.fig1(J), fig2(J), fig3(J) devuelven el texto SVG. Mismos datos que las tablas.
(function (root) {
  const NOM = {PORTADA: 'Portada', BUSQUEDA: 'Búsqueda', LISTADO: 'Listado', PRODUCTO: 'Producto', AGREGAR_CARRO: 'Agregar al carro',
    CARRITO: 'Carrito', CHECKOUT: 'Checkout', CUENTA_SERVICIOS: 'Cuenta y servicios', OTROS: 'Otros'};
  const C = '#2a78d6', N = '#eb6834', T1 = '#0b0b0b', T2 = '#52514e', GR = '#e6e6e3', F = 'Arial, Helvetica, sans-serif';
  const f1 = x => Number(x).toFixed(1).replace('.', ',');
  const esc = s => String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;');
  const txt = (x, y, s, o = {}) => `<text x="${x}" y="${y}" font-family="${F}" font-size="${o.fs || 13}" fill="${o.c || T2}" text-anchor="${o.a || 'start'}"${o.w ? ` font-weight="${o.w}"` : ''}>${esc(s)}</text>`;
  const svg = (w, h, body) => `<svg xmlns="http://www.w3.org/2000/svg" width="${w}" height="${h}" viewBox="0 0 ${w} ${h}"><rect width="${w}" height="${h}" fill="#ffffff"/>${body}</svg>`;
  // barra con extremo redondeado (4px) y base recta
  const bar = (x0, y, len, h, col) => { if (len <= 0.5) return `<rect x="${x0}" y="${y}" width="${Math.max(len, 1)}" height="${h}" fill="${col}"/>`;
    const r = Math.min(4, len, h / 2); return `<path d="M${x0},${y}h${len - r}a${r},${r} 0 0 1 ${r},${r}v${h - 2 * r}a${r},${r} 0 0 1 -${r},${r}h-${len - r}z" fill="${col}"/>`; };
  const leyenda = (x, y) => `<rect x="${x}" y="${y - 11}" width="14" height="14" rx="3" fill="${C}"/>${txt(x + 20, y, 'Compradores', {fs: 14, c: T1})}` +
    `<rect x="${x + 140}" y="${y - 11}" width="14" height="14" rx="3" fill="${N}"/>${txt(x + 160, y, 'No compradores', {fs: 14, c: T1})}`;
  const ejeX = (x0, w, y0, y1, max, paso, suf) => { let s = ''; for (let v = 0; v <= max + 1e-9; v += paso) { const x = x0 + w * v / max;
    s += `<line x1="${x}" y1="${y0}" x2="${x}" y2="${y1}" stroke="${GR}" stroke-width="1"/>` + txt(x, y1 + 18, v + suf, {a: 'middle', fs: 12}); } return s; };

  // Figura 1: % del total de abandonos (barras) y fuga por salida con IC (puntos)
  function fig1(J) {
    const E = Object.keys(NOM), m = (g, e) => J.medidas.find(x => x.version === 'base' && x.grupo === g && x.estado === e);
    const W = 1000, top = 92, rowH = 46, H = top + rowH * E.length + 40, lab = 150, gap = 40;
    const pw = (W - lab - gap - 30) / 2, xA = lab, xB = lab + pw + gap, yEnd = top + rowH * E.length;
    let b = leyenda(lab, 26) + txt(xA, 66, 'A. % del total de abandonos', {fs: 15, c: T1, w: 'bold'}) + txt(xB, 66, 'B. Fuga por salida de estado [IC 95%]', {fs: 15, c: T1, w: 'bold'});
    b += ejeX(xA, pw, top - 6, yEnd, 70, 10, '%') + ejeX(xB, pw, top - 6, yEnd, 50, 10, '%');
    E.forEach((e, i) => {
      const y = top + i * rowH, c = m('C', e), n = m('NC', e);
      b += txt(lab - 10, y + rowH / 2 + 4, NOM[e], {a: 'end', fs: 13, c: T1});
      [[n, N, y + 6], [c, C, y + 23]].forEach(([d, col, yy]) => {
        const len = pw * d.pct_abandonos / 70; b += bar(xA, yy, len, 15, col);
        if (d.pct_abandonos >= 5) b += txt(xA + len + 5, yy + 12, f1(d.pct_abandonos), {fs: 12});
        const xl = xB + pw * d.fuga_salida_lo / 50, xh = xB + pw * d.fuga_salida_hi / 50, xp = xB + pw * d.fuga_salida / 50, yc = yy + 7.5;
        b += `<line x1="${xl}" y1="${yc}" x2="${xh}" y2="${yc}" stroke="${col}" stroke-width="2.5"/><circle cx="${xp}" cy="${yc}" r="5.5" fill="${col}" stroke="#fff" stroke-width="2"/>`;
        if (['PRODUCTO', 'LISTADO', 'CHECKOUT', 'CUENTA_SERVICIOS', 'CARRITO'].includes(e)) b += txt(Math.max(xh, xp + 6) + 6, yc + 4, f1(d.fuga_salida), {fs: 12});
      });
    });
    return svg(W, H, b);
  }

  // Figura 2: brecha (NC - C) en fuga por salida, en 7 versiones (pequeños múltiplos)
  function fig2(J) {
    const V = [['base', 'Matriz principal'], ['sin_rebotes', 'Sin rebotes (visitas de 1 clic)'], ['hasta_1a_compra', 'Compradores solo hasta su 1ª compra'],
      ['mas_justa', 'Las dos anteriores juntas'], ['solo_app', 'Solo app'], ['dias_1_29', 'Sin días 30–31'], ['ajuste_actividad', 'Ajuste por actividad (M.4)']];
    const S = [['PRODUCTO', 'Producto'], ['LISTADO', 'Listado'], ['CARRITO', 'Carrito'], ['CHECKOUT', 'Checkout'], ['CUENTA_SERVICIOS', 'Cuenta y serv.']];
    const W = 1000, lab = 270, top = 50, rowH = 40, H = top + rowH * V.length + 60, gap = 16, pw = (W - lab - 20 - gap * (S.length - 1)) / S.length;
    const mn = -20, mx = 50, sx = (x0, v) => x0 + pw * (v - mn) / (mx - mn), yEnd = top + rowH * V.length;
    let b = '';
    V.forEach(([v, l], i) => { b += txt(lab - 12, top + i * rowH + rowH / 2 + 5, l, {a: 'end', fs: 13, c: T1}); });
    S.forEach(([e, l], k) => {
      const x0 = lab + k * (pw + gap);
      b += txt(x0, top - 18, l, {fs: 14, c: T1, w: 'bold'});
      [0, 20, 40].forEach(t => { const x = sx(x0, t); b += `<line x1="${x}" y1="${top - 4}" x2="${x}" y2="${yEnd}" stroke="${t === 0 ? T2 : GR}" stroke-width="${t === 0 ? 1.2 : 1}"/>` + txt(x, yEnd + 18, (t > 0 ? '+' : '') + t, {a: 'middle', fs: 12}); });
      V.forEach(([v], i) => {
        const d = J.diferencias.find(z => z.version === v && z.medida === 'fuga_salida' && z.estado === e), y = top + i * rowH + rowH / 2;
        b += `<line x1="${sx(x0, d.lo)}" y1="${y}" x2="${sx(x0, d.hi)}" y2="${y}" stroke="${C}" stroke-width="2.5"/>`;
        b += v === 'base' ? `<circle cx="${sx(x0, d.dif)}" cy="${y}" r="6" fill="#fff" stroke="${C}" stroke-width="2.5"/>` : `<circle cx="${sx(x0, d.dif)}" cy="${y}" r="5.5" fill="${C}" stroke="#fff" stroke-width="2"/>`;
      });
    });
    b += txt(lab + (W - lab) / 2, H - 14, 'Brecha en fuga por salida de estado (no compradores − compradores, pp). Punto blanco = matriz principal.', {a: 'middle', fs: 13, c: T1});
    return svg(W, H, b);
  }

  // Figura 3: visitas que pasaron por el carro y no compraron: ¿desde dónde se fueron? (y referencia: todas las visitas sin compra)
  function fig3(J) {
    const K = [['PRODUCTO', 'Producto'], ['CARRITO', 'Carrito'], ['CHECKOUT', 'Checkout'], ['BUSQUEDA', 'Búsqueda'], ['LISTADO', 'Listado'], ['PORTADA', 'Portada'], ['OTROS_', 'Otros estados']];
    const val = (g, k, todas) => {
      const fuente = todas ? J.medidas.filter(x => x.version === 'base' && x.grupo === g).map(x => ({u: x.estado, p: x.pct_abandonos}))
        : J.embudo_salida.filter(x => x.grupo === g).map(x => ({u: x.ultimo, p: x.pct}));
      const prin = K.slice(0, 6).map(z => z[0]);
      return k === 'OTROS_' ? fuente.filter(x => !prin.includes(x.u)).reduce((a, x) => a + x.p, 0) : (fuente.find(x => x.u === k) || {p: 0}).p;
    };
    const W = 1000, top = 92, rowH = 50, H = top + rowH * K.length + 40, lab = 130, gap = 50, pw = (W - lab - gap - 30) / 2, yEnd = top + rowH * K.length;
    let b = leyenda(lab, 26);
    [[false, 'Pasaron por el carro y no compraron'], [true, 'Todas las visitas sin compra (referencia)']].forEach(([todas, t], k) => {
      const x0 = lab + k * (pw + gap);
      b += txt(x0, 66, t, {fs: 15, c: T1, w: 'bold'}) + ejeX(x0, pw, top - 6, yEnd, 70, 10, '%');
      K.forEach(([u, l], i) => {
        const y = top + i * rowH;
        if (k === 0) b += txt(lab - 10, y + rowH / 2 + 4, l, {a: 'end', fs: 13, c: T1});
        [['NC', N, y + 7], ['C', C, y + 25]].forEach(([g, col, yy]) => {
          const v = val(g, u, todas), len = pw * v / 70; b += bar(x0, yy, len, 16, col);
          if (v >= 4) b += txt(x0 + len + 5, yy + 12.5, f1(v), {fs: 12});
        });
      });
    });
    return svg(W, H, b);
  }

  // Convierte un SVG en PNG (Blob) con escala; sirve para pegar la figura en el informe.
  function aPNG(svgTxt, escala = 2) {
    return new Promise((ok, mal) => {
      const img = new Image(); img.onload = () => { const c = document.createElement('canvas'); c.width = img.width * escala; c.height = img.height * escala;
        const x = c.getContext('2d'); x.scale(escala, escala); x.drawImage(img, 0, 0); c.toBlob(b => b ? ok(b) : mal(new Error('toBlob')), 'image/png'); };
      img.onerror = () => mal(new Error('svg')); img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svgTxt);
    });
  }
  root.P5charts = {fig1, fig2, fig3, aPNG};
})(typeof window !== 'undefined' ? window : globalThis);
