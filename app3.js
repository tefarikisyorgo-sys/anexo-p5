// Anexo: tipos de visita (P4 y P5 con diferenciación de visitas). Lee p45_tipos_anexo.json (fase7_p45_tipos_visita.R).
(function () {
  const NOM = {INICIO: '(inicio)', PORTADA: 'Portada', BUSQUEDA: 'Búsqueda', LISTADO: 'Listado', PRODUCTO: 'Producto', AGREGAR_CARRO: 'Agregar al carro',
    CARRITO: 'Carrito', CHECKOUT: 'Checkout', CUENTA_SERVICIOS: 'Cuenta y servicios', OTROS: 'Otros', COMPRA: 'Compra', ABANDONO: 'Abandono'};
  const ORD = ['INICIO', 'PORTADA', 'BUSQUEDA', 'LISTADO', 'PRODUCTO', 'AGREGAR_CARRO', 'CARRITO', 'CHECKOUT', 'CUENTA_SERVICIOS', 'OTROS', 'COMPRA', 'ABANDONO'];
  const TIPOS = ['Superficial (rebote)', 'Explora (vitrinea)', 'Busca y compara', 'Gestión de cuenta', 'Compra dirigida'];
  const $ = s => document.querySelector(s);
  const n1 = x => (x === null || x === undefined || isNaN(x)) ? '—' : Number(x).toLocaleString('es-CL', {minimumFractionDigits: 1, maximumFractionDigits: 1});
  const n0 = x => (x === null || x === undefined) ? '—' : Number(x).toLocaleString('es-CL', {maximumFractionDigits: 0});
  const ic = (lo, hi) => (lo === null || lo === undefined) ? '' : `<span class="ic"> [${n1(lo)}–${n1(hi)}]</span>`;
  const G = g => g === 'C' ? 'Compradores' : 'No compradores';
  const put = (id, head, rows) => { const el = $(id); if (el) el.innerHTML = '<thead><tr>' + head.map(h => `<th>${h}</th>`).join('') + '</tr></thead><tbody>' + rows.map(r => '<tr>' + r.map(c => `<td>${c}</td>`).join('') + '</tr>').join('') + '</tbody>'; };
  let T; const st = {tipo: 'Compra dirigida', grupo: 'C'};
  function matriz() {
    const xs = T.matriz_tipo.filter(x => x.tipo === st.tipo && x.grupo === st.grupo);
    const desde = ORD.filter(e => xs.some(x => x.desde === e)), hasta = ORD.filter(e => e !== 'INICIO' && xs.some(x => x.hasta === e));
    put('#tabla-t-matriz', ['Desde \\ hacia', ...hasta.map(h => NOM[h])], desde.map(d => [NOM[d], ...hasta.map(h => { const x = xs.find(z => z.desde === d && z.hasta === h); return x ? n1(x.p) + '%' : ''; })]));
  }
  function render() {
    put('#tabla-t-reparto', ['Tipo de visita', 'Compradores', 'No compradores'], TIPOS.map(t => { const c = T.tipos_reparto.find(x => x.cat === t && x.grupo === 'C'), nc = T.tipos_reparto.find(x => x.cat === t && x.grupo === 'NC');
      return [t, n1(c.pct) + '%' + ic(c.lo, c.hi) + ` <span class="ic">(${n0(c.N)})</span>`, n1(nc.pct) + '%' + ic(nc.lo, nc.hi) + ` <span class="ic">(${n0(nc.N)})</span>`]; }));
    const K = [['entrada_mas_comun', 'Entrada más común', x => `${x.entrada_mas_comun} (${n1(x.p_entrada)}%)`], ['producto_se_queda', 'Producto: se queda'], ['producto_a_carro', 'Producto → carro'],
      ['producto_a_busqueda', 'Producto → Búsqueda'], ['producto_abandono', 'Producto → Abandono'], ['carrito_a_checkout', 'Carrito → Checkout'], ['checkout_a_compra', 'Checkout → Compra'], ['p_compra_visita', 'La visita termina en compra']];
    const rows = []; TIPOS.forEach(t => ['C', 'NC'].forEach(g => { const x = T.clave_tipo.find(z => z.tipo === t && z.grupo === g); if (!x) return;
      rows.push([g === 'C' ? t : '', G(g), n0(x.visitas), ...K.map(k => k[2] ? k[2](x) : (x[k[0]] === null ? '—' : n1(x[k[0]]) + '%'))]); }));
    put('#tabla-t-clave', ['Tipo', 'Grupo', 'Visitas', ...K.map(k => k[1])], rows);
    const E = ['PORTADA', 'BUSQUEDA', 'LISTADO', 'PRODUCTO', 'AGREGAR_CARRO', 'CARRITO', 'CHECKOUT', 'CUENTA_SERVICIOS'];
    const fr = []; TIPOS.forEach(t => ['C', 'NC'].forEach(g => fr.push([g === 'C' ? t : '', G(g), ...E.map(e => { const x = T.fs_tipo.find(z => z.tipo === t && z.grupo === g && z.estado === e); return x ? n1(x.est) + '%' + ic(x.lo, x.hi) + ` <span class="ic">(${n0(x.salidas)})</span>` : '—'; })])));
    put('#tabla-t-fuga', ['Tipo', 'Grupo', ...E.map(e => NOM[e])], fr);
    const sr = []; TIPOS.forEach(t => ['C', 'NC'].forEach(g => { const xs = T.salida_tipo.filter(z => z.tipo === t && z.grupo === g && z.pct >= 3).sort((a, b) => b.pct - a.pct); if (xs.length) sr.push([g === 'C' ? t : '', G(g), xs.map(x => `${NOM[x.cat]} ${n1(x.pct)}%`).join(' · ')]); }));
    put('#tabla-t-salida', ['Tipo', 'Grupo', 'Desde dónde se van las visitas sin compra (≥ 3%)'], sr);
    const ests = [...new Set(T.mezcla.map(x => x.estado))];
    put('#tabla-t-mezcla', ['Estado', 'Compradores', 'No compradores', 'No compradores con la mezcla de visitas de los compradores'], ests.map(e => [NOM[e], ...['Compradores', 'No compradores', 'No compradores con la mezcla de tipos de los compradores'].map(m => { const x = T.mezcla.find(z => z.estado === e && z.medida === m); return n1(x.fuga_salida) + '%' + ic(x.lo, x.hi); })]));
    const mom = [...new Set(T.momento_reparto.map(x => x.momento))];
    put('#tabla-t-momento', ['Tipo de visita', ...mom], TIPOS.map(t => [t, ...mom.map(m => { const x = T.momento_reparto.find(z => z.tipo === t && z.momento === m); return x ? n1(x.pct) + '%' : '—'; })]));
    const mf = []; TIPOS.forEach(t => ['PRODUCTO', 'LISTADO', 'CARRITO', 'CUENTA_SERVICIOS'].forEach(e => { const xs = mom.map(m => T.momento_fs.find(z => z.tipo === t && z.estado === e && z.momento === m)); if (xs.some(Boolean) && !t.startsWith('Superficial')) mf.push([t, NOM[e], ...xs.map(x => x ? n1(x.fuga_salida) + '%' : '—')]); }));
    put('#tabla-t-momento-fs', ['Tipo', 'Estado', ...mom.map(m => 'Fuga por salida: ' + m)], mf);
    const regs = [...new Set(T.reglas_cambio.map(x => x.regla))];
    put('#tabla-t-reglas', ['Regla', '% visitas que cambian de tipo (C | NC)', 'NC con mezcla de C: Producto', 'Listado', 'Carrito'], regs.map(r => { const c = T.reglas_cambio.find(x => x.regla === r && x.grupo === 'C'), nc = T.reglas_cambio.find(x => x.regla === r && x.grupo === 'NC');
      const m = e => { const x = T.reglas_mezcla.find(z => z.regla === r && z.estado === e); return n1(x.NC_mezcla_C) + '%'; }; return [r, n1(c.pct_visitas_cambian) + '% | ' + n1(nc.pct_visitas_cambian) + '%', m('PRODUCTO'), m('LISTADO'), m('CARRITO')]; }));
    const S = ['S1', 'S2', 'S3', 'S4', 'S5'], sw = [];
    TIPOS.forEach(t => ['C', 'NC'].forEach(g => sw.push([g === 'C' ? t : '', G(g), ...S.map(s => { const x = T.semanas_reparto.find(z => z.tipo === t && z.grupo === g && z.semana === s); return x ? n1(x.pct) + '%' : '—'; }),
      ...S.map(s => { const x = T.semanas_fs.find(z => z.tipo === t && z.grupo === g && z.semana === s); return x ? n1(x.fuga_salida) + '%' : '—'; })])));
    put('#tabla-t-semanas', ['Tipo', 'Grupo', ...S.map(s => '% visitas ' + s), ...S.map(s => 'Fuga Producto ' + s)], sw);
    const sel = $('#t-sel'); if (sel) { sel.innerHTML = TIPOS.flatMap(t => ['C', 'NC'].map(g => `<option value="${t}|${g}">${t} · ${G(g)}</option>`)).join(''); sel.value = st.tipo + '|' + st.grupo;
      sel.onchange = () => { [st.tipo, st.grupo] = sel.value.split('|'); matriz(); }; }
    matriz();
  }
  fetch('./p45_tipos_anexo.json?v=2026111101').then(r => r.json()).then(j => { T = j; render(); }).catch(e => console.error('p45', e));
})();
