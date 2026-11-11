// Anexo P5 (v2, 11-nov): tablas nuevas desde p5c_anexo.json (generado por fase7_p5_complementos.R).
(function () {
  const NOM = {PORTADA: 'Portada', BUSQUEDA: 'Búsqueda', LISTADO: 'Listado', PRODUCTO: 'Producto', AGREGAR_CARRO: 'Agregar al carro',
    CARRITO: 'Carrito', CHECKOUT: 'Checkout', CUENTA_SERVICIOS: 'Cuenta y servicios', OTROS: 'Otros', COMPRA: 'Compra', ABANDONO: 'Abandono'};
  const $ = s => document.querySelector(s);
  const n1 = x => (x === null || x === undefined || isNaN(x)) ? '—' : Number(x).toLocaleString('es-CL', {minimumFractionDigits: 1, maximumFractionDigits: 1});
  const n0 = x => (x === null || x === undefined) ? '—' : Number(x).toLocaleString('es-CL', {maximumFractionDigits: 0});
  const ic = (lo, hi) => (lo === null || lo === undefined) ? '' : `<span class="ic"> [${n1(lo)}–${n1(hi)}]</span>`;
  const G = g => g === 'C' ? 'Compradores' : 'No compradores';
  const put = (id, head, rows) => { const el = $(id); if (el) el.innerHTML = '<thead><tr>' + head.map(h => `<th>${h}</th>`).join('') + '</tr></thead><tbody>' + rows.map(r => '<tr>' + r.map(c => `<td>${c}</td>`).join('') + '</tr>').join('') + '</tbody>'; };
  const par = (A, key, val, fmt) => { const ks = [...new Set(A.map(x => x[key]))]; return ks.map(k => { const c = A.find(x => x[key] === k && x.grupo === 'C'), nc = A.find(x => x[key] === k && x.grupo === 'NC'); return [k, c ? fmt(c) : '—', nc ? fmt(nc) : '—']; }); };
  let J;
  function render() {
    // ¿Cómo termina la visita?
    put('#tabla-termina', ['Estado por el que pasa la visita', 'Probabilidad de terminar sin compra (por visita)', 'Por clic (datos)', 'Por clic según orden 1 (1 − B)', 'Por clic según orden 2'],
      J.termina.map(x => [x.Estado, n1(x.visitas_pct) + '%' + ic(x.visitas_lo, x.visitas_hi), n1(x.clics_pct) + '%' + ic(x.clics_lo, x.clics_hi), n1(x.orden1) + '%', n1(x.orden2) + '%']));
    // 1 clic al entrar por Producto
    put('#tabla-rebote', ['', 'Compradores', 'No compradores'], par(J.rebote_entrada, 'filtro', 'est', x => n1(x.est) + '%' + ic(x.lo, x.hi) + ` <span class="ic">(${n0(x.visitas)} visitas)</span>`));
    // caminos
    ['Todas las visitas', 'Visitas con 2 o más clics'].forEach((f, i) => {
      const rows = []; ['C', 'NC'].forEach(g => J.rutas_top.filter(x => x.filtro === f && x.grupo === g).forEach((x, k) => rows.push([k === 0 ? G(g) : '', x.ruta, n1(x.pct) + '%', n0(x.N)])));
      put(i === 0 ? '#tabla-rutas1' : '#tabla-rutas2', ['Grupo', 'Camino (estados seguidos iguales se juntan)', 'Probabilidad de que una visita siga ese camino', 'Visitas'], rows);
    });
    put('#tabla-perdida', ['Etapa', 'Compradores', 'No compradores'], par(J.perdida, 'etapa', 'est', x => n1(x.est) + '%' + ic(x.lo, x.hi) + ` <span class="ic">(${n0(x.visitas)} visitas llegan)</span>`));
    put('#tabla-salida2', ['Se va desde', 'Compradores', 'No compradores'], par(J.salida.filter(x => x.pct >= 1), 'Estado', 'pct', x => n1(x.pct) + '%' + ic(x.lo, x.hi)));
    const tr = []; ['C', 'NC'].forEach(g => J.trans.filter(x => x.grupo === g).forEach((x, k) => tr.push([k === 0 ? G(g) : '', x.cat, n1(x.pct) + '%'])));
    put('#tabla-trans', ['Grupo', 'Transición justo antes de irse', '% de las visitas sin compra con 2 o más clics'], tr);
    // carro
    put('#tabla-carro-detalle', ['Indicador', 'Compradores', 'No compradores'], par(J.carro_detalle, 'indicador', 'est', x => n1(x.est) + '%' + ic(x.lo, x.hi)));
    put('#tabla-carro-ck', ['Paso del Checkout desde el que se va', 'Compradores', 'No compradores'], par(J.carro_ck, 'cat', 'pct', x => n1(x.pct) + '%' + ic(x.lo, x.hi)));
    put('#tabla-despues', ['Después de una visita con carro sin compra…', 'Compradores', 'No compradores'], par(J.carro_despues, 'indicador', 'est', x => n1(x.est) + '%' + ic(x.lo, x.hi)));
    // checkout
    put('#tabla-ck-pasos', ['Paso', 'Probabilidad de que la visita pase por el paso (C | NC)', 'Clics (C | NC)', 'Clics por visita (C | NC)'],
      ['Login', 'Resumen y despacho', 'Dirección', 'Pago'].map(p => { const c = J.ck_pasos.find(x => x.grupo === 'C' && x.paso === p), nc = J.ck_pasos.find(x => x.grupo === 'NC' && x.paso === p);
        return [p, n1(c.pct_visitas) + '% | ' + n1(nc.pct_visitas) + '%', n0(c.clics) + ' | ' + n0(nc.clics), n1(c.clics_por_visita) + ' | ' + n1(nc.clics_por_visita)]; }));
    const ckn = {}; J.ck_nodos.forEach(x => { (ckn[x.paso] = ckn[x.paso] || new Set()).add(x.nodo); });
    $('#ck-nodos').innerHTML = 'Nodos de cada paso: ' + Object.entries(ckn).map(([p, s]) => `<b>${p}</b> = ${[...s].join(', ')}`).join(' · ') + '.';
    const rr = []; ['C', 'NC'].forEach(g => J.ck_rutas.filter(x => x.grupo === g).slice(0, 8).forEach((x, k) => rr.push([k === 0 ? G(g) : '', x.ruta.replace(/COMPRA/g, 'Compra'), n1(x.pct) + '%', n0(x.N)])));
    put('#tabla-ck-rutas', ['Grupo', 'Camino dentro del Checkout (pasos seguidos iguales se juntan)', '% de las visitas con Checkout', 'Visitas'], rr);
    // ficha repetida
    put('#tabla-pdp', ['Grupo', 'Plataforma', 'Producto → Producto es…', '% de esos pares', 'Pares'], J.pdp_rep.map(x => [G(x.grupo), x.plat, x.tipo, n1(x.pct) + '%', n0(x.N)]));
    put('#tabla-pdp-pares', ['Grupo', 'page_name del 1er clic → 2º clic (misma ficha)', '% de los pares', 'Segundos entre clics (mediana)', '% con 10 s o menos'],
      J.pdp_pares.map(x => [G(x.grupo), x.par, n1(x.pct) + '%', n0(x.seg_mediana), n1(x.pct_hasta_10s) + '%']));
    put('#tabla-pdp-ej', ['Ejemplo', 'Clic', 'Hora', 'Estado', 'page_name', 'product_id', 'Cambió variante', 'Siguiente'],
      J.pdp_ejemplos.map(x => [x.clic === 1 ? `${x.ejemplo} (${G(x.grupo)})` : '', x.clic, x.hora, x.estado, x.page_name, x.product_id || '—', x.variants_clicked ? 'sí' : '', x.siguiente]));
    // connector
    const cp = []; ['C', 'NC'].forEach(g => J.con_primero.filter(x => x.grupo === g).slice(0, 7).forEach((x, k) => cp.push([k === 0 ? G(g) : '', x.connector, n1(x.pct) + '%'])));
    put('#tabla-con-primero', ['Grupo', 'connector en el primer clic', '% de las visitas'], cp);
    const byc = {}; J.con_pantalla.forEach(x => { (byc[x.connector] = byc[x.connector] || []).push(x); });
    put('#tabla-con-pantalla', ['connector', 'Pantalla (estado) más común del clic', '2ª más común'], Object.entries(byc).map(([k, v]) => [k, `${NOM[v[0].estado]} (${n1(v[0].pct)}%)`, v[1] ? `${NOM[v[1].estado]} (${n1(v[1].pct)}%)` : '—']));
    put('#tabla-con-num1', ['Grupo', 'Plataforma', '% de visitas cuyo primer clic es la página 1 (source_visit_page_num)'], J.con_num1.map(x => [G(x.grupo), x.plat, n1(x.pct_primer_clic_es_pagina_1) + '%']));
    // ajuste
    put('#tabla-ajuste-cuart', ['Grupo', 'Grupo de actividad', 'Clientes', 'Transiciones por cliente', '% de las transiciones del grupo', '% visitas de 1 clic', 'Fuga por salida de Producto', 'Fuga por salida de Listado'],
      J.ajuste_cuartiles.map(x => [G(x.grupo), x.grupo_actividad, n0(x.clientes), x.transiciones, n1(x.pct_transiciones) + '%', n1(x.pct_visitas_1_clic) + '%', n1(x.fuga_salida_producto) + '%', n1(x.fuga_salida_listado) + '%']));
    const ng = [...new Set(J.ajuste_ngrupos.map(x => x.n_grupos))];
    put('#tabla-ajuste-ng', ['Número de grupos de actividad', 'Producto C | NC', 'Listado C | NC', 'Carrito C | NC', 'Checkout C | NC'],
      ng.map(n => [n === 1 ? '1 (= matriz principal)' : n, ...['PRODUCTO', 'LISTADO', 'CARRITO', 'CHECKOUT'].map(e => { const c = J.ajuste_ngrupos.find(x => x.n_grupos === n && x.grupo === 'C' && x.estado === e), nc = J.ajuste_ngrupos.find(x => x.n_grupos === n && x.grupo === 'NC' && x.estado === e); return n1(c.fuga_salida) + '% | ' + n1(nc.fuga_salida) + '%'; })]));
    if (window.P5tipos) window.P5tipos(J);
  }
  fetch('./p5c_anexo.json?v=2026111101').then(r => r.json()).then(j => { J = j; render(); }).catch(e => console.error('p5c', e));
})();
