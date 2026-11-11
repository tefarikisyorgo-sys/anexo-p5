// Anexo P5: lee data/p5_anexo.json (generado por fase7_p5_fugas.R) y arma las tablas.
(function () {
  const NOM = {PORTADA: 'Portada', BUSQUEDA: 'Búsqueda', LISTADO: 'Listado', PRODUCTO: 'Producto', AGREGAR_CARRO: 'Agregar al carro',
    CARRITO: 'Carrito', CHECKOUT: 'Checkout', CUENTA_SERVICIOS: 'Cuenta y servicios', OTROS: 'Otros'};
  const EST = Object.keys(NOM);
  const POCOS_NC = ['CHECKOUT', 'CUENTA_SERVICIOS', 'OTROS'];
  const VER = {
    base: ['Matriz principal', 'Todas las visitas (como en P4).'],
    sin_rebotes: ['Sin rebotes', 'Se quitan las visitas de 1 clic en ambos grupos (23,6% de las visitas C y 45,1% de las NC).'],
    hasta_1a_compra: ['Compradores hasta su 1ª compra', 'Compradores: solo visitas hasta su primera compra del mes (40,8%). No compradores: todas. Compara a ambos antes de comprar.'],
    mas_justa: ['Comparación más justa', 'Las dos anteriores juntas: compradores hasta su 1ª compra y sin rebotes vs no compradores sin rebotes.'],
    posteriores: ['Compradores después de su 1ª compra', 'Compradores: solo visitas posteriores a su primera compra (59,2%).'],
    solo_app: ['Solo app', 'Visitas solo en la app (89% de las visitas).'],
    solo_web: ['Solo web', 'Visitas solo en la web. La web no registra Agregar al carro y los no compradores tienen muy pocos datos.'],
    dias_1_29: ['Sin días 30–31', 'Días 1 al 29 (sin la actividad inusual de fin de mes).'],
    dias_30_31: ['Solo días 30–31', 'Actividad inusual de fin de mes; pocos datos en no compradores.'],
    entrada_portada: ['Entra por Portada', 'Solo visitas que empiezan en Portada.'],
    entrada_producto: ['Entra por Producto', 'Solo visitas que empiezan directo en una ficha.'],
    entrada_busq_list: ['Entra por Búsqueda/Listado', 'Solo visitas que empiezan en Búsqueda o Listado.'],
    semana_S1: ['Semana 1 (1–7)', ''], semana_S2: ['Semana 2 (8–14)', ''], semana_S3: ['Semana 3 (15–21)', ''],
    semana_S4: ['Semana 4 (22–28)', ''], semana_S5: ['Semana 5 (29–31)', 'Incluye los días 30–31.'],
    ajuste_actividad: ['Ponderación por cliente (M.4)', 'Matriz dentro de 4 grupos de clientes según su actividad, promediada por número de clientes.']
  };
  const ORDEN_V = ['base', 'sin_rebotes', 'hasta_1a_compra', 'mas_justa', 'solo_app', 'dias_1_29', 'ajuste_actividad', 'posteriores',
    'entrada_portada', 'entrada_producto', 'entrada_busq_list', 'semana_S1', 'semana_S2', 'semana_S3', 'semana_S4', 'semana_S5', 'solo_web', 'dias_30_31'];
  const $ = s => document.querySelector(s);
  const n1 = x => (x === null || x === undefined || isNaN(x)) ? '—' : Number(x).toLocaleString('es-CL', {minimumFractionDigits: 1, maximumFractionDigits: 1});
  const n0 = x => Number(x).toLocaleString('es-CL', {maximumFractionDigits: 0});
  const sg = x => (x > 0 ? '+' : '') + n1(x);
  const ic = (lo, hi) => (lo === null || lo === undefined) ? '' : `<span class="ic"> [${n1(lo)}–${n1(hi)}]</span>`;
  const G = g => g === 'C' ? 'Compradores' : 'No compradores';
  let J = null;
  const st = {estados: 'C', sensMedida: 'dif', sensTipo: 'fuga_salida', ck: 'C'};

  function tabs() {
    document.querySelectorAll('.tabs').forEach(t => t.addEventListener('click', e => {
      const b = e.target.closest('button'); if (!b) return;
      t.querySelectorAll('button').forEach(x => x.classList.toggle('on', x === b));
      const k = t.dataset.target, v = b.dataset.v;
      if (k === 'estados-grupo') { st.estados = v; tablaEstados(); }
      if (k === 'sens-medida') { st.sensMedida = v; tablaSens(); }
      if (k === 'sens-tipo') { st.sensTipo = v; tablaSens(); }
      if (k === 'ck-grupo') { st.ck = v; tablaCheckout(); }
    }));
  }
  const med = (v, g, e) => J.medidas.find(x => x.version === v && x.grupo === g && x.estado === e);
  const dif = (v, m, e) => J.diferencias.find(x => x.version === v && x.medida === m && x.estado === e);
  const aj = (g, m, e) => J.ajuste.find(x => x.grupo === g && x.medida === m && x.estado === e);

  function kpis() {
    const pC = med('base', 'C', 'PRODUCTO'), pN = med('base', 'NC', 'PRODUCTO'), d = dif('base', 'fuga_salida', 'PRODUCTO');
    const eC = J.embudo.find(x => x.grupo === 'C'), eN = J.embudo.find(x => x.grupo === 'NC');
    const k = [
      ['% del total de abandonos que ocurre desde Producto', n1(pC.pct_abandonos) + '%', n1(pN.pct_abandonos) + '%', 'Dónde se concentra la fuga'],
      ['Fuga por salida de Producto', n1(pC.fuga_salida) + '%', n1(pN.fuga_salida) + '%', `Diferencia ${sg(d.dif)} pp [${n1(d.lo)}–${n1(d.hi)}]`],
      ['Visitas que pasan por el carro', n1(eC.pasa_carro) + '%', n1(eN.pasa_carro) + '%', `C: ${n1(eC.carro_sin_compra)}% de ellas termina sin compra`],
      ['De las visitas con carro sin compra, se van desde Producto', n1(J.embudo_salida.find(x => x.grupo === 'C' && x.ultimo === 'PRODUCTO').pct) + '%',
        n1(J.embudo_salida.find(x => x.grupo === 'NC' && x.ultimo === 'PRODUCTO').pct) + '%', 'Desde Checkout: menos del 7%']
    ];
    $('#kpis').innerHTML = k.map(r => `<div class="kpi"><div class="t">${r[0]}</div><div class="v"><div>${r[1]}<small>Compradores</small></div><div class="nc">${r[2]}<small>No compradores</small></div></div><div class="f">${r[3]}</div></div>`).join('');
  }

  function tablaEstados() {
    const g = st.estados;
    let h = `<thead><tr><th>Estado</th><th>Clics</th><th>Salidas</th><th>Se queda (%)</th><th>Fuga por hit (%)</th><th>Fuga por salida (%)</th><th>% del total de abandonos</th></tr></thead><tbody>`;
    EST.forEach(e => {
      const x = med('base', g, e), poco = g === 'NC' && POCOS_NC.includes(e) ? ' <span class="chip">pocos datos</span>' : '';
      h += `<tr${e === 'PRODUCTO' ? ' class="dest"' : ''}><td>${NOM[e]}${poco}</td><td>${n0(x.clics)}</td><td>${n0(x.salidas)}</td><td>${n1(x.queda)}</td>` +
        `<td>${n1(x.fuga_hit)}${ic(x.fuga_hit_lo, x.fuga_hit_hi)}</td><td>${n1(x.fuga_salida)}${ic(x.fuga_salida_lo, x.fuga_salida_hi)}</td>` +
        `<td>${n1(x.pct_abandonos)}${ic(x.pct_abandonos_lo, x.pct_abandonos_hi)}</td></tr>`;
    });
    $('#tabla-estados').innerHTML = h + '</tbody>';
  }

  function tablaBrecha() {
    let h = `<thead><tr><th>Estado</th><th>Fuga por salida C / NC</th><th>Diferencia por salida (pp)</th><th>Fuga por hit C / NC</th><th>Diferencia por hit (pp)</th><th>% abandonos C / NC</th><th>Diferencia % abandonos (pp)</th></tr></thead><tbody>`;
    EST.forEach(e => {
      const c = med('base', 'C', e), nc = med('base', 'NC', e);
      const ds = dif('base', 'fuga_salida', e), dh = dif('base', 'fuga_hit', e), dp = dif('base', 'pct_abandonos', e);
      h += `<tr${e === 'PRODUCTO' ? ' class="dest"' : ''}><td>${NOM[e]}</td><td>${n1(c.fuga_salida)} / ${n1(nc.fuga_salida)}</td><td class="${ds.dif < 0 ? 'neg' : ''}">${sg(ds.dif)}${ic(ds.lo, ds.hi)}</td>` +
        `<td>${n1(c.fuga_hit)} / ${n1(nc.fuga_hit)}</td><td class="${dh.dif < 0 ? 'neg' : ''}">${sg(dh.dif)}${ic(dh.lo, dh.hi)}</td>` +
        `<td>${n1(c.pct_abandonos)} / ${n1(nc.pct_abandonos)}</td><td>${sg(dp.dif)}${ic(dp.lo, dp.hi)}</td></tr>`;
    });
    $('#tabla-brecha').innerHTML = h + '</tbody>';
    let d = `<thead><tr><th>Estado</th><th>Se queda C / NC (%)</th><th>Diferencia por hit (pp)</th><th>= Se va más (pp)</th><th>+ Se queda menos (pp)</th><th>% de la diferencia que es “se va más”</th></tr></thead><tbody>`;
    EST.forEach(e => {
      const x = J.descomposicion.find(z => z.estado === e), c = med('base', 'C', e), nc = med('base', 'NC', e);
      d += `<tr${e === 'PRODUCTO' ? ' class="dest"' : ''}><td>${NOM[e]}</td><td>${n1(c.queda)} / ${n1(nc.queda)}</td><td>${sg(x.brecha_hit)}</td><td>${sg(x.se_va_mas)}</td><td>${sg(x.se_queda_menos)}</td><td>${n0(Math.min(100, Math.max(0, x.pct_se_va_mas)))}%</td></tr>`;
    });
    $('#tabla-descomp').innerHTML = d + '</tbody>';
    let s = `<thead><tr><th>Estado</th><th>Compradores: fuga por salida</th><th>Compradores: sin la opción Compra</th><th>No compradores</th><th>Diferencia sin la opción Compra (pp)</th></tr></thead><tbody>`;
    ['CARRITO', 'CHECKOUT'].forEach(e => {
      const c = med('base', 'C', e), nc = med('base', 'NC', e), dd = dif('base', 'fuga_salida_sin_compra', e);
      s += `<tr><td>${NOM[e]}</td><td>${n1(c.fuga_salida)}${ic(c.fuga_salida_lo, c.fuga_salida_hi)}</td><td>${n1(c.fuga_salida_sin_compra)}${ic(c.fuga_salida_sin_compra_lo, c.fuga_salida_sin_compra_hi)}</td><td>${n1(nc.fuga_salida)}${ic(nc.fuga_salida_lo, nc.fuga_salida_hi)}</td><td>${sg(dd.dif)}${ic(dd.lo, dd.hi)}</td></tr>`;
    });
    $('#tabla-sincompra').innerHTML = s + '</tbody>';
  }

  function tablaVersiones() {
    const vv = J.versiones;
    let h = `<thead><tr><th>Versión</th><th>Qué cambia</th><th>Visitas C</th><th>Visitas NC</th></tr></thead><tbody>`;
    ORDEN_V.forEach(v => {
      const x = vv.find(z => z.version === v);
      h += `<tr><td><b>${VER[v][0]}</b></td><td>${VER[v][1] || 'Solo visitas de esa semana.'}</td><td>${x ? n0(x.visitas_C) : 'todas'}</td><td>${x ? n0(x.visitas_NC) : 'todas'}</td></tr>`;
    });
    $('#tabla-versiones').innerHTML = h + '</tbody>';
  }

  function tablaSens() {
    const m = st.sensTipo, q = st.sensMedida;
    const cols = ['PRODUCTO', 'LISTADO', 'CARRITO', 'CHECKOUT', 'BUSQUEDA', 'PORTADA', 'CUENTA_SERVICIOS'];
    let h = `<thead><tr><th>Versión</th>${cols.map(e => `<th>${NOM[e]}</th>`).join('')}</tr></thead><tbody>`;
    ORDEN_V.forEach(v => {
      h += `<tr${v === 'base' ? ' class="dest"' : ''}><td>${VER[v][0]}</td>`;
      cols.forEach(e => {
        let cel = '—';
        if (q === 'dif') { const d = dif(v, m, e); if (d) cel = `<span class="${d.dif < 0 ? 'neg' : ''}">${sg(d.dif)}</span>${ic(d.lo, d.hi)}`; }
        else if (v === 'ajuste_actividad') { const a = aj(q, m, e); if (a) cel = n1(a.est) + ic(a.lo, a.hi); }
        else { const x = med(v, q, e); if (x) cel = n1(x[m]) + ic(x[m + '_lo'], x[m + '_hi']); }
        h += `<td>${cel}</td>`;
      });
      h += '</tr>';
    });
    $('#tabla-sens').innerHTML = h + '</tbody>';
    $('#nota-sens').textContent = (q === 'dif' ? 'Diferencia = no compradores − compradores (pp). ' : G(q) + ' (%). ') +
      'En “Compradores hasta / después de su 1ª compra”, los no compradores usan todas sus visitas. Solo web, días 30–31 y algunas entradas tienen pocos datos en no compradores (intervalos anchos).';
  }

  function tablaRobustez() {
    let h = `<thead><tr><th>Estado</th><th>Versiones</th><th>Diferencia positiva con IC sobre 0</th><th>Diferencia negativa con IC bajo 0</th><th>Rango de la diferencia (7 versiones principales, pp)</th></tr></thead><tbody>`;
    const R7 = ['base', 'sin_rebotes', 'hasta_1a_compra', 'mas_justa', 'solo_app', 'dias_1_29', 'ajuste_actividad'];
    EST.forEach(e => {
      const r = J.robustez.find(z => z.estado === e), ds = R7.map(v => dif(v, 'fuga_salida', e).dif);
      h += `<tr${e === 'PRODUCTO' ? ' class="dest"' : ''}><td>${NOM[e]}</td><td>${r.versiones}</td><td>${r.positiva_ic}</td><td>${r.negativa_ic}</td><td>${sg(Math.min(...ds))} a ${sg(Math.max(...ds))}</td></tr>`;
    });
    $('#tabla-robustez').innerHTML = h + '</tbody>';
  }

  function tablaSemanas() {
    const S = ['base', 'semana_S1', 'semana_S2', 'semana_S3', 'semana_S4', 'semana_S5'];
    const E6 = ['PRODUCTO', 'LISTADO', 'BUSQUEDA', 'PORTADA', 'CARRITO', 'CHECKOUT'];
    let h = `<thead><tr><th>Estado</th>${S.map(s => `<th>${s === 'base' ? 'Mes' : s.replace('semana_', '')}</th>`).join('')}</tr></thead><tbody>`;
    ['C', 'NC'].forEach(g => {
      h += `<tr class="sub"><td colspan="${S.length + 1}">${G(g)}: fuga por salida (%) y puesto</td></tr>`;
      E6.forEach(e => {
        h += `<tr><td>${NOM[e]}</td>` + S.map(s => { const o = J.orden_semanas.find(z => z.version === s && z.grupo === g && z.estado === e); return `<td>${n1(o.fuga_salida)} <span class="ic">(${o.orden})</span></td>`; }).join('') + '</tr>';
      });
    });
    $('#tabla-semanas').innerHTML = h + '</tbody>';
  }

  function tablaQuien() {
    const T = [...new Set(J.producto_quien.map(x => x.tipo))].sort();
    let h = `<thead><tr><th>Tipo de abandono desde Producto</th><th>Compradores (% de sus abandonos desde Producto)</th><th>No compradores</th></tr></thead><tbody>`;
    T.forEach(t => {
      const c = J.producto_quien.find(x => x.grupo === 'C' && x.tipo === t), nc = J.producto_quien.find(x => x.grupo === 'NC' && x.tipo === t);
      const lab = t.replace(/^\d\. /, '').replace(/.*naveg[oó] un poco.*/i, 'Entró directo a una ficha y vio más páginas').replace(/.*otra etapa.*/i, 'La visita empezó en otra página (Portada, Búsqueda, Listado…)');
      h += `<tr><td>${lab}</td><td>${n1(c.pct)}${ic(c.lo, c.hi)} <span class="ic">(${n0(c.N)})</span></td><td>${n1(nc.pct)}${ic(nc.lo, nc.hi)} <span class="ic">(${n0(nc.N)})</span></td></tr>`;
    });
    $('#tabla-quien').innerHTML = h + '</tbody>';
  }

  function tablaEmbudo() {
    const e = g => J.embudo.find(x => x.grupo === g), i = (g, k) => J.embudo_ic.find(x => x.grupo === g && x.indicador === k);
    const filas = [['Visitas', 'visitas', null, true], ['Pasan por Producto (%)', 'pasa_producto'], ['Pasan por el carro (%)', 'pasa_carro', 'pasa_carro'],
      ['Pasan por Checkout (%)', 'pasa_checkout'], ['Terminan en compra (%)', 'compra'],
      ['De las que pasan por el carro, terminan sin compra (%)', 'carro_sin_compra', 'carro_sin_compra'],
      ['De las que pasan por Checkout, terminan sin compra (%)', 'checkout_sin_compra', 'checkout_sin_compra']];
    let h = `<thead><tr><th>Indicador (por visita)</th><th>Compradores</th><th>No compradores</th></tr></thead><tbody>`;
    filas.forEach(f => {
      const cel = g => { const v = e(g)[f[1]]; if (f[3]) return n0(v); const c = f[2] && i(g, f[2]); return n1(v) + (c && g === 'C' ? ic(c.lo, c.hi) : (c && g === 'NC' && f[2] === 'pasa_carro' ? ic(c.lo, c.hi) : '')); };
      h += `<tr><td>${f[0]}</td><td>${cel('C')}</td><td>${cel('NC')}${f[1].includes('sin_compra') ? ' <span class="chip">por definición</span>' : ''}</td></tr>`;
    });
    $('#tabla-embudo').innerHTML = h + '</tbody>';
    const U = ['PRODUCTO', 'CARRITO', 'CHECKOUT', 'BUSQUEDA', 'LISTADO', 'PORTADA', 'AGREGAR_CARRO', 'CUENTA_SERVICIOS', 'OTROS'];
    const ik = {PRODUCTO: 'sale_producto', CARRITO: 'sale_carrito', CHECKOUT: 'sale_checkout'};
    let s = `<thead><tr><th>Se fue desde</th><th>Compradores: visitas con carro sin compra (%)</th><th>No compradores (%)</th></tr></thead><tbody>`;
    U.forEach(u => {
      const c = J.embudo_salida.find(x => x.grupo === 'C' && x.ultimo === u), nc = J.embudo_salida.find(x => x.grupo === 'NC' && x.ultimo === u);
      const ic1 = g => { if (!ik[u]) return ''; const z = i(g, ik[u]); return ic(z.lo, z.hi); };
      s += `<tr${u === 'PRODUCTO' ? ' class="dest"' : ''}><td>${NOM[u]}</td><td>${c ? n1(c.pct) : '0,0'}${ic1('C')}</td><td>${nc ? n1(nc.pct) : '0,0'}${ic1('NC')}</td></tr>`;
    });
    $('#tabla-salida').innerHTML = s + '</tbody>';
    const r = J.recompra[0];
    $('#recompra').innerHTML = `<b>Fin de visita no es cliente perdido:</b> de las ${n0(r.visitas)} visitas de compradores que pasaron por el carro y terminaron sin compra, en el ${n1(r.compra_despues)}% el cliente compró en una visita posterior de julio (${n1(r.en_7_dias)}% dentro de 7 días). En parte es por definición (son clientes que compraron en el mes), pero muestra que el carro sin compra es muchas veces una pausa, no una pérdida.`;
  }

  function tablaCheckout() {
    const g = st.ck, P = [...new Set(J.checkout_pasos.map(x => x.paso))].sort();
    const D = ['Compra', 'Abandona', 'Vuelve al carro', 'Vuelve a Producto', 'Otro paso del pago', 'Repite el paso', 'Otro estado'];
    let h = `<thead><tr><th>Paso</th><th>Clics</th>${D.map(d => `<th>${d} (%)</th>`).join('')}</tr></thead><tbody>`;
    P.forEach(p => {
      const xs = J.checkout_pasos.filter(x => x.grupo === g && x.paso === p); if (!xs.length) return;
      h += `<tr><td>${p}</td><td>${n0(xs[0].clics)}</td>` + D.map(d => { const x = xs.find(z => z.destino === d); if (!x) return '<td>0,0</td>'; return `<td>${n1(x.pct)}${x.lo !== null && x.lo !== undefined ? ic(x.lo, x.hi) : ''}</td>`; }).join('') + '</tr>';
    });
    $('#tabla-checkout').innerHTML = h + '</tbody>';
    const sal = J.checkout_salidas.filter(x => x.grupo === g).sort((a, b) => a.paso.localeCompare(b.paso));
    $('#nota-checkout').innerHTML = 'Fuga por salida de cada paso (entre los clics que dejan el Checkout): ' +
      sal.map(x => `${x.paso.replace(/^\d\. /, '')} ${n1(x.fuga_salida)}% abandona` + (g === 'C' ? ` y ${n1(x.compra_salida)}% compra` : '') + ` (${n0(x.salidas)} salidas)`).join(' · ') +
      (g === 'NC' ? '. Ojo: no compradores tienen solo 524 clics en Checkout; los intervalos son anchos.' : '.');
  }

  function figuras() { if (!window.P5charts) return; ['fig1'].forEach(k => { const el = document.getElementById(k); if (el) el.innerHTML = window.P5charts[k](J); }); }

  fetch('./p5_anexo.json?v=2026111101').then(r => { if (!r.ok) throw new Error(r.status); return r.json(); }).then(j => {
    J = j; $('#estado-carga').remove();
    tabs(); kpis(); figuras(); tablaEstados(); tablaBrecha(); tablaVersiones(); tablaSens(); tablaRobustez(); tablaSemanas(); tablaQuien(); tablaEmbudo(); tablaCheckout();
  }).catch(e => { const a = $('#estado-carga'); a.classList.add('error'); a.textContent = 'No se pudieron cargar los datos (' + e.message + ').'; });
})();
