# =====================================================================
# fase7_p5_fugas.R  ·  Pregunta 5: ¿dónde se concentra la fuga y cómo cambia
#                      entre compradores y no compradores?
#
# QUÉ HACE (en simple):
#   1) Arma la misma cadena de P4 (orden 1, con autotransiciones, visitas
#      cortadas en su primera compra, Abandono al final de las visitas sin compra).
#   2) Calcula, para cada estado y grupo, las tres medidas de fuga:
#        - fuga por hit              = clics del estado seguidos de Abandono / clics del estado
#        - fuga por salida de estado = clics del estado seguidos de Abandono / clics que dejan el estado
#                                      (= fuga por hit / (1 - % que se queda))
#        - % del total de abandonos  = abandonos desde el estado / todos los abandonos del grupo
#      con intervalos del 95% por bootstrap por cliente (500 réplicas, igual que M.5).
#   3) Diferencia no compradores - compradores con su intervalo, y separa la brecha de la
#      fuga por hit en "se va más" y "se queda menos".
#   4) Sensibilidades (mismos clientes sorteados en cada réplica): sin rebotes, compradores
#      solo hasta su 1ª compra, solo app, solo web, semanas S1-S5, días 1-29 vs 30-31,
#      estado de entrada, comparación "más justa" y ajuste por nivel de actividad (M.4).
#   5) Checkout por pasos y embudo por visita (visitas que llegan al carro: dónde se pierden).
#
# NO MODIFICA df_falabella_estados ni df_markov (trabaja sobre una copia: d).
# OBJETOS QUE CREA: d, h, v, tr, ck, medidas, diferencias, descomp, aj, ajuste_res,
#                   checkout_pasos, ck_sal, embudo, embudo_ic, embudo_salida, recompra,
#                   producto_quien, resumen_rangos, orden_semanas, robustez
#                   (y auxiliares: VERS, MB, clientes, pa, carro_antes).
# ARCHIVOS QUE GUARDA (carpeta "resultados_p5", al lado de donde se corre el script):
#   p5_medidas.csv, p5_diferencias.csv, p5_descomposicion.csv, p5_checkout_pasos.csv,
#   p5_embudo.csv, p5_embudo_salida.csv, p5_anexo.json (todo junto, para el anexo web).
# Tiempo aproximado: 1-2 minutos.
# =====================================================================
suppressMessages({library(data.table); library(jsonlite)})
set.seed(2026)
B <- 500                                   # réplicas bootstrap
SALIDA <- "resultados_p5"; dir.create(SALIDA, showWarnings = FALSE)
RDS <- if (file.exists("df_falabella_estados.rds")) "df_falabella_estados.rds" else
  "/mnt/user-data/uploads/Script R tesis/df_falabella_estados.rds"

ESTADOS <- c("PORTADA","BUSQUEDA","LISTADO","PRODUCTO","AGREGAR_CARRO","CARRITO","CHECKOUT","CUENTA_SERVICIOS","OTROS")
NOMBRE  <- c(PORTADA="Portada", BUSQUEDA="Búsqueda", LISTADO="Listado", PRODUCTO="Producto",
             AGREGAR_CARRO="Agregar al carro", CARRITO="Carrito", CHECKOUT="Checkout",
             CUENTA_SERVICIOS="Cuenta y servicios", OTROS="Otros")

# ---------------------------------------------------------------- 1) cadena de P4
d <- as.data.table(readRDS(RDS))
sel <- d$node_type_agrupacion == "SLP_RESIDUAL" & tolower(trimws(d$page_name)) == "warrantyselection"
d[sel, `:=`(node_type_agrupacion = "WARRANTY_MODAL_VIEW", estado = "AGREGAR_CARRO")]

h <- d[, .(uid = unified_id, visit_id, seg = cliente_compro_en_ventana, dia = as.Date(date_time_start),
           node_order, t = date_time_start, estado, nodo = node_type_agrupacion, plat = platform)]
setorder(h, visit_id, dia, node_order, t)
h[, pos := seq_len(.N), by = visit_id]
h[, pc := if (any(estado == "COMPRA")) min(pos[estado == "COMPRA"]) else NA_integer_, by = visit_id]
h <- h[is.na(pc) | pos <= pc]
h[, sig := shift(estado, -1L, fill = "ABANDONO"), by = visit_id]     # siguiente estado (Abandono si es el último hit)
h[estado == "COMPRA", sig := NA_character_]

# atributos de cada visita
v <- h[, .(uid = uid[1], seg = seg[1], t0 = min(t), dia0 = min(dia), hits = .N,
           compra = any(estado == "COMPRA"), entrada = estado[1],
           plat = if (uniqueN(plat) > 1) "mixta" else plat[1],
           carro = any(estado %in% c("AGREGAR_CARRO","CARRITO")), checkout = any(estado == "CHECKOUT"),
           ultimo = estado[.N]), by = visit_id]
fp <- v[compra == TRUE, .(t_primera = min(t0)), by = uid]
v[fp, t_primera := i.t_primera, on = "uid"]
v[, hasta_1a := seg == 1 & !is.na(t_primera) & t0 <= t_primera]      # compradores: visitas hasta su 1ª compra (incluida)
v[, rebote := hits == 1]
v[, dmes := as.integer(format(dia0, "%d"))]
v[, semana := fcase(dmes <= 7, "S1", dmes <= 14, "S2", dmes <= 21, "S3", dmes <= 28, "S4", default = "S5")]
v[, entrada3 := fcase(entrada == "PORTADA", "Portada", entrada == "PRODUCTO", "Producto",
                      entrada %in% c("BUSQUEDA","LISTADO"), "Búsqueda/Listado", default = "Otra")]

# transiciones (desde -> hasta), una por hit + Abandono final
tr <- h[estado != "COMPRA", .(visit_id, uid, seg, desde = estado, hasta = sig)]

# ---------------------------------------------------------------- 2) conteos por cliente y estado
# Por cada cliente y estado: H = clics del estado, S = se queda, A = abandona, K = va a Compra
# orden "radix" = mismo orden en Windows, Mac o Linux (no depende del idioma del sistema) -> mismos IC
clientes <- list(C = sort(unique(v[seg == 1, uid]), method = "radix"), NC = sort(unique(v[seg == 0, uid]), method = "radix"))
conteos <- function(t, grupo) {
  u <- clientes[[grupo]]
  x <- t[, .(H = .N, S = sum(hasta == desde), A = sum(hasta == "ABANDONO"), K = sum(hasta == "COMPRA")), by = .(uid, desde)]
  arr <- array(0, c(length(u), length(ESTADOS), 4), dimnames = list(u, ESTADOS, c("H","S","A","K")))
  x <- x[desde %in% ESTADOS]
  i <- match(x$uid, u); j <- match(x$desde, ESTADOS)
  for (k in 1:4) arr[cbind(i, j, k)] <- x[[c("H","S","A","K")[k]]]
  arr
}
# multiplicidades bootstrap: cada fila = una réplica, cada columna = cuántas veces salió ese cliente
mult <- function(n) { M <- matrix(0L, B, n); for (b in 1:B) M[b, ] <- tabulate(sample.int(n, n, TRUE), n); M }
MB <- list(C = mult(length(clientes$C)), NC = mult(length(clientes$NC)))

medir <- function(Hs, Ss, As, Ks) {           # vectores por estado -> medidas en %
  data.table(estado = ESTADOS,
             fuga_hit = 100 * As / Hs,
             queda = 100 * Ss / Hs,
             fuga_salida = 100 * As / (Hs - Ss),
             fuga_salida_sin_compra = 100 * As / (Hs - Ss - Ks),
             pct_abandonos = 100 * As / sum(As),
             clics = Hs, salidas = Hs - Ss, abandonos = As)
}
# devuelve estimación puntual + matriz de réplicas para una versión y grupo
estimar <- function(t, grupo) {
  arr <- conteos(t, grupo)
  tot <- apply(arr, c(2, 3), sum)
  punto <- medir(tot[, "H"], tot[, "S"], tot[, "A"], tot[, "K"])
  M <- MB[[grupo]]
  R <- lapply(c("H","S","A","K"), function(k) M %*% arr[, , k])      # B x estados
  names(R) <- c("H","S","A","K")
  rep <- list(fuga_hit = 100 * R$A / R$H, queda = 100 * R$S / R$H, fuga_salida = 100 * R$A / (R$H - R$S),
              fuga_salida_sin_compra = 100 * R$A / (R$H - R$S - R$K), pct_abandonos = 100 * R$A / rowSums(R$A))
  list(punto = punto, rep = rep)
}
MEDIDAS <- c("fuga_hit","queda","fuga_salida","fuga_salida_sin_compra","pct_abandonos")
ic <- function(m) apply(m, 2, function(x) quantile(x, c(.025, .975), na.rm = TRUE))

# versiones (filtros de visitas). Para no compradores, "hasta 1ª compra" no aplica (se usan todas).
VERS <- list(
  base             = list(C = v[seg == 1, visit_id],                       NC = v[seg == 0, visit_id]),
  sin_rebotes      = list(C = v[seg == 1 & !rebote, visit_id],             NC = v[seg == 0 & !rebote, visit_id]),
  hasta_1a_compra  = list(C = v[seg == 1 & hasta_1a, visit_id],            NC = v[seg == 0, visit_id]),
  posteriores      = list(C = v[seg == 1 & !hasta_1a, visit_id],           NC = v[seg == 0, visit_id]),
  mas_justa        = list(C = v[seg == 1 & hasta_1a & !rebote, visit_id],  NC = v[seg == 0 & !rebote, visit_id]),
  solo_app         = list(C = v[seg == 1 & plat == "app", visit_id],       NC = v[seg == 0 & plat == "app", visit_id]),
  solo_web         = list(C = v[seg == 1 & plat == "web", visit_id],       NC = v[seg == 0 & plat == "web", visit_id]),
  dias_1_29        = list(C = v[seg == 1 & dmes <= 29, visit_id],          NC = v[seg == 0 & dmes <= 29, visit_id]),
  dias_30_31       = list(C = v[seg == 1 & dmes >= 30, visit_id],          NC = v[seg == 0 & dmes >= 30, visit_id]),
  entrada_portada  = list(C = v[seg == 1 & entrada3 == "Portada", visit_id],  NC = v[seg == 0 & entrada3 == "Portada", visit_id]),
  entrada_producto = list(C = v[seg == 1 & entrada3 == "Producto", visit_id], NC = v[seg == 0 & entrada3 == "Producto", visit_id]),
  entrada_busq_list= list(C = v[seg == 1 & entrada3 == "Búsqueda/Listado", visit_id], NC = v[seg == 0 & entrada3 == "Búsqueda/Listado", visit_id]))
for (s in paste0("S", 1:5)) VERS[[paste0("semana_", s)]] <- list(C = v[seg == 1 & semana == s, visit_id], NC = v[seg == 0 & semana == s, visit_id])

setkey(tr, visit_id)
medidas <- list(); diferencias <- list(); rep_guard <- list()
for (nv in names(VERS)) {
  est <- list()
  for (g in c("C","NC")) {
    e <- estimar(tr[J(VERS[[nv]][[g]]), nomatch = 0L], g)
    est[[g]] <- e
    p <- e$punto
    for (m in MEDIDAS) { q <- ic(e$rep[[m]]); p[, paste0(m, "_lo") := q[1, ]]; p[, paste0(m, "_hi") := q[2, ]] }
    p[, `:=`(version = nv, grupo = g, visitas = length(VERS[[nv]][[g]]))]
    medidas[[paste(nv, g)]] <- p
  }
  for (m in MEDIDAS) {
    dif <- est$NC$rep[[m]] - est$C$rep[[m]]; q <- ic(dif)
    diferencias[[paste(nv, m)]] <- data.table(version = nv, medida = m, estado = ESTADOS,
      dif = est$NC$punto[[m]] - est$C$punto[[m]], lo = q[1, ], hi = q[2, ])
  }
  if (nv == "base") rep_guard <- est
}
medidas <- rbindlist(medidas, fill = TRUE); diferencias <- rbindlist(diferencias)

# ---------------------------------------------------------------- 3) descomposición de la brecha en fuga por hit
# fuga_hit = (1 - queda) x fuga_salida  =>  brecha = "se va más" + "se queda menos" (reparto simétrico, exacto)
b <- merge(medidas[version == "base" & grupo == "C", .(estado, qC = queda / 100, fC = fuga_salida / 100, hC = fuga_hit)],
           medidas[version == "base" & grupo == "NC", .(estado, qN = queda / 100, fN = fuga_salida / 100, hN = fuga_hit)], by = "estado")
descomp <- b[, .(estado, brecha_hit = hN - hC,
                 se_va_mas      = 100 * ((1 - qC) + (1 - qN)) / 2 * (fN - fC),
                 se_queda_menos = 100 * (fC + fN) / 2 * (qC - qN))]
descomp[, pct_se_va_mas := 100 * se_va_mas / brecha_hit]

# ---------------------------------------------------------------- 4) ajuste por nivel de actividad (M.4), con bootstrap estratificado
ajuste_grupo <- function(g) {
  t <- tr[seg == (g == "C")]
  arr <- conteos(t, g); u <- clientes[[g]]
  # transiciones por cliente, igual que en M.4/P4: incluye Inicio -> primer estado (una por visita)
  Tc <- apply(arr[, , "H"], 1, sum) + as.numeric(v[, .N, by = uid][match(u, uid), N])
  cortes <- quantile(Tc, c(.25, .5, .75), type = 7)
  st <- findInterval(Tc, cortes, rightmost.closed = FALSE) + 1L        # cuartil de actividad (empates juntos)
  anyA <- as.numeric(apply(arr[, , "A"], 1, sum) > 0)
  calc <- function(m) {                                                # m = multiplicidad por cliente
    num <- matrix(0, 3, length(ESTADOS)); W <- rep(0, length(ESTADOS)); numP <- rep(0, length(ESTADOS)); WP <- 0
    for (k in sort(unique(st))) {                                       # un cuartil puede quedar vacío por empates
      mk <- m * (st == k); if (sum(mk) == 0) next
      Hk <- colSums(mk * arr[, , "H"]); Sk <- colSums(mk * arr[, , "S"]); Ak <- colSums(mk * arr[, , "A"])
      Nk <- colSums(mk * (arr[, , "H"] > 0))                           # clientes del estrato que salen del estado
      num[1, ] <- num[1, ] + Nk * ifelse(Hk > 0, Ak / Hk, 0)
      num[2, ] <- num[2, ] + Nk * ifelse(Hk > 0, Sk / Hk, 0); W <- W + Nk
      Wk <- sum(mk * anyA); if (sum(Ak) > 0) { numP <- numP + Wk * Ak / sum(Ak); WP <- WP + Wk }
    }
    fh <- num[1, ] / W; q <- num[2, ] / W
    rbind(fuga_hit = 100 * fh, queda = 100 * q, fuga_salida = 100 * fh / (1 - q), pct_abandonos = 100 * numP / WP)
  }
  punto <- calc(rep(1, length(u)))
  reps <- array(NA, c(B, 4, length(ESTADOS)))
  idx <- split(seq_along(u), st)
  for (b in 1:B) {
    m <- numeric(length(u))
    for (ix in idx) { s <- tabulate(ix[sample.int(length(ix), length(ix), TRUE)], length(u)); m <- m + s }
    reps[b, , ] <- calc(m)
  }
  list(punto = punto, reps = reps,
       estratos = data.table(cuartil = sort(unique(st)), clientes = as.integer(table(st)),
                             trans_min = as.numeric(tapply(Tc, st, min)), trans_max = as.numeric(tapply(Tc, st, max)),
                             pct_trans = round(100 * as.numeric(tapply(Tc, st, sum)) / sum(Tc), 1)))
}
aj <- list(C = ajuste_grupo("C"), NC = ajuste_grupo("NC"))
ajuste_res <- list()
for (g in c("C","NC")) for (i in 1:4) {
  m <- c("fuga_hit","queda","fuga_salida","pct_abandonos")[i]
  q <- apply(aj[[g]]$reps[, i, ], 2, quantile, c(.025, .975), na.rm = TRUE)
  ajuste_res[[paste(g, m)]] <- data.table(version = "ajuste_actividad", grupo = g, medida = m, estado = ESTADOS,
                                          est = aj[[g]]$punto[i, ], lo = q[1, ], hi = q[2, ])
}
ajuste_res <- rbindlist(ajuste_res)
for (i in 1:4) {
  m <- c("fuga_hit","queda","fuga_salida","pct_abandonos")[i]
  dif <- aj$NC$reps[, i, ] - aj$C$reps[, i, ]; q <- apply(dif, 2, quantile, c(.025, .975), na.rm = TRUE)
  diferencias <- rbind(diferencias, data.table(version = "ajuste_actividad", medida = m, estado = ESTADOS,
                       dif = aj$NC$punto[i, ] - aj$C$punto[i, ], lo = q[1, ], hi = q[2, ]))
}

# ---------------------------------------------------------------- 5) Checkout por pasos
paso_de <- function(nodo) fcase(nodo == "CHECKOUT_LOGIN", "1. Login",
                                nodo %in% c("CHECKOUT_ONEPAGE","ONE_PAGE_CHECKOUT","CHECKOUT"), "2. Resumen y despacho (one page)",
                                nodo %in% c("CHECKOUT_SHIPPING","USER ADDRESS PAGE","ADDRESSLIST","ADDRESSMAP","ADDADDRESSFORM"), "3. Dirección y agenda de despacho",
                                nodo %in% c("CHECKOUT_PAYMENT","PAYMENT"), "4. Pago", default = "Otro")
h[estado == "CHECKOUT", paso := paso_de(nodo)]
h[, paso_sig := shift(paso, -1L), by = visit_id]
ck <- h[estado == "CHECKOUT"]
ck[, destino := fcase(sig == "ABANDONO", "Abandona", sig == "COMPRA", "Compra",
                      sig == "CHECKOUT" & paso_sig == paso, "Repite el paso", sig == "CHECKOUT", "Otro paso del pago",
                      sig %in% c("CARRITO","AGREGAR_CARRO"), "Vuelve al carro", sig == "PRODUCTO", "Vuelve a Producto", default = "Otro estado")]
checkout_pasos <- ck[, .N, by = .(grupo = fifelse(seg == 1, "C", "NC"), paso, destino)]
checkout_pasos[, clics := sum(N), by = .(grupo, paso)][, pct := 100 * N / clics]
# IC bootstrap (por cliente) de "Abandona", "Vuelve al carro" y "Compra" por paso
ck_ic <- function(g) {
  u <- clientes[[g]]; x <- ck[seg == (g == "C")]
  agg <- dcast(x[, .N, by = .(uid, paso, destino)], uid ~ paso + destino, value.var = "N", fill = 0)
  A <- as.matrix(agg[match(u, uid), -1]); A[is.na(A)] <- 0
  R <- MB[[g]] %*% A
  out <- list()
  for (pp in unique(x$paso)) {
    cols <- colnames(R)[startsWith(colnames(R), paste0(pp, "_"))]
    tot <- rowSums(R[, cols, drop = FALSE])
    for (dd in c("Abandona","Vuelve al carro","Compra")) {
      cc <- paste0(pp, "_", dd)
      val <- if (cc %in% colnames(R)) 100 * R[, cc] / tot else rep(0, B)
      q <- quantile(val, c(.025, .975), na.rm = TRUE)
      out[[paste(pp, dd)]] <- data.table(grupo = g, paso = pp, destino = dd, lo = q[1], hi = q[2])
    }
  }
  rbindlist(out)
}
checkout_pasos <- merge(checkout_pasos, rbind(ck_ic("C"), ck_ic("NC")), by = c("grupo","paso","destino"), all.x = TRUE)
# fuga por salida de cada paso: entre los clics que dejan el Checkout, % que abandona
ck_sal <- ck[sig != "CHECKOUT", .(salidas = .N, abandona = sum(sig == "ABANDONO"), compra = sum(sig == "COMPRA")),
             by = .(grupo = fifelse(seg == 1, "C", "NC"), paso)]
ck_sal[, `:=`(fuga_salida = 100 * abandona / salidas, compra_salida = 100 * compra / salidas)]

# ---------------------------------------------------------------- 6) embudo por visita y dónde se pierde el que iba a comprar
v[, grupo := fifelse(seg == 1, "C", "NC")]
embudo <- v[, .(visitas = .N, pasa_producto = 100 * mean(visit_id %in% h[estado == "PRODUCTO", unique(visit_id)]),
                pasa_carro = 100 * mean(carro), pasa_checkout = 100 * mean(checkout), compra = 100 * mean(compra),
                carro_sin_compra = 100 * sum(carro & !compra) / sum(carro),
                checkout_sin_compra = 100 * sum(checkout & !compra) / sum(checkout)), by = grupo]
# visitas que tocaron el carro y terminaron sin compra: ¿desde qué estado se fueron?
embudo_salida <- v[carro & !compra, .N, by = .(grupo, ultimo)][, pct := 100 * N / sum(N), by = grupo][order(grupo, -pct)]
# lo mismo pero para todas las visitas sin compra (para comparar)
todas_salida <- v[compra == FALSE, .N, by = .(grupo, ultimo)][, pct := 100 * N / sum(N), by = grupo]
embudo_salida <- merge(embudo_salida, todas_salida[, .(grupo, ultimo, pct_todas = pct)], by = c("grupo","ultimo"), all.x = TRUE)
# compradores: ¿la visita con carro sin compra fue seguida de una compra más tarde en julio?
vc <- v[seg == 1]; setorder(vc, uid, t0)
compras_t <- vc[compra == TRUE, .(uid, tc = t0)]
cs <- vc[carro & !compra, .(visit_id, uid, t0)]
cs[, compra_despues := sapply(seq_len(.N), function(i) any(compras_t$tc[compras_t$uid == uid[i]] > t0[i]))]
cs[, dias_a_compra := sapply(seq_len(.N), function(i) { x <- compras_t$tc[compras_t$uid == uid[i] & compras_t$tc > t0[i]]
                                                         if (length(x)) as.numeric(difftime(min(x), t0[i], units = "days")) else NA_real_ })]
recompra <- cs[, .(visitas = .N, compra_despues = 100 * mean(compra_despues), en_7_dias = 100 * mean(!is.na(dias_a_compra) & dias_a_compra <= 7))]

# IC bootstrap (por cliente) de las cifras del embudo
emb_ic <- function(g) {
  u <- clientes[[g]]; vv <- v[grupo == g]
  agg <- vv[, .(n = .N, carro = sum(carro), carro_sc = sum(carro & !compra), chk = sum(checkout), chk_sc = sum(checkout & !compra),
                cs_prod = sum(carro & !compra & ultimo == "PRODUCTO"), cs_carr = sum(carro & !compra & ultimo == "CARRITO"),
                cs_chk = sum(carro & !compra & ultimo == "CHECKOUT")), by = uid]
  A <- as.matrix(agg[match(u, uid), -1]); A[is.na(A)] <- 0
  R <- MB[[g]] %*% A
  out <- cbind(pasa_carro = 100 * R[, "carro"] / R[, "n"], carro_sin_compra = 100 * R[, "carro_sc"] / R[, "carro"],
               checkout_sin_compra = 100 * R[, "chk_sc"] / R[, "chk"],
               sale_producto = 100 * R[, "cs_prod"] / R[, "carro_sc"], sale_carrito = 100 * R[, "cs_carr"] / R[, "carro_sc"],
               sale_checkout = 100 * R[, "cs_chk"] / R[, "carro_sc"])
  q <- apply(out, 2, quantile, c(.025, .975), na.rm = TRUE)
  data.table(grupo = g, indicador = colnames(out), lo = q[1, ], hi = q[2, ])
}
embudo_ic <- rbind(emb_ic("C"), emb_ic("NC"))

# ---------------------------------------------------------------- 6b) ¿quiénes abandonan desde Producto?
pa <- merge(h[estado == "PRODUCTO" & sig == "ABANDONO", .(visit_id, pos)],
            v[, .(visit_id, uid, grupo, rebote, entrada)], by = "visit_id")
carro_antes <- h[estado %in% c("AGREGAR_CARRO","CARRITO"), .(pos_carro = min(pos)), by = visit_id]
pa[carro_antes, pos_carro := i.pos_carro, on = "visit_id"]
pa[, tipo := fcase(rebote, "1. Rebote: entra a una ficha y se va (1 hit)",
                   !is.na(pos_carro) & pos_carro < pos, "2. Ya había pasado por el carro en la visita",
                   entrada == "PRODUCTO", "3. Entró directo a Producto y navegó un poco",
                   default = "4. Llegó desde otra etapa (Portada, Búsqueda, Listado...)")]
producto_quien <- pa[, .N, by = .(grupo, tipo)][, pct := 100 * N / sum(N), by = grupo][order(grupo, tipo)]
pq_ic <- rbindlist(lapply(c("C","NC"), function(g) {
  u <- clientes[[g]]; agg <- dcast(pa[grupo == g, .N, by = .(uid, tipo)], uid ~ tipo, value.var = "N", fill = 0)
  A <- as.matrix(agg[match(u, uid), -1]); A[is.na(A)] <- 0; R <- MB[[g]] %*% A
  q <- apply(100 * R / rowSums(R), 2, quantile, c(.025, .975))
  data.table(grupo = g, tipo = colnames(A), lo = q[1, ], hi = q[2, ])
}))
producto_quien <- merge(producto_quien, pq_ic, by = c("grupo","tipo"))

# ---------------------------------------------------------------- 7) resumen: rango de la brecha (fuga por salida) entre versiones
resumen_rangos <- diferencias[medida == "fuga_salida" & !grepl("^semana|entrada|solo_web|dias_30", version),
                              .(min = min(dif, na.rm = TRUE), max = max(dif, na.rm = TRUE), versiones = .N), by = estado]

# orden de las fugas (por salida) por semana, entre los estados del embudo de compra
embudo_estados <- c("PORTADA","BUSQUEDA","LISTADO","PRODUCTO","CARRITO","CHECKOUT")
orden_semanas <- medidas[(version == "base" | grepl("^semana", version)) & estado %in% embudo_estados,
                         .(estado, fuga_salida, orden = frank(-fuga_salida, ties.method = "min")), by = .(version, grupo)]
# ¿en cuántas versiones la brecha (por salida) es positiva con IC que no toca el cero?
robustez <- diferencias[medida == "fuga_salida", .(versiones = .N, positiva_ic = sum(lo > 0, na.rm = TRUE),
                                                    negativa_ic = sum(hi < 0, na.rm = TRUE)), by = estado]

# ---------------------------------------------------------------- 8) guardar
r1 <- function(x) { x <- copy(x); for (c in names(x)) if (is.numeric(x[[c]]) && !is.integer(x[[c]])) set(x, j = c, value = round(x[[c]], 2)); x }
fwrite(r1(medidas), file.path(SALIDA, "p5_medidas.csv"))
fwrite(r1(rbind(diferencias)), file.path(SALIDA, "p5_diferencias.csv"))
fwrite(r1(descomp), file.path(SALIDA, "p5_descomposicion.csv"))
fwrite(r1(checkout_pasos), file.path(SALIDA, "p5_checkout_pasos.csv"))
fwrite(r1(embudo), file.path(SALIDA, "p5_embudo.csv"))
fwrite(r1(embudo_salida), file.path(SALIDA, "p5_embudo_salida.csv"))
write_json(list(medidas = r1(medidas), diferencias = r1(diferencias), descomposicion = r1(descomp),
                ajuste = r1(ajuste_res), estratos = list(C = aj$C$estratos, NC = aj$NC$estratos),
                checkout_pasos = r1(checkout_pasos), checkout_salidas = r1(ck_sal),
                embudo = r1(embudo), embudo_ic = r1(embudo_ic), embudo_salida = r1(embudo_salida),
                recompra = r1(recompra), resumen_rangos = r1(resumen_rangos),
                orden_semanas = r1(orden_semanas), robustez = robustez, producto_quien = r1(producto_quien),
                versiones = data.table(version = names(VERS),
                                       visitas_C = sapply(VERS, function(x) length(x$C)), visitas_NC = sapply(VERS, function(x) length(x$NC)))),
           file.path(SALIDA, "p5_anexo.json"), auto_unbox = TRUE, digits = NA, na = "null")

# ---------------------------------------------------------------- 9) vista rápida en consola
cat("\nVisitas: C =", nrow(v[seg == 1]), " NC =", nrow(v[seg == 0]), " | hasta 1a compra:", sum(v$hasta_1a), "\n")
print(r1(medidas[version == "base", .(grupo, estado, fuga_hit, queda, fuga_salida, fuga_salida_lo, fuga_salida_hi, pct_abandonos, salidas)]))
print(r1(diferencias[version == "base" & medida %in% c("fuga_salida","fuga_hit","pct_abandonos")]))
print(r1(descomp)); print(r1(producto_quien)); print(r1(embudo)); print(r1(embudo_salida[pct > 2])); print(r1(recompra))
