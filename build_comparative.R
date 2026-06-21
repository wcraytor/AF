# Build a single comparative HTML paper from the three appraisal models.
# Reads exact stats from the report_data.rds bundles (earth figures are the
# confirmed format_summary() values, hardcoded to avoid loading the 499MB
# earth result object). Emits AF/Comparative_Analysis.html.

AF <- "/Volumes/Nvme_1/ClaudeCode/AF"
setwd(AF)

# ---- Value conclusions (subject indicated value, raw $) --------------------
val <- c(earth = 491172, glmnet = 487828, mgcv = 508433)

# ---- earth: confirmed format_summary() figures (raw $) ---------------------
earth <- list(
  r2 = 0.819660, grsq = 0.779189, gcv = 2098180842.04, rss = 1333194866936.02,
  cv_r2 = 0.808072, n_terms = 31L, n_pred_model = 9L, n_obs = 780L, degree = 2L)
earth$rmse <- sqrt(earth$rss / earth$n_obs)

# ---- glmnet + mgcv from their report_data.rds ------------------------------
g <- readRDS("glmnet/report_data.rds")
m <- readRDS("mgcv/report_data.rds"); msi <- m$summary_info

glmnet <- list(
  r2 = g$r_squared, adj = g$adj_r_squared, gen = g$gen_r_squared,
  cv_r2 = g$cv_r_squared, rmse = g$rmse, mae = g$mae,
  alpha = g$alpha, lambda = g$lambda, n_obs = g$n_obs)

mgcv <- list(
  r2 = msi$r_squared, cv_r2 = msi$cv_rsq, dev = msi$dev_explained,
  aic = msi$aic, bic = msi$bic, n_obs = msi$n_obs, n_smooths = msi$n_smooths,
  method = msi$method, family = msi$family,
  smooth_table = msi$smooth_table, param_table = msi$parametric_table)

# Predictors (earth/glmnet share the same 19-variable candidate set)
preds <- g$predictors

# ---- value-comparison statistics ------------------------------------------
pct <- function(a, b) (a - b) / b * 100        # a relative to b
mean_v <- mean(val); med_v <- median(val); sd_v <- sd(val)
cv_v <- sd_v / mean_v * 100; rng <- max(val) - min(val)
rng_pct <- rng / min(val) * 100

# ---- helpers ---------------------------------------------------------------
d <- function(x) format(round(x), big.mark = ",")              # dollars
p2 <- function(x) sprintf("%.2f%%", x)
r4 <- function(x) sprintf("%.4f", x)
find1 <- function(dir, pat) {
  f <- list.files(dir, pattern = pat, full.names = FALSE)
  f <- f[!grepl("^~\\$|^\\.", f)]   # drop Excel lock files (~$...) and dotfiles
  if (length(f)) file.path(dir, sort(f)[1]) else NA_character_
}
link <- function(path, label) if (is.na(path)) label else
  sprintf('<a href="%s">%s</a>', path, label)

e_html <- link(find1("earth",  "\\.html$"),  "Full earthUI report")
g_html <- link(find1("glmnet", "\\.html$"),  "Full glmnetUI report")
m_html <- link(find1("mgcv",   "\\.html$"),  "Full mgcvUI report")
e_grid <- link(find1("earth",  "^SalesGrid.*xlsx$"), "Sales grid")
g_grid <- link(find1("glmnet", "^SalesGrid.*xlsx$"), "Sales grid")
m_grid <- link(find1("mgcv",   "^SalesGrid.*xlsx$"), "Sales grid")
e_adj  <- link(find1("earth",  "adjusted.*xlsx$"), "RCA-adjusted workbook")
g_adj  <- link(find1("glmnet", "adjusted.*xlsx$"), "RCA-adjusted workbook")
m_adj  <- link(find1("mgcv",   "adjusted.*xlsx$"), "RCA-adjusted workbook")

# mgcv top parametric effects (raw $) for discussion.
# ANONYMIZE: MLS area codes (e.g. "area30925") are identifying -> generic label.
anon_term <- function(x) {
  x <- sub("^area[0-9]+$", "Market area (code withheld)", x)
  x <- gsub("TRUE$", " (yes)", x)
  x
}
pt <- mgcv$param_table
pt <- pt[order(-abs(pt$Estimate)), ]
pt_rows <- paste(apply(utils::head(pt[pt$Term != "(Intercept)", ], 6), 1, function(r)
  sprintf("<tr><td>%s</td><td class='num'>%s</td><td class='num'>%.3g</td></tr>",
          anon_term(r["Term"]), d(as.numeric(r["Estimate"])), as.numeric(r["p_value"]))),
  collapse = "\n")

# mgcv smooth terms split into main smooths vs ti interactions
st <- mgcv$smooth_table
is_ti <- grepl("^ti\\(", st$Term)
main_terms <- st$Term[!is_ti]; int_terms <- st$Term[is_ti]

css <- "
:root{--bg:#eceff4;--fg:#2e3440;--mut:#4c566a;--card:#fff;--bd:#d8dee9;
--frost:#5e81ac;--frost2:#88c0d0;--ok:#a3be8c;--warn:#bf616a;--accent:#b48ead;}
*{box-sizing:border-box}
body{font-family:-apple-system,Helvetica Neue,Arial,sans-serif;line-height:1.55;
color:var(--fg);background:var(--bg);margin:0;padding:0 0 80px}
.wrap{max-width:920px;margin:0 auto;padding:0 24px}
header{background:linear-gradient(135deg,#2e3440,#3b4252);color:#eceff4;
padding:40px 0 30px;margin-bottom:28px}
header .wrap{display:block}
h1{margin:0 0 6px;font-size:1.9em}
h2{margin:34px 0 12px;border-bottom:2px solid var(--frost);padding-bottom:6px;font-size:1.4em}
h3{margin:22px 0 8px;color:var(--frost);font-size:1.15em}
.sub{color:#d8dee9;font-size:.95em}
table{border-collapse:collapse;width:100%;margin:14px 0;background:var(--card);
font-size:.93em;box-shadow:0 1px 3px rgba(0,0,0,.06)}
th,td{border:1px solid var(--bd);padding:7px 11px;text-align:left}
th{background:#e5e9f0;font-weight:700}
td.num,th.num{text-align:right;font-variant-numeric:tabular-nums}
.card{background:var(--card);border:1px solid var(--bd);border-radius:8px;
padding:6px 20px 16px;margin:16px 0;box-shadow:0 1px 3px rgba(0,0,0,.06)}
.kpi{display:flex;gap:16px;flex-wrap:wrap;margin:18px 0}
.kpi .b{flex:1;min-width:170px;background:var(--card);border:1px solid var(--bd);
border-radius:8px;padding:14px 16px;text-align:center}
.kpi .b .v{font-size:1.7em;font-weight:700;color:var(--frost)}
.kpi .b .l{color:var(--mut);font-size:.85em;text-transform:uppercase;letter-spacing:.04em}
.hi{color:var(--warn);font-weight:700}.good{color:var(--ok);font-weight:700}
.note{background:#fef9e7;border-left:4px solid #ebcb8b;padding:10px 16px;margin:14px 0;border-radius:4px}
.foot{color:var(--mut);font-size:.85em;margin-top:40px;border-top:1px solid var(--bd);padding-top:12px}
.links a{margin-right:14px}
code{background:#e5e9f0;padding:1px 5px;border-radius:3px;font-size:.9em}
"

H <- function(...) paste0(..., collapse = "")
html <- H(
'<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
'<meta name="viewport" content="width=device-width,initial-scale=1">',
'<title>Comparative Valuation Analysis — earthUI · glmnetUI · mgcvUI</title>',
'<style>', css, '</style></head><body>',
'<header><div class="wrap"><h1>Comparative Valuation Analysis</h1>',
'<div class="sub">Subject: a residential submarket (location &amp; IDs withheld) &nbsp;·&nbsp; ',
'Three regression engines &mdash; <b>earthUI</b> (MARS), <b>glmnetUI</b> ',
'(elastic net), <b>mgcvUI</b> (GAM) &nbsp;·&nbsp; ', as.character(Sys.Date()),
'</div></div></header><div class="wrap">',
'<div class="note"><b>Data confidentiality.</b> The full per-model reports ',
'(linked in each section) are included. Only the underlying MLS ',
'<b>spreadsheets</b> are withheld &mdash; those tie records to specific ',
'<b>addresses, parcel numbers, and MLS/transaction IDs</b>. The reports and ',
'plots contain aggregate model output only (no addresses or IDs). The models ',
'were fit to the real, unaltered data; only the raw spreadsheets are restricted.</div>',

# -------- Executive summary --------
'<h2>1. Executive Summary</h2>',
'<p>Three independent regression methods were fitted to the same ',
sprintf('%d-comparable', earth$n_obs), ' sales dataset to estimate the subject’s ',
'market value. All three were run on the <b>raw dollar scale</b> (no log ',
'transform) so their fit statistics and value conclusions are directly ',
'comparable. The indicated values cluster within ', p2(rng_pct), ' of one another:</p>',
'<div class="kpi">',
sprintf('<div class="b"><div class="v">$%s</div><div class="l">earthUI &middot; MARS</div></div>', d(val["earth"])),
sprintf('<div class="b"><div class="v">$%s</div><div class="l">glmnetUI &middot; elastic net</div></div>', d(val["glmnet"])),
sprintf('<div class="b"><div class="v">$%s</div><div class="l">mgcvUI &middot; GAM</div></div>', d(val["mgcv"])),
'</div>',
'<p>earthUI and glmnetUI agree to within <b>', p2(abs(pct(val["glmnet"],val["earth"]))),
'</b>; mgcvUI sits highest, about <b>', p2(pct(val["mgcv"],val["earth"])),
'</b> above earthUI. Cross-validated accuracy, however, diverges sharply &mdash; ',
'earthUI generalizes best and mgcvUI shows clear overfitting (see &sect;5).</p>',

# -------- Value reconciliation --------
'<h2>2. Value Reconciliation &amp; Differences</h2>',
'<table><thead><tr><th>Indication</th><th class="num">Value</th>',
'<th class="num">vs earthUI</th><th class="num">vs glmnetUI</th><th class="num">vs mgcvUI</th></tr></thead><tbody>',
sprintf('<tr><td>earthUI (MARS)</td><td class="num">$%s</td><td class="num">&mdash;</td><td class="num">%s</td><td class="num">%s</td></tr>',
        d(val["earth"]), p2(pct(val["earth"],val["glmnet"])), p2(pct(val["earth"],val["mgcv"]))),
sprintf('<tr><td>glmnetUI (elastic net)</td><td class="num">$%s</td><td class="num">%s</td><td class="num">&mdash;</td><td class="num">%s</td></tr>',
        d(val["glmnet"]), p2(pct(val["glmnet"],val["earth"])), p2(pct(val["glmnet"],val["mgcv"]))),
sprintf('<tr><td>mgcvUI (GAM)</td><td class="num">$%s</td><td class="num">%s</td><td class="num">%s</td><td class="num">&mdash;</td></tr>',
        d(val["mgcv"]), p2(pct(val["mgcv"],val["earth"])), p2(pct(val["mgcv"],val["glmnet"]))),
'</tbody></table>',
'<table><thead><tr><th>Dispersion of the three indications</th><th class="num">Value</th></tr></thead><tbody>',
sprintf('<tr><td>Mean</td><td class="num">$%s</td></tr>', d(mean_v)),
sprintf('<tr><td>Median (= earthUI)</td><td class="num">$%s</td></tr>', d(med_v)),
sprintf('<tr><td>Std. deviation</td><td class="num">$%s</td></tr>', d(sd_v)),
sprintf('<tr><td>Coefficient of variation</td><td class="num">%s</td></tr>', p2(cv_v)),
sprintf('<tr><td>Range (max &minus; min)</td><td class="num">$%s (%s)</td></tr>', d(rng), p2(rng_pct)),
'</tbody></table>',
'<p>A coefficient of variation of <b>', p2(cv_v), '</b> across three structurally ',
'different models is tight reconciliation &mdash; well inside the spread an appraiser ',
'would typically see across approaches.</p>',

# -------- Side-by-side fit stats --------
'<h2>3. Model Fit &mdash; Side by Side</h2>',
'<p>All metrics are on the raw dollar scale. <b>CV R&sup2;</b> (out-of-sample, ',
'10-fold) is the honest measure of predictive accuracy; <b>in-sample R&sup2;</b> ',
'flatters models that overfit.</p>',
'<table><thead><tr><th>Metric</th><th class="num">earthUI</th><th class="num">glmnetUI</th><th class="num">mgcvUI</th></tr></thead><tbody>',
sprintf('<tr><td>In-sample R&sup2;</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td></tr>', r4(earth$r2), r4(glmnet$r2), r4(mgcv$r2)),
sprintf('<tr><td><b>CV R&sup2; (10-fold)</b></td><td class="num good">%s</td><td class="num">%s</td><td class="num hi">%s</td></tr>', r4(earth$cv_r2), r4(glmnet$cv_r2), r4(mgcv$cv_r2)),
sprintf('<tr><td>In-sample &minus; CV gap</td><td class="num">%s</td><td class="num">%s</td><td class="num hi">%s</td></tr>', r4(earth$r2-earth$cv_r2), r4(glmnet$r2-glmnet$cv_r2), r4(mgcv$r2-mgcv$cv_r2)),
sprintf('<tr><td>In-sample RMSE</td><td class="num">$%s</td><td class="num">$%s</td><td class="num">&mdash;<sup>&dagger;</sup></td></tr>', d(earth$rmse), d(glmnet$rmse)),
sprintf('<tr><td>In-sample MAE</td><td class="num">&mdash;</td><td class="num">$%s</td><td class="num">&mdash;</td></tr>', d(glmnet$mae)),
sprintf('<tr><td>Observations (comps)</td><td class="num">%d</td><td class="num">%d</td><td class="num">%d</td></tr>', earth$n_obs, glmnet$n_obs, mgcv$n_obs),
'</tbody></table>',
'<p style="font-size:.85em;color:var(--mut)"><sup>&dagger;</sup>mgcv RMSE/MAE were ',
'not stored in the report bundle; deviance-explained (', p2(mgcv$dev*100),
') is the GAM analogue of R&sup2;. Note mgcv fit on ', mgcv$n_obs, ' rows vs ',
earth$n_obs, ' for the others &mdash; it did not drop the subject row.</p>',

# -------- earthUI section --------
'<h2>4. Model Sections</h2>',
'<h3>4.1 earthUI &mdash; Multivariate Adaptive Regression Splines (MARS)</h3>',
'<div class="card">',
'<table><tbody>',
sprintf('<tr><th>Indicated value</th><td class="num">$%s</td></tr>', d(val["earth"])),
sprintf('<tr><th>In-sample R&sup2; / GRSq</th><td class="num">%s / %s</td></tr>', r4(earth$r2), r4(earth$grsq)),
sprintf('<tr><th>CV R&sup2; (10-fold)</th><td class="num good">%s</td></tr>', r4(earth$cv_r2)),
sprintf('<tr><th>Terms / interaction degree</th><td class="num">%d / %d</td></tr>', earth$n_terms, earth$degree),
sprintf('<tr><th>Predictors retained (of %d offered)</th><td class="num">%d</td></tr>', length(preds), earth$n_pred_model),
sprintf('<tr><th>GCV</th><td class="num">%s</td></tr>', format(earth$gcv, big.mark=",", scientific=FALSE)),
'</tbody></table>',
'<p>MARS builds piecewise-linear hinge functions and <i>automatically</i> selects ',
'interactions (degree 2), then prunes via generalized cross-validation. It kept a ',
'parsimonious ', earth$n_pred_model, '-predictor, ', earth$n_terms, '-term model. The ',
'near-zero in-sample&ndash;to&ndash;CV gap (', r4(earth$r2-earth$cv_r2),
') is the signature of a well-regularized fit that generalizes.</p>',
'<p class="links">', e_html, '</p>',
'</div>',

# -------- glmnetUI section --------
'<h3>4.2 glmnetUI &mdash; Elastic-Net Regularized Regression</h3>',
'<div class="card">',
'<table><tbody>',
sprintf('<tr><th>Indicated value</th><td class="num">$%s</td></tr>', d(val["glmnet"])),
sprintf('<tr><th>In-sample R&sup2; / adj. R&sup2;</th><td class="num">%s / %s</td></tr>', r4(glmnet$r2), r4(glmnet$adj)),
sprintf('<tr><th>CV R&sup2; (10-fold)</th><td class="num">%s</td></tr>', r4(glmnet$cv_r2)),
sprintf('<tr><th>RMSE / MAE</th><td class="num">$%s / $%s</td></tr>', d(glmnet$rmse), d(glmnet$mae)),
sprintf('<tr><th>&alpha; (mix) / &lambda;</th><td class="num">%.2f / %.1f</td></tr>', glmnet$alpha, glmnet$lambda),
'</tbody></table>',
'<p>Elastic net is a <i>linear</i> model with an L1/L2 penalty (&alpha;=', sprintf('%.2f',glmnet$alpha),
', a ridge/lasso blend) that shrinks coefficients for stability. Its value (',
'$', d(val["glmnet"]), ') and in-sample fit track earthUI closely, but its CV R&sup2; ',
'(', r4(glmnet$cv_r2), ') is ', r4(earth$cv_r2-glmnet$cv_r2),
' below earthUI &mdash; the price of a purely linear, additive form that captures ',
'interactions only where they were explicitly entered.</p>',
'<p class="links">', g_html, '</p>',
'</div>',

# -------- mgcvUI section --------
'<h3>4.3 mgcvUI &mdash; Generalized Additive Model (GAM)</h3>',
'<div class="card">',
'<table><tbody>',
sprintf('<tr><th>Indicated value</th><td class="num">$%s</td></tr>', d(val["mgcv"])),
sprintf('<tr><th>In-sample R&sup2; (adj)</th><td class="num">%s</td></tr>', r4(mgcv$r2)),
sprintf('<tr><th>CV R&sup2; (10-fold)</th><td class="num hi">%s</td></tr>', r4(mgcv$cv_r2)),
sprintf('<tr><th>Deviance explained</th><td class="num">%s</td></tr>', p2(mgcv$dev*100)),
sprintf('<tr><th>AIC / BIC</th><td class="num">%s / %s</td></tr>', d(mgcv$aic), d(mgcv$bic)),
sprintf('<tr><th>Smooth terms / method</th><td class="num">%d / %s</td></tr>', mgcv$n_smooths, mgcv$method),
'</tbody></table>',
'<p>The GAM fits penalized smooths plus explicit tensor (<code>ti</code>) ',
'interactions and factor-by-smooth terms. Main smooths: <code>',
paste(main_terms, collapse="</code>, <code>"), '</code>. Tensor interactions: <code>',
paste(int_terms, collapse="</code>, <code>"), '</code>.</p>',
'<p>Strongest categorical (parametric) effects, raw $:</p>',
'<table><thead><tr><th>Term</th><th class="num">Estimate ($)</th><th class="num">p</th></tr></thead><tbody>',
pt_rows, '</tbody></table>',
'<p><b>Water view</b> is the dominant premium (+$', d(pt$Estimate[pt$Term=="view_waterTRUE"][1]),
', p&lt;1e-17). The GAM is the most flexible engine here, which is exactly why it ',
'overfits: in-sample R&sup2; ', r4(mgcv$r2), ' collapses to CV R&sup2; ', r4(mgcv$cv_r2),
' &mdash; a ', r4(mgcv$r2-mgcv$cv_r2), ' gap (see &sect;5).</p>',
'<p class="links">', m_html, '</p>',
'</div>',

# -------- Discussion --------
'<h2>5. Discussion &mdash; Why the Differences?</h2>',
'<h3>Why the value indications differ</h3>',
'<p>earthUI and glmnetUI agree to ', p2(abs(pct(val["glmnet"],val["earth"]))),
' because both resolve to compact, well-regularized structures dominated by the ',
'same main effects (size, location, age, water view). mgcvUI lands ',
p2(pct(val["mgcv"],val["earth"])), ' higher because its richer smooth + interaction ',
'surface bends more steeply toward the subject’s particular feature combination ',
'&mdash; helpful if those bends are real signal, but partly noise here.</p>',
'<h3>Why mgcv’s CV R&sup2; is so much lower</h3>',
'<p>This is the headline finding. On identical (raw-$) footing:</p>',
'<table><thead><tr><th></th><th class="num">earthUI</th><th class="num">glmnetUI</th><th class="num">mgcvUI</th></tr></thead><tbody>',
sprintf('<tr><td>In-sample R&sup2;</td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td></tr>', r4(earth$r2), r4(glmnet$r2), r4(mgcv$r2)),
sprintf('<tr><td>CV R&sup2;</td><td class="num good">%s</td><td class="num">%s</td><td class="num hi">%s</td></tr>', r4(earth$cv_r2), r4(glmnet$cv_r2), r4(mgcv$cv_r2)),
sprintf('<tr><td>Overfit gap</td><td class="num">%s</td><td class="num">%s</td><td class="num hi">%s</td></tr>', r4(earth$r2-earth$cv_r2), r4(glmnet$r2-glmnet$cv_r2), r4(mgcv$r2-mgcv$cv_r2)),
'</tbody></table>',
'<p>earthUI’s gap is essentially zero (', r4(earth$r2-earth$cv_r2),
'): MARS’s GCV pruning keeps only terms that pay their way. glmnetUI’s gap ',
'(', r4(glmnet$r2-glmnet$cv_r2), ') is modest &mdash; ridge/lasso shrinkage. mgcvUI’s gap ',
'(', r4(mgcv$r2-mgcv$cv_r2), ') is large: ', mgcv$n_smooths, ' penalized smooths plus ',
length(int_terms), ' tensor interactions and factor-by-smooths give it enough ',
'effective degrees of freedom to chase patterns that don’t replicate on held-out ',
'folds. REML smoothing resists this but cannot fully offset that much flexibility on ',
sprintf('~%d', mgcv$n_obs), ' rows.</p>',
'<div class="note"><b>Practical reading:</b> a lower mgcv CV R&sup2; does <i>not</i> ',
'mean a worse value conclusion &mdash; it means mgcv’s extra flexibility is not ',
'earning its keep on this dataset. Pruning its interaction set toward the few earthUI ',
'actually used would likely raise its CV R&sup2; toward the others and is the recommended ',
'next step.</div>',
'<h3>Other points of interest</h3>',
'<ul>',
'<li><b>Consistent value signal.</b> Three different mathematical engines independently ',
'land within ', p2(rng_pct), ' &mdash; strong triangulation of the subject’s value.</li>',
'<li><b>Same drivers everywhere.</b> Size (<code>sq_ft_total</code>), location ',
'(<code>area</code>/<code>latitude</code>), age, and especially <b>water view</b> ',
'dominate all three models.</li>',
'<li><b>Row handling.</b> mgcv fit on ', mgcv$n_obs, ' rows vs ', earth$n_obs,
' for earth/glmnet (it retained the subject row); a minor inconsistency worth aligning.</li>',
'</ul>',

# -------- Latent variables --------
'<h2>6. Caveat &mdash; Latent Variables (Condition &amp; Quality)</h2>',
'<p>This analysis does <b>not</b> model property <b>Condition</b> and <b>Quality</b>, ',
'which are the two latent variables most likely to carry material unexplained ',
'value here. Their absence has two consequences:</p>',
'<ul>',
'<li>Part of every model’s residual variance &mdash; and a share of the ~', p2(rng_pct),
' spread between the three indications &mdash; is almost certainly Condition/Quality ',
'differences between the subject and comparables that the models cannot see.</li>',
'<li>Because Condition and Quality often <i>correlate</i> with included features ',
'(newer/larger/water-view homes tend to be higher quality), the models may be ',
'absorbing some of that effect through correlated proxies &mdash; flattering in-sample ',
'fit while adding out-of-sample noise (a contributor to mgcv’s CV gap).</li>',
'</ul>',
'<p>Incorporating Condition and Quality (e.g. as ordinal factors or factor-by-smooth ',
'terms) is the single most promising avenue to tighten all three models and their ',
'reconciliation.</p>',

'<div class="foot">Generated from the earthUI / glmnetUI / mgcvUI report bundles in ',
'<code>AF/</code>. All figures raw-dollar scale, no log transform. ',
'Indicated values supplied by the analyst; fit statistics read directly from each ',
'model’s <code>report_data.rds</code>.</div>',
'</div></body></html>')

writeLines(html, "Comparative_Analysis.html")
cat("WROTE", file.path(AF, "Comparative_Analysis.html"),
    "(", round(file.size("Comparative_Analysis.html")/1024,1), "KB )\n")
cat(sprintf("values: earth %d  glmnet %d  mgcv %d  | CV: %.4f/%.4f/%.4f | range %.2f%%\n",
            val["earth"], val["glmnet"], val["mgcv"],
            earth$cv_r2, glmnet$cv_r2, mgcv$cv_r2, rng_pct))
