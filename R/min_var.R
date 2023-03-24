calculate_min_variance_portfolio <- function(smi, gold, bitcoin, swiss_gov_bonds, us_gov_bonds, sp500, usd_chf) {
  portfolio_spec <- portfolio.spec(colnames(returns))
  portfolio_constraints <- add.constraint(portfolio_spec, type = "full_investment")
  portfolio_constraints <- add.constraint(portfolio_constraints, type = "long_only")
  portfolio_constraints <- add.constraint(portfolio_constraints, type = "box", min = 0.05, max = 0.3)
  portfolio_constraints}