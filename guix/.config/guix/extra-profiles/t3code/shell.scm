(use-modules (roquix extra-profiles shell-configuration))

;; T3 launches providers with per-session MCP credentials and ACP bridge settings.
(shell-configuration
 (preserve '("^T3_" "^T3CODE_")))
