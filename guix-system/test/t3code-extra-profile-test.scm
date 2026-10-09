(use-modules (roquix extra-profiles shell-configuration)
             (roquix extra-profiles paths)
             (ice-9 regex)
             (srfi srfi-1)
             (srfi srfi-64))

(define definition-directory
  (string-append (getcwd) "/guix/.config/guix/extra-profiles"))
(define profile-file
  (string-append definition-directory "/t3code/shell.scm"))
(define config
  (if (file-exists? profile-file)
      (load profile-file)
      (shell-configuration)))

(define (preserved? configuration name)
  (any (lambda (pattern) (string-match pattern name))
       (shell-configuration-preserve configuration)))

(test-begin "t3code-extra-profile")
(define runner (test-runner-current))
(test-assert "T3 credentials and runtime settings cross the container boundary"
  (every (lambda (name) (preserved? config name))
         '("T3_MCP_BEARER_TOKEN" "T3_MCP_URL" "T3_ACP_MCP_AUTHORIZATION"
           "T3_ACP_MCP_ENDPOINT" "T3_ACP_MCP_NODE" "T3_ACP_MCP_ENTRYPOINT"
           "T3CODE_HOME" "T3CODE_LOG_WS_EVENTS")))
(test-assert "Unrelated environment variables remain excluded"
  (not (any (lambda (name) (preserved? config name))
            '("OTHER_SECRET" "NOT_T3_MCP_BEARER_TOKEN" "T3CODEX_HOME"))))
(test-assert "Codex composition retains both T3 credentials and terminal colors"
  (let ((combined
         (compose-shell-configurations
          (list (cons (parse-profile-name "codex")
                      (load (string-append definition-directory "/codex/shell.scm")))
                (cons (parse-profile-name "t3code") config)))))
    (and (preserved? (composed-shell-configuration-settings combined) "T3_MCP_BEARER_TOKEN")
         (preserved? (composed-shell-configuration-settings combined) "COLORTERM"))))
(test-end "t3code-extra-profile")
(exit (if (zero? (test-runner-fail-count runner)) 0 1))
