(define-module (roquix services tailscale)
  #:use-module (gnu services)
  #:export (tailscale-service-type))

(define tailscale-service-type
  (service-type
   (name 'tailscale)
   (extensions '())
   (default-value 'stub)
   (description "Test stub for the local roquix tailscale service.")))
