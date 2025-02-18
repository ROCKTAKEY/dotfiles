(use-modules (gnu home)
             (gnu home services)
             (guix gexp))

(home-environment
  (services
   (cons*
    %base-home-services)))
