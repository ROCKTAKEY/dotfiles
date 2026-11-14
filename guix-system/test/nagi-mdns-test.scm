(use-modules (gnu services)
             (gnu services networking)
             (gnu system)
             (guix gexp)
             (srfi srfi-1))

(define os (primitive-load "guix-system/config/nagi.scm"))
(define firewall
  (service-value
   (find (lambda (service)
           (eq? iptables-service-type (service-kind service)))
         (operating-system-services os))))

(for-each
 (lambda (rules)
   (let* ((text (plain-file-content rules))
          (allow (string-contains
                  text "-A INPUT -i eno1 -p udp --dport 5353 -j ACCEPT\n"))
          (reject (string-contains text "-A INPUT -j REJECT")))
     (unless (and allow reject (< allow reject))
       (error "LAN mDNS must be accepted before the default rejection"
              (plain-file-name rules)))))
 (list (iptables-configuration-ipv4-rules firewall)
       (iptables-configuration-ipv6-rules firewall)))
