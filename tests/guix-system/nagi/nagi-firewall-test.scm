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

(define (assert-accepted-before-rejection rules match)
  (let* ((text (plain-file-content rules))
         (allow (string-contains
                 text (string-append "-A INPUT " match " -j ACCEPT\n")))
         (reject (string-contains text "-A INPUT -j REJECT")))
    (unless (and allow reject (< allow reject))
      (error "Traffic must be accepted before the default rejection"
             (plain-file-name rules) match))))

(for-each
 (lambda (rules protocol)
   (assert-accepted-before-rejection rules "-p udp --dport 5353")
   (assert-accepted-before-rejection rules (string-append "-p " protocol)))
 (list (iptables-configuration-ipv4-rules firewall)
       (iptables-configuration-ipv6-rules firewall))
 '("icmp" "ipv6-icmp"))
