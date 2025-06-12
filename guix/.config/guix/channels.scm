(cons* (channel
        (name 'nonguix)
        (url "https://gitlab.com/nonguix/nonguix")
        ;; Enable signature verification:
        (introduction
         (make-channel-introduction
          "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
          (openpgp-fingerprint
           "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
       (channel
        (name 'guix-jp)
        (url "https://gitlab.com/guix-jp/channel")
        (branch "main"))
       (channel
        (name 'guix-hpc)
        (url "https://gitlab.inria.fr/guix-hpc/guix-hpc.git"))
       ;; (channel
       ;;  (name 'guix-hpc-non-free)
       ;;  (url "https://gitlab.inria.fr/guix-hpc/guix-hpc-non-free.git"))
       (channel
        (name 'ccwl)
        (url "https://github.com/arunisaac/ccwl")
        (branch "main")
        (introduction
         (make-channel-introduction
          "01ec2e0c15a9f7db2b6374e1469a90df30109300"
          (openpgp-fingerprint
           "7F73 0343 F2F0 9F3C 77BF  79D3 2E25 EE8B 6180 2BB3"))))
       (channel
        (name 'roquix)
        (url "https://github.com/ROCKTAKEY/roquix"))
       %default-channels)
