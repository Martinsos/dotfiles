((elisp-refs :source #1="elpaca-menu-lock-file" :recipe
             (:package "elisp-refs" :repo "Wilfred/elisp-refs" :fetcher github :files
                       (:defaults (:exclude "elisp-refs-bench.el")) :source #2=
                       "elpaca-menu-lock-file" :protocol https :inherit t :depth treeless :ref
                       "541a064c3ce27867872cf708354a65d83baf2a6d"))
 (shell-maker :source #1# :recipe
              (:package "shell-maker" :fetcher github :repo "xenodium/shell-maker" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source #2# :protocol https :inherit t :depth treeless :ref
                        "98780ec0a8ac4fe1d6be9e2f8047195cdfad5f70"))
 (polymode :source #1# :recipe
           (:package "polymode" :fetcher github :repo "polymode/polymode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "74ba75d4bcfbea959ccc9080a95ab9ef759849f2"))
 (reformatter :source #1# :recipe
              (:package "reformatter" :repo "purcell/emacs-reformatter" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source #2# :protocol https :inherit t :depth treeless :ref
                        "f2cb59466b1c3f85a8c960f7d4b7b7ead015bedc"))
 (cfrs :source #1# :recipe
       (:package "cfrs" :repo "Alexander-Miller/cfrs" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source #2# :protocol https :inherit t :depth treeless :ref
                 "f3a21f237b2a54e6b9f8a420a9da42b4f0a63121"))
 (pfuture :source #1# :recipe
          (:package "pfuture" :repo "Alexander-Miller/pfuture" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "19b53aebbc0f2da31de6326c495038901bffb73c"))
 (treemacs :source #1# :recipe
           (:package "treemacs" :fetcher github :repo "Alexander-Miller/treemacs" :files
                     (:defaults "Changelog.org" "icons" "src/elisp/treemacs*.el"
                                "src/scripts/treemacs*.py" (:exclude "src/extra/*"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "32bb3dd02ddfca85661614b3b227e770fab821e2"))
 (spinner :source #1# :recipe
          (:package "spinner" :repo ("https://github.com/Malabarba/spinner.el" . "spinner") :files
                    ("*" (:exclude ".git")) :source #2# :protocol https :inherit t :depth treeless
                    :ref "d4647ae87fb0cd24bc9081a3d287c860ff061c21"))
 (posframe :source #1# :recipe
           (:package "posframe" :fetcher github :repo "tumashu/posframe" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "81651536827c96bf5af5265ee7918ab70e1dd5b1"))
 (with-editor :source #1#
   :recipe
   (:package "with-editor" :fetcher github :repo "magit/with-editor" :files
             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
              "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
              "docs/*.texinfo"
              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                        "README*" "*-pkg.el"))
             :source #2# :protocol https :inherit t :depth treeless :ref
             "ca902ae02972bdd6919a902be2593d8cb6bd991b"))
 (magit-section :source #1# :recipe
                (:package "magit-section" :fetcher github :repo "magit/magit" :files
                          ("lisp/magit-section.el" "docs/magit-section.texi" "magit-section-pkg.el")
                          :source #2# :protocol https :inherit t :depth treeless :ref
                          "e9614a336ffef61e8660188a95ae5d78674ed36d"))
 (ts :source #1# :recipe
     (:package "ts" :fetcher github :repo "alphapapa/ts.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                          "README*" "*-pkg.el"))
               :source #2# :protocol https :inherit t :depth treeless :ref
               "552936017cfdec89f7fc20c254ae6b37c3f22c5b"))
 (ht :source #1# :recipe
     (:package "ht" :fetcher github :repo "Wilfred/ht.el" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                          "README*" "*-pkg.el"))
               :source #2# :protocol https :inherit t :depth treeless :ref
               "1c49aad1c820c86f7ee35bf9fff8429502f60fef"))
 (log4e :source #1# :recipe
        (:package "log4e" :repo "aki2o/log4e" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "6d71462df9bf595d3861bfb328377346aceed422"))
 (gntp :source #1# :recipe
       (:package "gntp" :repo "tekai/gntp.el" :fetcher github :files
                 ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                  "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                  "docs/*.texinfo"
                  (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                            "README*" "*-pkg.el"))
                 :source #2# :protocol https :inherit t :depth treeless :ref
                 "767571135e2c0985944017dc59b0be79af222ef5"))
 (kv :source #1# :recipe
     (:package "kv" :fetcher github :repo "nicferrier/emacs-kv" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                          "README*" "*-pkg.el"))
               :source #2# :protocol https :inherit t :depth treeless :ref
               "721148475bce38a70e0b678ba8aa923652e8900e"))
 (db :source #1# :recipe
     (:package "db" :fetcher github :repo "nicferrier/emacs-db" :files
               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                "docs/*.texinfo"
                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                          "README*" "*-pkg.el"))
               :source #2# :protocol https :inherit t :depth treeless :ref
               "b3a423fb8e72f9013009cbe033d654df2ce31438"))
 (fakir :source #1# :recipe
        (:package "fakir" :fetcher github :repo "nicferrier/emacs-fakir" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "1fca406ad7de80fece6319ff75d4230b648534b0"))
 (creole :source #1# :recipe
         (:package "creole" :fetcher github :repo "nicferrier/elwikicreole" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source #2# :protocol https :inherit t :depth treeless :ref
                   "7d5cffe93857f6c75ca09ac79c0e47b8d4410e53"))
 (noflet :source #1# :recipe
         (:package "noflet" :fetcher github :repo "nicferrier/emacs-noflet" :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source #2# :protocol https :inherit t :depth treeless :ref
                   "7ae84dc3257637af7334101456dafe1759c6b68a"))
 (web :source #1# :recipe
      (:package "web" :fetcher github :repo "nicferrier/emacs-web" :files ("web.el") :source #2#
                :protocol https :inherit t :depth treeless :ref
                "483188dac4bc6b409b985c9dae45f3324a425efd"))
 (deferred :source #1# :recipe
           (:package "deferred" :repo "kiwanami/emacs-deferred" :fetcher github :files
                     ("deferred.el") :source #2# :protocol https :inherit t :depth treeless :ref
                     "2239671d94b38d92e9b28d4e12fd79814cfb9c16"))
 (request-deferred :source #1# :recipe
                   (:package "request-deferred" :repo "tkf/emacs-request" :fetcher github :files
                             ("request-deferred.el") :source #2# :protocol https :inherit t :depth
                             treeless :ref "01e338c335c07e4407239619e57361944a82cb8a"))
 (request :source #1#
   :recipe
   (:package "request" :repo "tkf/emacs-request" :fetcher github :files ("request.el") :source #2#
             :protocol https :inherit t :depth treeless :ref
             "01e338c335c07e4407239619e57361944a82cb8a"))
 (persist :source #1# :recipe
          (:package "persist" :repo ("git://git.sv.gnu.org/emacs/elpa" . "persist") :branch
                    "externals/persist" :files ("*" (:exclude ".git")) :source #2# :protocol https
                    :inherit t :depth treeless :ref "076bf3ffc4fb0fa904c6ace9620fccaacb3454ba"))
 (elnode :source #1# :recipe
         (:package "elnode" :fetcher github :repo "jcaw/elnode" :branch "melpa" :files
                   ("default*" "elnode*") :source #2# :protocol https :inherit t :depth treeless
                   :ref "29ef0f51a65a24fca7fdcdb4140d2e4556e4bb29"))
 (alert :source #1# :recipe
        (:package "alert" :fetcher github :repo "jwiegley/alert" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "7774b5fd2feb98d4910ff06435d08c19fba93e26"))
 (aio :source #1# :recipe
      (:package "aio" :fetcher github :repo "skeeto/emacs-aio" :files
                ("aio.el" "README.md" "UNLICENSE") :source #2# :protocol https :inherit t :depth
                treeless :ref "da93523e235529fa97d6f251319d9e1d6fc24a41"))
 (annalist :source #1# :recipe
           (:package "annalist" :fetcher github :repo "noctuid/annalist.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "e1ef5dad75fa502d761f70d9ddf1aeb1c423f41d"))
 (goto-chg :source #1# :recipe
           (:package "goto-chg" :repo "emacs-evil/goto-chg" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "72f556524b88e9d30dc7fc5b0dc32078c166fda7"))
 (lv :source #1# :recipe
     (:package "lv" :repo "abo-abo/hydra" :fetcher github :files ("lv.el") :source #2# :protocol
               https :inherit t :depth treeless :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (htmlize :source #1# :recipe
          (:package "htmlize" :fetcher github :repo "hniksic/emacs-htmlize" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "8e3841c837b4b78bd72ad7f0436e919f39315a46"))
 (rainbow-delimiters :source #1# :recipe
                     (:package "rainbow-delimiters" :fetcher github :repo
                               "Fanael/rainbow-delimiters" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source #2# :protocol https :inherit t :depth treeless :ref
                               "f40ece58df8b2f0fb6c8576b527755a552a5e763"))
 (colorful-mode :source #1# :recipe
                (:package "colorful-mode" :repo
                          ("https://github.com/DevelopmentCool2449/colorful-mode" . "colorful-mode")
                          :files ("*" (:exclude ".git")) :source #2# :protocol https :inherit t
                          :depth treeless :ref "706472ea7f0ee2fe5719cd91df7315fe9ca86114"))
 (helpful :source #1# :recipe
          (:package "helpful" :repo "Wilfred/helpful" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "4ba24cac9fb14d5fdc32582cd947572040e82b2c"))
 (all-the-icons :source #1# :recipe
                (:package "all-the-icons" :repo "domtronn/all-the-icons.el" :fetcher github :files
                          (:defaults "data") :source #2# :protocol https :inherit t :depth treeless
                          :ref "39ef44f810c34e8900978788467cc675870bcd19"))
 (hl-todo :source #1# :recipe
          (:package "hl-todo" :repo "tarsius/hl-todo" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "fb692ec0923bcd98eaa8760127921aca61db8e1e"))
 (amx :source #1# :recipe
      (:package "amx" :repo "DarwinAwardWinner/amx" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source #2# :protocol https :inherit t :depth treeless :ref
                "5b3aa1aae84f4a225cb8d26ab79a32f97693f023"))
 (winum :source #1# :recipe
        (:package "winum" :fetcher github :repo "deb0ch/emacs-winum" :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "c5455e866e8a5f7eab6a7263e2057aff5f1118b9"))
 (ace-window :source #1# :recipe
             (:package "ace-window" :repo "abo-abo/ace-window" :fetcher github :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source #2# :protocol https :inherit t :depth treeless :ref
                       "77115afc1b0b9f633084cf7479c767988106c196"))
 (avy :source #1# :recipe
      (:package "avy" :repo "abo-abo/avy" :fetcher github :files
                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                 "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                 "docs/*.texinfo"
                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                           "README*" "*-pkg.el"))
                :source #2# :protocol https :inherit t :depth treeless :ref
                "933d1f36cca0f71e4acb5fac707e9ae26c536264"))
 (xclip :source #1# :recipe
        (:package "xclip" :repo ("git://git.sv.gnu.org/emacs/elpa" . "xclip") :branch
                  "externals/xclip" :files ("*" (:exclude ".git")) :source #2# :protocol https
                  :inherit t :depth treeless :ref "7febe164de2a881b83b9d604d3c7cf20b69f422d"))
 (visual-fill-column :source #1# :recipe
                     (:package "visual-fill-column" :fetcher codeberg :repo
                               "joostkremers/visual-fill-column" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source #2# :protocol https :inherit t :depth treeless :ref
                               "11575df4e042f3263db62de0e8ee4f2331fbb1b4"))
 (column-enforce-mode :source #1# :recipe
                      (:package "column-enforce-mode" :fetcher github :repo
                                "jordonbiondo/column-enforce-mode" :files
                                ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                 "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                 "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                 (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                           "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                                :source #2# :protocol https :inherit t :depth treeless :ref
                                "14a7622f2268890e33536ccd29510024d51ee96f"))
 (ethan-wspace :source #1# :recipe
               (:package "ethan-wspace" :fetcher github :repo "glasserc/ethan-wspace" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source #2# :protocol https :inherit t :depth treeless :ref
                         "d205f7781f18f6746d50e562dc5e3595189ccd64"))
 (copilot-chat :source #1# :recipe
               (:package "copilot-chat" :fetcher github :repo "chep/copilot-chat.el" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source #2# :protocol https :inherit t :depth treeless :ref
                         "fe28abe5ba3ef98d848c036630abf965b65e9ca8"))
 (copilot :source #1# :recipe
          (:package "copilot" :fetcher github :repo "copilot-emacs/copilot.el" :files ("*.el")
                    :source #2# :protocol https :inherit t :depth treeless :host github :ref
                    "7d105d708a23d16cdfd5240500be8bb02f95a46e"))
 (gptel :source #1# :recipe
        (:package "gptel" :repo "karthink/gptel" :fetcher github :files
                  ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                   "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                   "docs/*.texinfo"
                   (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                             "LICENSE" "README*" "*-pkg.el"))
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "748fd85493889079f0fe102010c816ee75e802ec"))
 (markdown-mode :source #1# :recipe
                (:package "markdown-mode" :fetcher github :repo "jrblevin/markdown-mode" :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source #2# :protocol https :inherit t :depth treeless :ref
                          "7659bc470d096e7a53285fa246cbabcb07d66a33"))
 (lsp-haskell :source #1# :recipe
              (:package "lsp-haskell" :repo "emacs-lsp/lsp-haskell" :fetcher github :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source #2# :protocol https :inherit t :depth treeless :ref
                        "6981f8d1225c038c1a130e8cf70530cfe15f976e"))
 (haskell-mode :source #1# :recipe
               (:package "haskell-mode" :repo "haskell/haskell-mode" :fetcher github :files
                         (:defaults "NEWS" "logo.svg") :source #2# :protocol https :inherit t :depth
                         treeless :ref "4cec530008ef4054826eb1b55d6b26ae8e2807c0"))
 (ormolu :source #1# :recipe
         (:package "ormolu" :repo "vyorkin/ormolu.el" :fetcher github :files
                   ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                    "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                    "docs/*.texinfo"
                    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                              "LICENSE" "README*" "*-pkg.el"))
                   :source #2# :protocol https :inherit t :depth treeless :ref
                   "a6b1d3f8838d067ac5352fb0673c3c3dae7abd73"))
 (lsp-treemacs :source #1# :recipe
               (:package "lsp-treemacs" :repo "emacs-lsp/lsp-treemacs" :fetcher github :files
                         (:defaults "icons") :source #2# :protocol https :inherit t :depth treeless
                         :ref "fb1a07ae0a3d781dea8ac78da2933e0173eb48a4"))
 (lsp-ivy :source #1# :recipe
          (:package "lsp-ivy" :repo "emacs-lsp/lsp-ivy" :fetcher github :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "6b2a625f08fb096a35faebf3c3ea0c8b295bdacd"))
 (lsp-ui :source #1# :recipe
         (:package "lsp-ui" :repo "emacs-lsp/lsp-ui" :fetcher github :files
                   (:defaults "lsp-ui-doc.html" "resources") :source #2# :protocol https :inherit t
                   :depth treeless :ref "1eae284ab0ae70fba4a2ee1e3f76d7013521c49c"))
 (lsp-mode :source #1# :recipe
           (:package "lsp-mode" :repo "emacs-lsp/lsp-mode" :fetcher github :files
                     (:defaults "clients/*.el") :source #2# :protocol https :inherit t :depth
                     treeless :ref "2e5f23a0dc5dcc1c957fca693c956d584338e3e9"))
 (sideline-lsp :source #1# :recipe
               (:package "sideline-lsp" :repo "emacs-sideline/sideline-lsp" :fetcher github :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source #2# :protocol https :inherit t :depth treeless :ref
                         "29b95888a91f53e5161a3f2e2fc27a9cfc71ebe5"))
 (sideline-flycheck :source #1# :recipe
                    (:package "sideline-flycheck" :repo "emacs-sideline/sideline-flycheck" :fetcher
                              github :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source #2# :protocol https :inherit t :depth treeless :ref
                              "bce7bbc90edc3cdeb3144a8fb8d59f5529e3391b"))
 (sideline :source #1# :recipe
           (:package "sideline" :repo "emacs-sideline/sideline" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "709095ca022dd4ea4ceeb6ab18ce9b9dbda0d6a5"))
 (flycheck-posframe :source #1# :recipe
                    (:package "flycheck-posframe" :fetcher github :repo
                              "alexmurray/flycheck-posframe" :files
                              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                               "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                               "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                         "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                              :source #2# :protocol https :inherit t :depth treeless :ref
                              "19896b922c76a0f460bf3fe8d8ebc2f9ac9028d8"))
 (flycheck :source #1# :recipe
           (:package "flycheck" :repo "flycheck/flycheck" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "87ad201889b2055d0c4b0839d2f8b80eeccae59e"))
 (jinx :source #1# :recipe
       (:package "jinx" :repo "minad/jinx" :files (:defaults "jinx-mod.c" "emacs-module.h") :fetcher
                 github :source #2# :protocol https :inherit t :depth treeless :ref
                 "2144d03f7bcc3f461b4e67f913dfc0055e4ad7e2"))
 (vterm-toggle :source #1# :recipe
               (:package "vterm-toggle" :fetcher github :repo "jixiuf/vterm-toggle" :files
                         ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                          "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                          "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                          (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                    "LICENSE" "README*" "*-pkg.el"))
                         :source #2# :protocol https :inherit t :depth treeless :ref
                         "06cb4f3c565e46470a3c4505c11e26066d869715"))
 (vterm :source #1# :recipe
        (:package "vterm" :fetcher github :repo "akermu/emacs-libvterm" :files
                  ("CMakeLists.txt" "elisp.c" "elisp.h" "emacs-module.h" "etc" "utf8.c" "utf8.h"
                   "vterm.el" "vterm-module.c" "vterm-module.h")
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "056ad74653704bc353d8ec8ab52ac75267b7d373"))
 (company :source #1# :recipe
          (:package "company" :fetcher github :repo "company-mode/company-mode" :files
                    (:defaults "icons" ("images/small" "doc/images/small/*.png")) :source #2#
                    :protocol https :inherit t :depth treeless :ref
                    "9a81d0cca268d35d5e18d7d4e2277a9e57887dd8"))
 (diff-hl :source #1# :recipe
          (:package "diff-hl" :fetcher github :repo "dgutov/diff-hl" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "65a5de16e21c87b7c12a78a63fc3b57e07c03c86"))
 (magit :source #1# :recipe
        (:package "magit" :fetcher github :repo "magit/magit" :files
                  ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE"
                   "magit-pkg.el" (:exclude "lisp/magit-section.el"))
                  :source #2# :protocol https :inherit t :depth treeless :ref
                  "e9614a336ffef61e8660188a95ae5d78674ed36d"))
 (transient :source #1# :recipe
            (:package "transient" :fetcher github :repo "magit/transient" :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source #2# :protocol https :inherit t :depth treeless :ref
                      "13f3f5e0da85805e9ead0db70ba76a6bbc62d3a7"))
 (gitstatus :source #1# :recipe
            (:package "gitstatus" :repo "igorepst/gitstatus-el" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source #2# :protocol https :inherit t :depth treeless :ref
                      "c3e30341d0add9728010e566b9eb031c76414b47"))
 (counsel-projectile :source #1# :recipe
                     (:package "counsel-projectile" :fetcher github :repo
                               "ericdanan/counsel-projectile" :files
                               ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                                "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                                "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                                (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                          "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                               :source #2# :protocol https :inherit t :depth treeless :ref
                               "40d1e1d4bb70acb00fddd6f4df9778bf2c52734b"))
 (projectile :source #1# :recipe
             (:package "projectile" :fetcher github :repo "bbatsov/projectile" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source #2# :protocol https :inherit t :depth treeless :ref
                       "3b32633d8f90cea05de8ec04bf3b3f5e80b34935"))
 (ivy-rich :source #1# :recipe
           (:package "ivy-rich" :repo "Yevgnen/ivy-rich" :fetcher github :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "aff9b6bd53e0fdcf350ab83c90e64e651b47dba4"))
 (swiper :source #1# :recipe
         (:package "swiper" :repo "abo-abo/swiper" :fetcher github :files ("swiper.el") :source #2#
                   :protocol https :inherit t :depth treeless :ref
                   "abb9e1e5649bcd078a2a75bcb27abb42311b4f86"))
 (counsel :source #1# :recipe
          (:package "counsel" :repo "abo-abo/swiper" :fetcher github :files ("counsel.el") :source
                    #2# :protocol https :inherit t :depth treeless :ref
                    "abb9e1e5649bcd078a2a75bcb27abb42311b4f86"))
 (ivy :source #1# :recipe
      (:package "ivy" :repo "abo-abo/swiper" :fetcher github :files
                (:defaults "doc/ivy-help.org"
                           (:exclude "swiper.el" "counsel.el" "ivy-hydra.el" "ivy-avy.el"))
                :source #2# :protocol https :inherit t :depth treeless :ref
                "abb9e1e5649bcd078a2a75bcb27abb42311b4f86"))
 (org-super-agenda :source #1# :recipe
                   (:package "org-super-agenda" :fetcher github :repo "alphapapa/org-super-agenda"
                             :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source #2# :protocol https :inherit t :depth treeless :ref
                             "c07e35416ce749b42cbb70ff8d4e814362111bb5"))
 (org-gcal :source #1# :recipe
           (:package "org-gcal" :fetcher github :repo "kidd/org-gcal.el" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "9f9d93e4f0d5863b1318e9e702e1ee6e841c2649"))
 (org-present :source #1# :recipe
              (:package "org-present" :fetcher github :repo "rlister/org-present" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source #2# :protocol https :inherit t :depth treeless :ref
                        "4ec04e1b77dea76d7c30066ccf3200d2e0b7bee9"))
 (org-tidy :source #1# :recipe
           (:package "org-tidy" :fetcher github :repo "jxq0/org-tidy" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "0bea3a2ceaa999e0ad195ba525c5c1dcf5fba43b"))
 (evil-org :source #1# :recipe
           (:package "evil-org" :fetcher github :repo "Somelauw/evil-org-mode" :files
                     ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                      "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                      "docs/*.texi" "docs/*.texinfo"
                      (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                "LICENSE" "README*" "*-pkg.el"))
                     :source #2# :protocol https :inherit t :depth treeless :ref
                     "b1f309726b1326e1a103742524ec331789f2bf94"))
 (org-appear :source #1# :recipe
             (:package "org-appear" :fetcher github :repo "awth13/org-appear" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source #2# :protocol https :inherit t :depth treeless :ref
                       "32ee50f8fdfa449bbc235617549c1bccb503cb09"))
 (org-rainbow-tags :source #1# :recipe
                   (:package "org-rainbow-tags" :fetcher github :repo
                             "KaratasFurkan/org-rainbow-tags" :files
                             ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                              "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                              "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                              (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el"
                                        "*-tests.el" "LICENSE" "README*" "*-pkg.el"))
                             :source #2# :protocol https :inherit t :depth treeless :ref
                             "dfe36047bc9646b621452f3e2e97170e99e2b43f"))
 (org-superstar :source #1# :recipe
                (:package "org-superstar" :fetcher github :repo "integral-dw/org-superstar-mode"
                          :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source #2# :protocol https :inherit t :depth treeless :ref
                          "54c81c27dde2a6dc461bb064e79a8b2089093a2e"))
 (org :source #1# :recipe
      (:package "org" :repo ("https://git.savannah.gnu.org/git/emacs/org-mode.git" . "org")
                :pre-build (progn (require 'elpaca-menu-org) (elpaca-menu-org--build)) :autoloads
                "org-loaddefs.el" :depth treeless :build (:not elpaca--generate-autoloads-async)
                :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")) :source #2# :protocol
                https :inherit t :ref "2afac54c573e373aa77912c5e5c8c9cbe477108e"))
 (evil-collection :source #1# :recipe
                  (:package "evil-collection" :fetcher github :repo "emacs-evil/evil-collection"
                            :files (:defaults "modes") :source #2# :protocol https :inherit t :depth
                            treeless :ref "8fb2f037b890b84ac4bf35f3acb6c74a4c826acd"))
 (evil-escape :source #1# :recipe
              (:package "evil-escape" :fetcher github :repo "emacsorphanage/evil-escape" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source #2# :protocol https :inherit t :depth treeless :ref
                        "aebd1a78a6bd33e5164e7552096b3fe1172d3012"))
 (evil :source #1# :recipe
       (:package "evil" :repo "emacs-evil/evil" :fetcher github :files
                 (:defaults "doc/build/texinfo/evil.texi" (:exclude "evil-test-helpers.el")) :source
                 #2# :protocol https :inherit t :depth treeless :ref
                 "423c4b98588ef3219d69a3d0a985f24c705e4eb9"))
 (hydra :source #1# :recipe
        (:package "hydra" :repo "abo-abo/hydra" :fetcher github :files
                  (:defaults (:exclude "lv.el")) :source #2# :protocol https :inherit t :depth
                  treeless :ref "317e1de33086637579a7aeb60f77ed0405bf359b"))
 (which-key :source #1# :recipe
            (:package "which-key" :repo "justbur/emacs-which-key" :fetcher github :files
                      ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                       "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                       "docs/*.texi" "docs/*.texinfo"
                       (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                 "LICENSE" "README*" "*-pkg.el"))
                      :source #2# :protocol https :inherit t :depth treeless :ref
                      "38d4308d1143b61e4004b6e7a940686784e51500"))
 (f :source #1# :recipe
    (:package "f" :fetcher github :repo "rejeep/f.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source #2# :protocol https :inherit t :depth treeless :ref
              "931b6d0667fe03e7bf1c6c282d6d8d7006143c52"))
 (dash :source #1# :recipe
       (:package "dash" :fetcher github :repo "magnars/dash.el" :files ("dash.el" "dash.texi")
                 :source #2# :protocol https :inherit t :depth treeless :ref
                 "1de9dcb83eacfb162b6d9a118a4770b1281bcd84"))
 (s :source #1# :recipe
    (:package "s" :fetcher github :repo "magnars/s.el" :files
              ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
               "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
               "docs/*.texinfo"
               (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
                         "README*" "*-pkg.el"))
              :source #2# :protocol https :inherit t :depth treeless :ref
              "dda84d38fffdaf0c9b12837b504b402af910d01d"))
 (shrink-path :source #1# :recipe
              (:package "shrink-path" :fetcher gitlab :repo "bennya/shrink-path.el" :files
                        ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                         "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                         "docs/*.texi" "docs/*.texinfo"
                         (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                   "LICENSE" "README*" "*-pkg.el"))
                        :source #2# :protocol https :inherit t :depth treeless :ref
                        "c14882c8599aec79a6e8ef2d06454254bb3e1e41"))
 (nerd-icons :source #1# :recipe
             (:package "nerd-icons" :repo "rainstormstudio/nerd-icons.el" :fetcher github :files
                       (:defaults "data") :source #2# :protocol https :inherit t :depth treeless
                       :ref "546ee20caf825e65da4c5435d31f13d269ed2a81"))
 (general :source #1# :recipe
          (:package "general" :fetcher github :repo "noctuid/general.el" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :wait t :ref
                    "826bf2b97a0fb4a34c5eb96ec2b172d682fd548f"))
 (vundo :source #1# :recipe
        (:package "vundo" :repo ("https://github.com/casouri/vundo" . "vundo") :files
                  ("*" (:exclude ".git" "test")) :source #2# :protocol https :inherit t :depth
                  treeless :ref "288892ee93214ddcaf65fdd9f3134c1bd10100ee"))
 (undo-fu :source #1# :recipe
          (:package "undo-fu" :fetcher codeberg :repo "ideasman42/emacs-undo-fu" :files
                    ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                     "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info" "docs/*.texi"
                     "docs/*.texinfo"
                     (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                               "LICENSE" "README*" "*-pkg.el"))
                    :source #2# :protocol https :inherit t :depth treeless :ref
                    "399cc12f907f81a709f9014b6fad0205700d5772"))
 (doom-modeline :source #1# :recipe
                (:package "doom-modeline" :repo "seagle0128/doom-modeline" :fetcher github :files
                          ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir"
                           "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir"
                           "docs/*.info" "docs/*.texi" "docs/*.texinfo"
                           (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                     "LICENSE" "README*" "*-pkg.el"))
                          :source #2# :protocol https :inherit t :depth treeless :ref
                          "9d6f0f9635ae722b6bd943a76e996f54443e373f"))
 (ef-themes :source #1# :recipe
            (:package "ef-themes" :repo ("https://github.com/protesilaos/ef-themes" . "ef-themes")
                      :files
                      ("*" (:exclude ".git" "COPYING" "doclicense.texi" "contrast-ratios.org"))
                      :source #2# :protocol https :inherit t :depth treeless :wait t :ref
                      "5f81c603157b8584740c1af2b744d2b9780affc8"))
 (doom-themes :source #1# :recipe
              (:package "doom-themes" :fetcher github :repo "doomemacs/themes" :files
                        (:defaults "themes/*.el" "themes/*/*.el" "extensions/*.el") :source #2#
                        :protocol https :inherit t :depth treeless :wait t :ref
                        "e506a8724156da3b1e62cb8136265e9705549d04"))
 (quick-peek :source #1# :recipe
             (:package "quick-peek" :fetcher github :repo "cpitclaudel/quick-peek" :files
                       ("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo" "doc/dir" "doc/*.info"
                        "doc/*.texi" "doc/*.texinfo" "lisp/*.el" "docs/dir" "docs/*.info"
                        "docs/*.texi" "docs/*.texinfo"
                        (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"
                                  "LICENSE" "README*" "*-pkg.el"))
                       :source #2# :protocol https :inherit t :depth treeless :ref
                       "03a276086795faad46a142454fc3e28cab058b70"))
 (elpaca-use-package :source #1# :recipe
                     (:package "elpaca-use-package" :wait t :repo
                               "https://github.com/progfolio/elpaca.git" :files
                               ("extensions/elpaca-use-package.el") :main
                               "extensions/elpaca-use-package.el" :build (:not elpaca--compile-info)
                               :source #2# :protocol https :inherit t :depth treeless :ref
                               "ab62a34254570b430dc5bd1d03f16dfe74bd279e"))
 (elpaca :source
   #1# :recipe
   (:source nil :protocol https :inherit ignore :depth 1 :repo
            "https://github.com/progfolio/elpaca.git" :ref
            "ab62a34254570b430dc5bd1d03f16dfe74bd279e" :files
            (:defaults "elpaca-test.el" (:exclude "extensions")) :build
            (:not elpaca--activate-package) :package "elpaca")))
