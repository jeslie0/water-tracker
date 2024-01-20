((nil
  . ((compile-command
      . "cmake -B build; cd build; make -j4")

     (eglot-workspace-configuration
      . (:purescript
         (:outputDirectory "./src/site/output"
                           :formatter "purs-tidy"
                           :buildOpenedFiles t)))))

 (auto-mode-alist
  . (("\\.lock\\'"
      . yaml-ts-mode)))

 (purescript-mode
  . ((eval
      . (eglot-ensure))))

 (haskell-mode
  . ((eval
      . (eglot-ensure))))

 (nix-mode
  . ((eval
      . (eglot-ensure))))
 )
