((nil
  . ((compile-command . "cmake -B build; cd build; make -j4")

     (eglot-workspace-configuration
      . (:purescript (:outputDirectory "./src/site/output")))))

 (auto-mode-alist
  . (("\\.clangd\\'" . yaml-ts-mode)
     ("\\.clang-format\\'" . python-ts-mode)))

 (c++-ts-mode
  . ((c-ts-mode-indent-offset . 4))))
