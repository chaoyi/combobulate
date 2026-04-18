;; Quick test for sibling navigation
(require 'combobulate)
(require 'combobulate-test-prelude)

(ert-deftest combobulate-test-sibling-forward ()
  "Test sibling-forward navigates to beginning of next sibling."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(a, b, c):\n    pass\n")
    ;; Position at parameter 'a'
    (goto-char (point-min))
    (search-forward "(")
    (let ((pos-a (point)))
      (should (= (char-after) ?a))
      ;; Navigate sibling forward: should go to 'b'
      (combobulate-navigate-sibling-forward)
      (let ((pos-b (point)))
        (should (= (char-after) ?b))
        (should (> pos-b pos-a))
        ;; Navigate forward again: should go to 'c'
        (combobulate-navigate-sibling-forward)
        (let ((pos-c (point)))
          (should (= (char-after) ?c))
          (should (> pos-c pos-b))
          ;; Navigate forward again: should wrap to 'a'
          (combobulate-navigate-sibling-forward)
          (should (= (point) pos-a))
          (should (= (char-after) ?a)))))))

(ert-deftest combobulate-test-sibling-backward ()
  "Test sibling-backward navigates to end of previous sibling."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(a, b, c):\n    pass\n")
    ;; Position at parameter 'b'
    (goto-char (point-min))
    (search-forward "(")
    (let ((pos-a (point)))
      (search-forward "b")
      (backward-char)  ;; at start of 'b'
      (let ((pos-b (point)))
        (should (= (char-after) ?b))
        ;; Navigate sibling backward: should land at end of 'a'
        (combobulate-navigate-sibling-backward)
        (let ((pos-end-a (point)))
          ;; End of 'a' is pos-a + 1
          (should (= pos-end-a (1+ pos-a)))
          ;; Going backward from end of 'a' at start of siblings
          ;; should wrap to end of 'c'
          (combobulate-navigate-sibling-backward)
          (should (> (point) pos-b)))))))

(ert-deftest combobulate-test-sibling-backward-repeated ()
  "Test repeated C-M-p cycles through all siblings correctly."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(alpha, beta, gamma):\n    pass\n")
    ;; Position at 'gamma'
    (goto-char (point-min))
    (search-forward "gamma")
    (backward-char 5)
    (should (= (char-after) ?g))
    ;; 1st C-M-p: end of beta
    (combobulate-navigate-sibling-backward)
    (should (= (char-before) ?a)) ;; 'a' of "beta"
    (let ((end-beta (point)))
      ;; 2nd C-M-p: end of alpha
      (combobulate-navigate-sibling-backward)
      (should (= (char-before) ?a)) ;; 'a' of "alpha"
      (should (< (point) end-beta))
      ;; 3rd C-M-p: wrap to end of gamma
      (combobulate-navigate-sibling-backward)
      (should (= (char-before) ?a)) ;; 'a' of "gamma"
      (should (> (point) end-beta)))))

(ert-deftest combobulate-test-forward-sexp ()
  "Test forward-sexp from start of node goes to end of same node."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(a, b, c):\n    pass\n")
    ;; Position at parameter 'a'
    (goto-char (point-min))
    (search-forward "(")
    (let ((pos-a (point)))
      (should (= (char-after) ?a))
      ;; Forward-sexp from start of 'a': should go to end of 'a'
      (combobulate-navigate-forward-sexp)
      ;; End of 'a' is pos-a + 1
      (should (= (point) (1+ pos-a))))))

(ert-deftest combobulate-test-forward-sexp-repeated ()
  "Test repeated C-M-f: start->end->next-end->next-end->up."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(alpha, beta, gamma):\n    pass\n")
    ;; Position at 'alpha'
    (goto-char (point-min))
    (search-forward "(")
    (let ((pos-alpha (point)))
      (should (= (char-after) ?a))
      ;; 1st C-M-f: start of alpha -> end of alpha
      (combobulate-navigate-forward-sexp)
      (should (= (char-before) ?a))  ;; last char of "alpha"
      (let ((end-alpha (point)))
        (should (> end-alpha pos-alpha))
        ;; 2nd C-M-f: end of alpha -> end of beta
        (combobulate-navigate-forward-sexp)
        (should (= (char-before) ?a))  ;; last char of "beta"
        (let ((end-beta (point)))
          (should (> end-beta end-alpha))
          ;; 3rd C-M-f: end of beta -> end of gamma
          (combobulate-navigate-forward-sexp)
          (should (= (char-before) ?a))  ;; last char of "gamma"
          (let ((end-gamma (point)))
            (should (> end-gamma end-beta))
            ;; 4th C-M-f: end of gamma (last sibling) -> go up to parent end
            (combobulate-navigate-forward-sexp)
            (should (> (point) end-gamma))))))))

(ert-deftest combobulate-test-backward-sexp ()
  "Test backward-sexp from end of node goes to start of same node."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(alpha, beta, gamma):\n    pass\n")
    ;; Position at end of 'beta' (after last char)
    (goto-char (point-min))
    (search-forward "beta")
    (let ((end-beta (point)))
      ;; Backward-sexp from end of beta -> start of beta
      (combobulate-navigate-backward-sexp)
      (should (= (char-after) ?b))
      (should (< (point) end-beta))
      (let ((start-beta (point)))
        ;; Backward-sexp from start of beta -> start of alpha
        (combobulate-navigate-backward-sexp)
        (should (= (char-after) ?a))
        (should (< (point) start-beta))))))

(ert-deftest combobulate-test-forward-sexp-go-up ()
  "Test forward-sexp goes up when at end of last sibling."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "def foo(a, b, c):\n    pass\n")
    ;; Position at 'c', then go to its end
    (goto-char (point-min))
    (search-forward "c")
    ;; Now at end of 'c' (search-forward leaves point after match)
    (let ((end-c (point)))
      (combobulate-navigate-forward-sexp)
      ;; Should go up to parent
      (should (> (point) end-c)))))

(ert-deftest combobulate-test-sibling-forward-statements ()
  "Test sibling-forward works for statement-level navigation."
  (combobulate-test
      (:language python :mode python-ts-mode)
    (insert "x = 1\ny = 2\nz = 3\n")
    (goto-char (point-min))
    ;; At 'x = 1' statement
    (let ((pos-1 (point)))
      (combobulate-navigate-sibling-forward)
      (let ((pos-2 (point)))
        (should (> pos-2 pos-1))
        (combobulate-navigate-sibling-forward)
        (let ((pos-3 (point)))
          (should (> pos-3 pos-2))
          ;; Wrap around
          (combobulate-navigate-sibling-forward)
          (should (= (point) pos-1)))))))
