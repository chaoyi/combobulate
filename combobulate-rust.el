;;; combobulate-rust.el --- rust support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Mickey Petersen

;; Author: Mickey Petersen <mickey@masteringemacs.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Combobulate support for Rust.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)
(require 'combobulate-procedure)

(defgroup combobulate-rust nil
  "Configuration switches for Rust."
  :group 'combobulate
  :prefix "combobulate-rust-")

(eval-and-compile
  (defconst combobulate-rust-definitions
    '((pretty-print-node-name-function #'combobulate-rust-pretty-print-node-name)
      (plausible-separators '("," ";"))
      (procedure-discard-rules '("line_comment" "block_comment" "use_declaration" "attribute_item" "visibility_modifier"))

      (procedures-defun
       '((:activation-nodes ((:nodes ("function_item" "struct_item" "enum_item" "impl_item" "trait_item" "mod_item" "macro_definition" "const_item" "static_item" "type_alias"))))))

      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))

      (procedures-sibling
       `((:activation-nodes
          ((:nodes ((rule "source_file"))
            :position at
            :has-parent ("source_file")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ((rule "block"))
            :position at
            :has-parent ("block")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
                   ((:nodes ("match_arm")
                     :position at
                     :has-parent ("match_block")))
                   :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ("declaration_list")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ("enum_variant_list")))
          :selector (:choose node :match-children t))

         (:activation-nodes
          ((:nodes ((rule "field_declaration_list"))
            :has-parent ("field_declaration_list")))
          :selector (:choose
                     parent
                     :match-children t))

         (:activation-nodes
          ((:nodes ((rule "arguments"))
            :has-parent ("arguments")))
          :selector (:choose
                     parent
                     :match-children t))))

      (procedures-hierarchy
       `((:activation-nodes
          ((:nodes ("impl_item" "mod_item")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((declaration_list (_)+ @match)))))

         (:activation-nodes
          ((:nodes ("struct_item")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((field_declaration_list (_)+ @match)))))

         (:activation-nodes
          ((:nodes ("enum_item")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((enum_variant_list (_)+ @match)))))

         (:activation-nodes
          ((:nodes ("function_item")
            :position at))
          :selector (:choose node
                     :match-query
                     (:discard-rules ()
                      :query ((function_item body: (block (_) @match))))))

         (:activation-nodes
          ((:nodes ("let_declaration")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((let_declaration value: (_) @match)))))

         (:activation-nodes
          ((:nodes ("match_arm")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((match_arm value: (_) @match)))))

         (:activation-nodes
          ((:nodes ("match_expression")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((match_expression body: (match_block (match_arm) @match))))))

         (:activation-nodes
          ((:nodes ("if_expression")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((if_expression consequence: (block (_) @match))))))

         (:activation-nodes
          ((:nodes ("for_expression")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((for_expression body: (block (_) @match))))))

         (:activation-nodes
          ((:nodes ("closure_expression")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((closure_expression body: (_) @match)))))

         ;; need to think through below

         ;; (:activation-nodes
         ;;  ((:nodes ("call_expression")
         ;;    :position any))
         ;;  :selector (:choose node
         ;;             :match-query
         ;;             (:query ((call_expression function: (_) @match)))))

         ;; (:activation-nodes
         ;;  ((:nodes ("field_expression")
         ;;    :position any))
         ;;  :selector (:choose node
         ;;             :match-query
         ;;             (:query ((field_expression value: (_) @match)))))

         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t))))

      (context-nodes ;; Nodes containing semantic information
       '("identifier" "type_identifier" "field_identifier" "shorthand_field_initializer" "self"
         "primitive_type" "mutable_specifier"
         "string_literal" "char_literal" "integer_literal" "float_literal" "boolean_literal"
         "crate")) ;; for use crate::

      (procedures-sequence nil) ;; Optional: e.g. for HTML tags start/end
      (display-ignored-node-types nil)
      (indent-after-edit nil) ;; Usually false for non-whitespace sensitive
      (indent-calculate-function nil) ;; For whitespace-sensitive like Python
      (envelope-indent-region-function #'indent-region) ;; Default is usually fine
      (envelope-deindent-function nil) ;; For whitespace-sensitive

      (envelope-procedure-shorthand-alist nil)
      (envelope-procedure-shorthand-default-alist nil)
      (envelope-default-list '((:key ")" :name "close-round" :template (")")))) ; Default M-)
      (envelope-list nil) ;; Add Rust-specific envelopes later

      (highlight-queries-default nil))) ;; Add helpful highlights later

  "The language definition for Rust.")

(define-combobulate-language
 :name rust
 :language rust
 :major-modes (rust-mode rust-ts-mode rustic-mode)
 :custom combobulate-rust-definitions
 :setup-fn #'combobulate-rust-setup)

(defun combobulate-rust-setup (_))

(provide 'combobulate-rust)

;;; combobulate-rust.el ends here
