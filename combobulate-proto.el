;;; combobulate-proto.el --- Protocol Buffers support for combobulate -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Mickey Petersen

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

;; Combobulate support for Protocol Buffers (proto2 and proto3).

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)
(require 'combobulate-procedure)

(defgroup combobulate-proto nil
  "Configuration switches for Protocol Buffers."
  :group 'combobulate
  :prefix "combobulate-proto-")

(eval-and-compile
  (defconst combobulate-proto-definitions
    `((pretty-print-node-name-function #'combobulate-proto-pretty-print-node-name)
      (plausible-separators '("," ";"))
      (procedure-discard-rules '("comment" "empty_statement"))

      ;; Main top-level definitions for C-M-a/e/h.
      (procedures-defun
       `((:activation-nodes
          ((:nodes ("message" "enum" "service" "extend"))))))

      (procedures-logical
       `((:activation-nodes ((:nodes (all))))))

      (procedures-sibling
       `(
         (:activation-nodes
          ((:nodes ((rule "source_file"))
            :position at
            :has-parent ("source_file")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ((rule "message_body"))
            :position at
            :has-parent ("message_body")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ((rule "enum_body"))
            :position at
            :has-parent ("enum_body")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ((rule "oneof"))
            :position at
            :has-parent ("oneof")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ((rule "service"))
            :position at
            :has-parent ("service")))
          :selector (:choose parent :match-children t))

         (:activation-nodes
          ((:nodes ((rule "rpc"))
            :position at
            :has-parent ("rpc")))
          :selector (:choose parent :match-children t))))

      ;; Hierarchical navigation for "drilling down" into definitions.
      (procedures-hierarchy
       '((:activation-nodes
          ((:nodes ("message")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((message_body (_) @match)))))

         (:activation-nodes
          ((:nodes ("enum")
            :position at))
          :selector (:choose node
                     :match-query
                     (:query ((enum_body (_) @match)))))

         (:activation-nodes
          ((:nodes ("service")
            :position at))
          :selector (:choose node
                     ;; Service bodies are anonymous blocks in the grammar, so
                     ;; we find the first child that isn't the name.
                     :match-children (:match-rules ((exclude (all) ("service_name"))))))

         (:activation-nodes
          ((:nodes ("oneof")
            :position at))
          :selector (:choose node
                     ;; A oneof is similar to a service in this regard.
                     :match-children (:match-rules ((exclude (all) ("identifier"))))))

         (:activation-nodes
          ((:nodes ("rpc")
            :position at))
          :selector (:choose node
                     ;; Descend into the RPC's option block if it exists.
                     :match-query (:query (("{" (_) @match)))))

         ;; A sensible fallback to descend into any node.
         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t))))

      ;; Nodes that contain important semantic information.
      (context-nodes
       '("identifier" "message_name" "enum_name" "service_name" "rpc_name"
         "type" "key_type" "message_or_enum_type" "full_ident"
         "string" "int_lit" "float_lit" "bool" "field_number"))

      ;; Sexp is not applicable to Protocol Buffers.
      (procedures-sequence nil)

      ;; ---- Boilerplate ----
      (display-ignored-node-types nil)
      (indent-after-edit nil)
      (indent-calculate-function nil)
      (envelope-indent-region-function #'indent-region)
      (envelope-deindent-function nil)
      (envelope-procedure-shorthand-alist nil)
      (envelope-procedure-shorthand-default-alist nil)
      (envelope-default-list '((:key ")" :name "close-round" :template (")")))) ; Default M-)
      (envelope-list nil)
      (highlight-queries-default nil)))

  "The language definition for Protocol Buffers.")

;; The main macro to register the language with Combobulate.
(define-combobulate-language
 :name proto
 :language proto
 ;; Add any major modes used for .proto files here.
 :major-modes (protobuf-mode)
 :custom combobulate-proto-definitions
 :setup-fn #'combobulate-proto-setup)

(defun combobulate-proto-setup (_)
  "Setup function for proto-mode buffers.")
  ;; Future-proofing: any special setup can go here.


(defun combobulate-proto-pretty-print-node-name (node default-name)
  "Pretty-print NODE's name representation.
This is a placeholder. You can enhance it later to show more
context, e.g., 'field: string name' instead of just 'field'."
  default-name)


(provide 'combobulate-proto)

;;; combobulate-proto.el ends here
