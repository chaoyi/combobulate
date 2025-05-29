;;; combobulate-cpp.el --- C++ support for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Your Name (or Mickey Petersen, if adapting)

;; Author: Your Name <your.email@example.com> (or Mickey Petersen)
;; Keywords: c++, languages, editing

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

;; Combobulate support for C++.
;; This is a best-effort initial implementation based on common C++ constructs
;; and a sample tree-sitter CST. Due to C++'s complexity, further refinement
;; for specific cases and advanced features will likely be necessary.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-manipulation)
(require 'combobulate-rules)
(require 'combobulate-procedure)

(defgroup combobulate-cpp nil
  "Configuration switches for C++."
  :group 'combobulate
  :prefix "combobulate-cpp-")

(defun combobulate-cpp--get-declarator-name (node)
  "Extract name from a declarator node."
  (when node
    (cond
     ((member (combobulate-node-type node) '("identifier" "type_identifier" "namespace_identifier" "field_identifier" "destructor_name"))
      (combobulate-node-text node))
     ((string= (combobulate-node-type node) "qualified_identifier")
      (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name"))
          (combobulate-node-text node))) ; Fallback for simpler qualified_id
     ((string= (combobulate-node-type node) "template_function")
      (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name")))
     ((string= (combobulate-node-type node) "function_declarator")
      (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "declarator")))
     ((string= (combobulate-node-type node) "pointer_declarator")
      (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "declarator")))
     ((string= (combobulate-node-type node) "reference_declarator")
      (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "declarator")))
     ((string= (combobulate-node-type node) "array_declarator")
      (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "declarator")))
     ((string= (combobulate-node-type node) "init_declarator")
      (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "declarator")))
     (t ""))))

(defun combobulate-cpp-pretty-print-node-name (node default-name)
  "Pretty printer for C++ nodes."
  (combobulate-string-truncate
   (replace-regexp-in-string
    (rx (| (>= 2 " ") "\n")) ""
    (pcase (combobulate-node-type node)
      ("function_definition"
       (let* ((declarator-node (combobulate-node-child-by-field node "declarator"))
              (name (or (combobulate-cpp--get-declarator-name declarator-node) ""))
              (params-node (and declarator-node (combobulate-node-child-by-field declarator-node "parameters")))
              (params (if params-node (combobulate-node-text params-node) "()")))
         ;; Try to get return type for context if name is empty (e.g. lambda)
         (if (string-empty-p name)
             (if-let ((type-node (combobulate-node-child-by-field node "type")))
                 (concat (combobulate-node-text type-node) " " params " {lambda}")
               (concat "lambda " params))
           (concat name params))))
      ("class_specifier"
       (concat "class " (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name")) "")))
      ("struct_specifier"
       (concat "struct " (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name")) "")))
      ("enum_specifier"
       (concat "enum " (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name")) "")))
      ("namespace_definition"
       (concat "namespace " (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name")) "")))
      ("alias_declaration"
       (concat "using " (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "name")) "")))
      ("type_definition" ; typedef
       (concat "typedef " (or (combobulate-cpp--get-declarator-name (combobulate-node-child-by-field node "declarator")) "")))
      ("declaration" ; Global variables or other declarations
       (let ((type-node (combobulate-node-child-by-field node "type"))
             (declarator-node (combobulate-node-child-by-field node "declarator")))
         (concat (if type-node (combobulate-node-text type-node) "")
                 " "
                 (or (combobulate-cpp--get-declarator-name declarator-node) ""))))
      ("template_declaration" "template <...>")
      ((or "identifier" "type_identifier" "field_identifier" "namespace_identifier") (combobulate-node-text node))
      (_ default-name)))
   80))

(eval-and-compile
  (defconst combobulate-cpp-definitions
    '((pretty-print-node-name-function #'combobulate-cpp-pretty-print-node-name)
      (plausible-separators '("," ";"))
      (procedure-discard-rules '("line_comment" "block_comment" "comment"
                                  "preproc_if" "preproc_ifdef" "preproc_ifndef"
                                  "preproc_else" "preproc_elif" "preproc_endif"
                                  "preproc_include" "preproc_def" "preproc_function_def"
                                  "preproc_call" "preproc_directive" "preproc_arg"
                                  "using_declaration" "access_specifier"))

      (procedures-defun
       '((:activation-nodes
          ((:nodes ("function_definition"
                     "class_specifier" "struct_specifier" "enum_specifier"
                     "namespace_definition" "alias_declaration" "type_definition"
                     "template_declaration"
                     ;; Top-level declarations (e.g. global variables)
                     (rule "declaration" :has-parent "translation_unit")))))))


      (procedures-logical
       '((:activation-nodes ((:nodes (all))))))

      (procedures-sibling
       `((:activation-nodes ;; Top-level items in a file
          ((:nodes ((rule "translation_unit"))
            :position at
            :has-parent ("translation_unit")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Statements in a compound statement (block)
          ((:nodes ((rule "compound_statement"))
            :position at
            :has-parent ("compound_statement")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Statements that are direct children of `if`, `for`, `while` etc.
          ((:nodes ( (rule "_statement"))
            :has-parent ("if_statement" "for_statement" "while_statement" "do_statement" "switch_statement" "case_statement" "try_statement" "catch_clause")))
          :selector (:choose parent :match-children t))


         (:activation-nodes ;; Members in a class/struct
          ((:nodes ((rule "field_declaration_list"))
            :position at
            :has-parent("field_declaration_list")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Enumerators in an enum
          ((:nodes ((rule "enumerator_list"))
            :position at
            :has-parent("enumerator_list")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Parameters in a function declaration/definition
          ((:nodes ((rule "parameter_list"))
            :position at
            :has-parent ("parameter_list")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Arguments in a function call
          ((:nodes ((rule "argument_list"))
            :position at
            :has-parent ("argument_list")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Initializers in an initializer list
          ((:nodes ((rule "initializer_list"))
            :position at
            :has-parent ("initializer_list")))
          :selector (:choose parent :match-children t))

         (:activation-nodes ;; Declarations in a namespace body
          ((:nodes ((rule "namespace_definition") (rule "declaration_list"))
            :position at
            :has-parent ("namespace_definition" "declaration_list")))
          :selector (:choose parent :match-children t))))


      (procedures-hierarchy
       `((:activation-nodes ;; Dive into function body
          ((:nodes ("function_definition") :position at))
          :selector (:choose node
                             :match-query (:query ((function_definition body: (compound_statement (_) @match))))))

         (:activation-nodes ;; Dive into class/struct body
          ((:nodes ("class_specifier" "struct_specifier") :position at))
          :selector (:choose node
                            :match-query (:query ([(class_specifier) (struct_specifier)] body: (field_declaration_list (_) @match)))))

         (:activation-nodes ;; Dive into enum body
          ((:nodes ("enum_specifier") :position at))
          :selector (:choose node
                            :match-query (:query ((enum_specifier body: (enumerator_list (_) @match))))))

         (:activation-nodes ;; Dive into namespace body
          ((:nodes ("namespace_definition") :position at))
          ;; Namespace body can be a compound_statement or directly contain declarations.
          ;; The CST you provided shows direct declarations, sometimes wrapped in declaration_list
          :selector (:choose node
                            :match-query (:query ((namespace_definition body: [(compound_statement (_) @match) (declaration_list (_) @match) ((_) @match)])))))

         (:activation-nodes ;; Dive into if/else/for/while/do/switch bodies
          ((:nodes ("if_statement") :position at))
          :selector (:choose node
                            :match-query (:query ((if_statement consequence: [(compound_statement (_) @match) ((_) @match)])))))
         (:activation-nodes ;; if_statement alternative (else/else if)
          ((:nodes ("if_statement") :position at))
          :selector (:choose node
                            :match-query (:query ((if_statement alternative: (else_clause [(compound_statement (_) @match) (if_statement (_) @match) ((_) @match)]))))))
         (:activation-nodes
          ((:nodes ("for_statement" "while_statement" "do_statement") :position at))
          :selector (:choose node
                            :match-query (:query ([(for_statement) (while_statement) (do_statement)] body: [(compound_statement (_) @match) ((_) @match)]))))
         (:activation-nodes
          ((:nodes ("switch_statement") :position at))
          :selector (:choose node
                            :match-query (:query ((switch_statement body: (compound_statement (case_statement body: (_) @match)))))))
         (:activation-nodes ;; Dive into try/catch blocks
          ((:nodes ("try_statement") :position at))
          :selector (:choose node
                            :match-query (:query ((try_statement body: (compound_statement (_) @match))))))
         (:activation-nodes
          ((:nodes ("catch_clause") :position at))
          :selector (:choose node
                            :match-query (:query ((catch_clause body: (compound_statement (_) @match))))))

         (:activation-nodes ;; Dive into initializer of a declaration
          ((:nodes ("declaration" "field_declaration") :position at))
          :selector (:choose node
                             :match-query (:query ([(declaration) (field_declaration)] declarator: (init_declarator value: (_) @match)))))

         ;; Default fallback for hierarchy
         (:activation-nodes
          ((:nodes ((all))))
          :selector (:choose node :match-children t))))

      (context-nodes
       '("identifier" "type_identifier" "field_identifier" "namespace_identifier"
         "primitive_type" "destructor_name"
         "string_literal" "char_literal" "number_literal" "nullptr" "true" "false"
         "this" "auto" "decltype"
         "qualified_identifier" "template_type" "template_function" ; Treat these as somewhat atomic for context
         "operator_name"))


      (procedures-sequence nil)
      (display-ignored-node-types nil)
      (indent-after-edit nil)
      (indent-calculate-function nil)
      (envelope-indent-region-function #'indent-region)
      (envelope-deindent-function nil)

      (envelope-procedure-shorthand-alist
       '((statement-context ;; For things that can go inside a function body or namespace
          . ((:activation-nodes
              ((:nodes ((rule "compound_statement")
                        (rule "namespace_definition") ; Can add things to namespace
                        (rule "translation_unit") ; Top level
                        (rule "_statement")) ; After an existing statement

                       :has-parent ("compound_statement" "namespace_definition" "translation_unit"
                                     "if_statement" "for_statement" "while_statement" "do_statement"
                                     "switch_statement" "case_statement" "try_statement" "catch_clause"))))))
         (class-member-context ;; For things that can go inside a class/struct body
          . ((:activation-nodes
              ((:nodes ((rule "field_declaration_list"))
                       :has-parent ("class_specifier" "struct_specifier"))))))))

      (envelope-procedure-shorthand-default-alist nil)
      (envelope-default-list '((:key ")" :name "close-round" :template (")"))
                               (:key "}" :name "close-curly" :template ("}"))
                               (:key "]" :name "close-square" :template ("]"))))

      (envelope-list
       '(;; Basic if
         (:description "if (condition) { ... }"
          :key "i" :mark-node t :shorthand statement-context :name "if-statement"
          :template ("if (" @ (p cond "condition") ") {" n> r> n> "}" > n>))
         ;; if-else
         (:description "if (condition) { ... } else { ... }"
          :key "I" :mark-node t :shorthand statement-context :name "if-else-statement"
          :template ("if (" @ (p cond "condition") ") {" n> r> n> "} else {" n> r> n> "}" > n>))
         ;; for loop (iterator)
         (:description "for (int i = 0; i < N; ++i) { ... }"
          :key "f" :mark-node t :shorthand statement-context :name "for-loop-iterator"
          :template ("for (int " (p var "i") " = 0; " (f var) " < " (p limit "N") "; ++" (f var) ") {" n> @ r> n> "}" > n>))
         ;; range-based for loop
         (:description "for (const auto& item : container) { ... }"
          :key "F" :mark-node t :shorthand statement-context :name "for-loop-range"
          :template ("for (const auto& " @ (p item "item") " : " (p container "container") ") {" n> r> n> "}" > n>))
         ;; while loop
         (:description "while (condition) { ... }"
          :key "w" :mark-node t :shorthand statement-context :name "while-loop"
          :template ("while (" @ (p cond "condition") ") {" n> r> n> "}" > n>))
         ;; Basic function definition (void)
         (:description "void functionName(params) { ... }"
          :key "vfn" :mark-node t :shorthand (statement-context class-member-context) :name "void-function"
          :template ("void " @ (p name "functionName") "(" (p params "") ") {" n> r> n> "}" > n>))
         ;; Basic class
         (:description "class ClassName { public: ... protected: ... private: ... };"
          :key "cls" :mark-node t :shorthand statement-context :name "class-definition"
          :template("class " @ (p name "ClassName") " {" n>
                    "public:" n>
                    (p public_members "") r> n>
                    "protected:" n>
                    (p protected_members "") r> n>
                    "private:" n>
                    (p private_members "") r> n>
                    "};" > n>))
         ;; Basic struct
         (:description "struct StructName { ... };"
          :key "str" :mark-node t :shorthand statement-context :name "struct-definition"
          :template("struct " @ (p name "StructName") " {" n> r> n> "};" > n>))
         ;; try-catch block
         (:description "try { ... } catch (const std::exception& e) { ... }"
          :key "try" :mark-node t :shorthand statement-context :name "try-catch"
          :template ("try {" n> @ r> n> "} catch (const std::exception& " (p ex "e") ") {" n> (p handler "// Handle exception") r> n> "}" > n>))))


      (highlight-queries-default
       '(;; Function names in definitions
         (function_definition
          declarator: [
                       (function_declarator declarator: @function.definition)
                       (qualified_identifier name: @function.definition)
                       (template_function name: @function.definition)])

         ;; Class, struct, enum, namespace names in definitions
         (class_specifier name: @type.definition)
         (struct_specifier name: @type.definition)
         (enum_specifier name: @type.definition)
         (namespace_definition name: @namespace.definition)
         (alias_declaration name: @type.definition)
         (type_definition declarator: @type.definition) ;; typedefs
         ;; Type names (might be broad, Tree-sitter's default might be better)
         (type_identifier) @type
         (primitive_type) @type.builtin
         ;; Keywords
         (storage_class_specifier) @keyword.storage
         (type_qualifier) @keyword.modifier
         ;; Preprocessor
         (preproc_directive) @keyword.preproc
         (preproc_include path: _ ) @string.special)))) ;; To highlight the include path


  "The language definition for C++.")

(define-combobulate-language
 :name cpp
 :language cpp ; or "cpp", must match tree-sitter grammar name
 :major-modes (c++-mode c++-ts-mode cc-mode) ; Add other C++ modes if needed
 :custom combobulate-cpp-definitions
 :setup-fn #'combobulate-cpp-setup)

(defun combobulate-cpp-setup (_)
  "Setup function for C++ combobulate mode.")
  ;; Potentially set tree-sitter parser if not auto-detected,
  ;; or other C++ specific one-time setups.
  ;; (setq-local combobulate-tree-sitter-language-id 'c_plus_plus) ; If needed


(provide 'combobulate-cpp)

;;; combobulate-cpp.el ends here
