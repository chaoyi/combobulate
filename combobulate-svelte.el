;;; combobulate-svelte.el --- Svelte structured editing for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
;; Package-Requires: ((emacs "29"))
;; Version: 0.1
;; Homepage: https://www.github.com/mickeynp/combobulate
;; Keywords: convenience, tools, languages

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

;; Svelte support for Combobulate. Uses the Himujjal/tree-sitter-svelte
;; grammar which svelte-ts-mode depends on.
;;
;; Svelte buffers are multi-language: the svelte parser handles the
;; template, while typescript/javascript and css parsers handle the
;; embedded <script> and <style> blocks. This module adds cross-language
;; navigation so `combobulate-navigate-up' can escape from an embedded
;; language back to the svelte tree.

;;; Code:

(require 'combobulate-settings)
(require 'combobulate-navigation)
(require 'combobulate-setup)
(require 'combobulate-rules)
(require 'combobulate-html)

(defun combobulate-svelte-pretty-print (node default-name)
  (cond
   ((and node (member (combobulate-node-type node) '("element" "script_element" "style_element")))
    (format "<%s>" (thread-first node
                                 (combobulate-node-child 0)
                                 (combobulate-node-child 0)
                                 (combobulate-node-text))))
   ((and node (equal (combobulate-node-type node) "if_statement"))
    "{#if ...}")
   ((and node (equal (combobulate-node-type node) "each_statement"))
    "{#each ...}")
   ((and node (equal (combobulate-node-type node) "await_statement"))
    "{#await ...}")
   ((and node (equal (combobulate-node-type node) "key_statement"))
    "{#key ...}")
   ((and node (equal (combobulate-node-type node) "snippet_statement"))
    "{#snippet ...}")
   ((and node (equal (combobulate-node-type node) "render_expr"))
    "{@render ...}")
   ((and node (equal (combobulate-node-type node) "html_expr"))
    "{@html ...}")
   ((and node (equal (combobulate-node-type node) "const_expr"))
    "{@const ...}")
   ((and node (equal (combobulate-node-type node) "debug_expr"))
    "{@debug ...}")
   ((and node (equal (combobulate-node-type node) "text"))
    (format "`%s'" (combobulate-node-text node)))
   ((and node (equal (combobulate-node-type node) "comment"))
    (combobulate-node-text node))
   (t default-name)))

;; Node types from Himujjal/tree-sitter-svelte grammar (used by svelte-ts-mode):
;;
;; Control flow blocks use *_expr suffixes:
;;   if:      if_statement, if_start_expr, else_if_statement, else_if_expr,
;;            else_statement, else_expr, if_end_expr
;;   each:    each_statement, each_start_expr, else_each_statement, each_end_expr
;;   await:   await_statement, await_start_expr, then_statement, then_expr,
;;            catch_statement, catch_expr, await_end_expr
;;   key:     key_statement, key_start_expr, key_end_expr
;;   snippet: snippet_statement, snippet_start_expr, snippet_end_expr
;;
;; Tag expressions: render_expr, html_expr, const_expr, debug_expr
;; Inline: expression

(eval-and-compile
  (defvar combobulate-svelte-definitions
    '((procedures-sexp
       '((:activation-nodes
          ((:nodes ("element" "attribute" "text" "script_element" "style_element"
                    "if_statement" "each_statement" "await_statement"
                    "key_statement" "snippet_statement" "expression"
                    "render_expr" "html_expr" "const_expr" "debug_expr"))))))
      (context-nodes '("attribute_name" "attribute_value" "tag_name" "text"))
      (envelope-list
       '((:description
          "<...> ... </...>"
          :name "tag"
          :mark-node t
          :nodes ("element" "self_closing_tag" "script_element" "style_element" "text"
                  "if_statement" "each_statement" "await_statement" "key_statement"
                  "render_expr" "html_expr" "const_expr" "debug_expr")
          :key "t"
          :template ("<" (p tag "Tag Name: ") ">" n>
                     r>
                     n> "</" (field tag) ">"))
         (:description
          "{#if ...} ... {/if}"
          :name "if-block"
          :mark-node t
          :nodes ("element" "self_closing_tag" "text" "if_statement" "each_statement"
                  "expression" "script_element" "style_element"
                  "render_expr" "html_expr" "const_expr" "debug_expr")
          :key "i"
          :template ("{#if " @ "}" n>
                     r>
                     n> "{/if}"))
         (:description
          "{#each ... as ...} ... {/each}"
          :name "each-block"
          :mark-node t
          :nodes ("element" "self_closing_tag" "text" "if_statement" "each_statement"
                  "expression" "script_element" "style_element"
                  "render_expr" "html_expr" "const_expr" "debug_expr")
          :key "e"
          :template ("{#each " (p items "Items") " as " (p item "Item") "}" n>
                     r>
                     n> "{/each}"))
         (:description
          "{#await ...} ... {/await}"
          :name "await-block"
          :mark-node t
          :nodes ("element" "self_closing_tag" "text" "expression"
                  "render_expr" "html_expr" "const_expr" "debug_expr")
          :key "a"
          :template ("{#await " @ "}" n>
                     r>
                     n> "{/await}"))
         (:description
          "{#key ...} ... {/key}"
          :name "key-block"
          :mark-node t
          :nodes ("element" "self_closing_tag" "text" "expression"
                  "render_expr" "html_expr" "const_expr" "debug_expr")
          :key "k"
          :template ("{#key " @ "}" n>
                     r>
                     n> "{/key}"))
         (:description
          "{...}"
          :name "expression"
          :mark-node t
          :nodes ("element" "self_closing_tag" "text")
          :key "{"
          :template ("{" r "}"))
         (:description
          "...=\" ... \""
          :key "=s"
          :mark-node nil
          :point-placement 'stay
          :nodes ("start_tag" "self_closing_tag")
          :name "attr-string"
          :template ("=" "\"" @ "\""))
         (:description
          "...={ ... }"
          :key "=e"
          :mark-node nil
          :point-placement 'stay
          :nodes ("start_tag" "self_closing_tag")
          :name "attr-expression"
          :template ("=" "{" @ "}"))))
      (procedures-sequence
       '((:activation-nodes
          ((:nodes ("tag_name") :position any :has-ancestor ("element")))
          :selector (:choose parent :match-query
                             (:query
                              (_ (start_tag (tag_name) @match)
                                 (end_tag (tag_name) @match))
                              :engine combobulate)))))
      (procedures-hierarchy
       '(;; navigate between elements and their children
         (:activation-nodes
          ((:nodes ("element" "script_element" "style_element"
                    "if_statement" "each_statement" "await_statement"
                    "key_statement" "snippet_statement"
                    "else_if_statement" "else_statement" "else_each_statement"
                    "then_statement" "catch_statement")
                   :position at))
          :selector (:choose node :match-children
                             (:discard-rules ("start_tag" "end_tag" "tag_name"
                                              "if_start_expr" "if_end_expr"
                                              "else_if_expr" "else_expr"
                                              "each_start_expr" "each_end_expr"
                                              "await_start_expr" "await_end_expr"
                                              "then_expr" "catch_expr"
                                              "key_start_expr" "key_end_expr"
                                              "snippet_start_expr" "snippet_end_expr"))))
         ;; tag expressions like {@render}, {@html}, {@const}, {@debug}
         ;; these are navigable parents so navigate-up works from inside them
         (:activation-nodes
          ((:nodes ("render_expr" "html_expr" "const_expr" "debug_expr")
                   :position at))
          :selector (:choose node :match-children t))
         ;; go into attribute if point is inside the start tag
         (:activation-nodes
          ((:nodes ("start_tag" "self_closing_tag") :position in))
          :selector (:choose node
                             :match-children
                             (:match-rules ("attribute"))))
         ;; if we're inside an attribute, go to its value
         (:activation-nodes
          ((:nodes ("attribute") :position in))
          :selector (:choose node :match-children t))))
      (procedure-discard-rules nil)
      (display-ignored-node-types '("start_tag" "self_closing_tag" "end_tag" "tag_name"
                                    "if_start_expr" "if_end_expr"
                                    "else_if_expr" "else_expr"
                                    "each_start_expr" "each_end_expr"
                                    "await_start_expr" "await_end_expr"
                                    "then_expr" "catch_expr"
                                    "key_start_expr" "key_end_expr"
                                    "snippet_start_expr" "snippet_end_expr"))
      (procedures-sibling
       '((:activation-nodes
          ((:nodes
            ("attribute")))
          :selector (:choose node :match-siblings (:match-rules ("attribute"))))
         (:activation-nodes
          ((:nodes
            ((rule "document") "comment" "element" "text" "expression"
             "if_statement" "each_statement" "await_statement"
             "key_statement" "snippet_statement"
             "else_if_statement" "else_statement" "else_each_statement"
             "then_statement" "catch_statement"
             "render_expr" "html_expr" "const_expr" "debug_expr"
             "script_element" "style_element")
            :has-parent ((rule "document") "document" "element"
                         "if_statement" "each_statement" "await_statement"
                         "key_statement" "snippet_statement"
                         "else_if_statement" "else_statement" "else_each_statement"
                         "then_statement" "catch_statement")))
          :selector (:match-children (:match-rules
                                      (exclude (all) "start_tag" "end_tag" "self_closing_tag"
                                               "if_start_expr" "if_end_expr"
                                               "else_if_expr" "else_expr"
                                               "each_start_expr" "each_end_expr"
                                               "await_start_expr" "await_end_expr"
                                               "then_expr" "catch_expr"
                                               "key_start_expr" "key_end_expr"
                                               "snippet_start_expr" "snippet_end_expr"))))))
      (pretty-print-node-name-function #'combobulate-svelte-pretty-print))))

(define-combobulate-language
 :name svelte
 :language svelte
 :major-modes (svelte-ts-mode svelte-mode)
 :custom combobulate-svelte-definitions
 :setup-fn combobulate-svelte-setup)

(defun combobulate-svelte--navigate-up-cross-language (orig-fn &rest args)
  "Advice for `combobulate-navigate-up' to cross language boundaries in svelte.
When navigation fails in an embedded language (typescript/javascript/css),
fall back to the svelte tree and navigate to the containing node."
  (let ((start-pos (point)))
    (apply orig-fn args)
    ;; If point didn't move, the inner language navigation failed.
    ;; Try to find the containing svelte node instead.
    (when (and (eq major-mode 'svelte-ts-mode)
               (= (point) start-pos)
               (not (eq (treesit-language-at (point)) 'svelte)))
      (let ((svelte-node (treesit-node-at (point) 'svelte)))
        ;; Walk up to find a navigable svelte container (script_element or style_element)
        (while (and svelte-node
                    (not (member (treesit-node-type svelte-node)
                                 '("script_element" "style_element"))))
          (setq svelte-node (treesit-node-parent svelte-node)))
        (when svelte-node
          (goto-char (treesit-node-start svelte-node)))))))

(defun combobulate-svelte-setup (_)
  (define-key (combobulate-read map) (kbd "=") #'combobulate-maybe-insert-attribute)
  (define-key (combobulate-read map) (kbd ">") #'combobulate-maybe-auto-close-tag)
  (setq combobulate-sgml-open-tag "start_tag")
  (setq combobulate-sgml-close-tag "end_tag")
  (setq combobulate-sgml-whole-tag "element")
  (setq combobulate-sgml-self-closing-tag "self_closing_tag")
  ;; Enable cross-language navigation for navigate-up
  (advice-add 'combobulate-navigate-up :around
              #'combobulate-svelte--navigate-up-cross-language))

(provide 'combobulate-svelte)
;;; combobulate-svelte.el ends here
