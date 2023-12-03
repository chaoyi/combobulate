;;; combobulate-cpp.el --- cpp-specific features for combobulate  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Mickey Petersen

;; Author: Mickey Petersen <mickey at masteringemacs.org>
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

;;

;;; Code:

(defun combobulate-setup-cpp (_)
  (setq combobulate-navigation-default-nodes `("translation_unit"
                                               "namespace_definition"
                                               "function_definition"
                                               "class_specifier"
                                               "alias_declaration"
                                               "using_declaration"
                                               "field_declaration"
                                               "template_declaration"
                                               "declaration"
                                               "attributed_statement"
                                               "co_return_statement"
                                               "co_yield_statement"
                                               "compound_statement"
                                               "expression_statement"
                                               "if_statement"
                                               "return_statement"
                                               "switch_statement"
                                               "throw_statement"
                                               "try_statement"
                                               "while_statement"
                                               "case_statement"
                                               "break_statement"
                                               "preproc_if"
                                               "preproc_ifdef"
                                               "preproc_include"
                                               "preproc_def"
                                               "preproc_function_def"
                                               "preproc_else"
                                               "preproc_elif")))

(provide 'combobulate-cpp)
;;; combobulate-cpp.el ends here
