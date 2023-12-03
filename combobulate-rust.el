;;; combobulate-rust.el --- python-specific features for combobulate  -*- lexical-binding: t; -*-

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

(defun combobulate-setup-rust (_)
  (setq combobulate-navigation-default-nodes `("source_file"
                                               "use_declaration"
                                               "const_item"
                                               "macro_definition"
                                               "mod_item"
                                               "foreign_mod_item"
                                               "struct_item"
                                               "union_item"
                                               "enum_item"
                                               "type_item"
                                               "function_item"
                                               "function_signature_item"
                                               "impl_item"
                                               "trait_item"
                                               "static_item")))

(provide 'combobulate-rust)
;;; combobulate-rust.el ends here
