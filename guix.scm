;; Flexlay - A Generic 2D Game Editor
;; Copyright (C) 2019 Ingo Ruhnke <grumbel@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(set! %load-path
      (cons* "/ipfs/QmafuNSaiqymsEvRECwQaz7TSz5hYMWiJ3X7Z3QGwuThPb/guix-cocfree_v0.0.0-37-g291d4eb"
             %load-path))

(use-modules (guix packages)
             (guix gexp)
             (guix build utils)
             (guix build-system python)
             ((guix licenses) #:prefix license:)
             (gnu packages freedesktop)
             (gnu packages python)
             (gnu packages python-xyz)
             (gnu packages qt)
             (guix-cocfree utils))

(define %source-dir (dirname (current-filename)))

(define-public flexlay
  (package
   (name "flexlay")
   (version (version-from-source %source-dir))
   (source (local-file %source-dir
                       #:recursive? #t
                       #:select? (source-select-function %source-dir)))
   (arguments
    `(#:tests? #f)) ; text_editor errors
   (inputs
    `(("python" ,python)
      ("python-pyqt" ,python-pyqt)
      ("python-numpy" ,python-numpy)
      ("python-pyxdg" ,python-pyxdg)))
   (build-system python-build-system)
   (synopsis "Generic 2d editor for games")
   (description "Flexlay is a generic 2d editor with special focus on games. It
currently supports multi layered tile-, object- and bitmaps, full
undo/redo, support for tile-brushes, easy copy/paste, multiple
buffers, minimap support, a metadata editor and some other stuff
usefull for creating levels for 2d games.")
   (home-page "https://gitlab.com/flexlay/flexlay")
   (license license:gpl3+)))

flexlay

;; EOF ;;
