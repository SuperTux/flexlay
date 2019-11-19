;; Feuerkraft - A Tank Battle Game
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
      (cons "/ipfs/QmXu4FXpCvsSs853mdtxv6HUBGvZXArMruLveTZExvtdKr/guix-cocfree_v0.0.0-42-gf296b6f"
            %load-path))

(use-modules (guix build utils)
             (guix build-system scons)
             (guix git-download)
             (guix gexp)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (gnu packages compression)
             (gnu packages gl)
             (gnu packages pkg-config)
             (gnu packages swig)
             (gnu packages ruby)
             (guix-cocfree utils)
             (guix-cocfree packages clanlib))

(define %source-dir (dirname (current-filename)))

(define-public flexlay-classic
  (package
   (name "flexlay-classic")
   (version (version-from-source %source-dir))
   (source (local-file %source-dir
                       #:recursive? #t
                       #:select? (source-select-function %source-dir)))
   (build-system scons-build-system)
   (arguments
    `(#:phases
      (modify-phases
       %standard-phases
       (replace 'install
                (lambda* (#:key outputs #:allow-other-keys)
                         (let* ((out (assoc-ref outputs "out")))
                           (invoke "make" "install" (string-append "PREFIX=" out))))))))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (inputs
    `(("clanlib-1.0" ,clanlib-1.0)
      ("mesa" ,mesa)
      ("glu" ,glu)
      ("swig" ,swig)
      ("ruby" ,ruby)
      ("zlib" ,zlib)))
   (synopsis "Generic 2d editor for games")
   (description "Flexlay is a generic 2d editor with special focus on games. It
currently supports multi layered tile-, object- and bitmaps, full
undo/redo, support for tile-brushes, easy copy/paste, multiple
buffers, minimap support, a metadata editor and some other stuff
usefull for creating levels for 2d games.

flexlay-classic is an older version based on ClanLib that has support
for netPanzer, which is currently missing in the latest version.")
   (home-page "https://gitlab.com/Flexlay/flexlay")
   (license license:gpl3+)))

flexlay-classic

;; EOF ;;
