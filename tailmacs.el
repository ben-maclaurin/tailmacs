;;; tailmacs.el --- Manage your tailnet in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Free Software Foundation, Inc.

;; Author: Ben MacLaurin <ben@maclaurin.email>
;; Maintainer: Ben MacLaurin <ben@maclaurin.email>
;; Created: 2024
;; Version: 0.01
;; Package-Requires: ((emacs "29") (magit "3.3.0") (plz "0.7"))
;; Homepage: https://github.com/ben-maclaurin/tailmacs
;; Keywords: network, files, tailscale, tramp, transient

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; tailmacs is a magit-inspired interface for managing your tailnet in
;; Emacs. The UI is transient-driven and depends on TRAMP for remote
;; connections.

;;; Code:

(require 'transient)
(require 'plz)
(require 'json)

;; == Tailmacs ==

(defgroup tailmacs nil
  "Tailmacs"
  :prefix "tailmacs-"
  :group 'applications)

(defcustom tailmacs-access-token ""
  "Used to retrieve device list via API."
  :group 'tailmacs
  :type 'string)

(defcustom tailmacs-organization ""
  "Usually the e-mail associated with your Tailscale account."
  :group 'tailmacs
  :type 'string)

;;;###autoload
(transient-define-prefix tailmacs ()
  "Begin using Tailmacs."
  ["Target" ("m" "Machine" "machine=" :choices tailmacs--device-names :always-read t)]
  ["TRAMP" ("t" "Connect to machine via TRAMP" (lambda () (interactive) (transient-save) (tailmacs-tramp)) :transient t)]
  ["Commands" ("s" "Serve content and local servers on your tailnet" tailmacs-serve) ("f" "Serve content and local servers on the internet" tailmacs-funnel)]
  ["Visit" ("o" "Visit MagicDNS URL in default browser" tailmacs--browse-magic-dns-url) ("e" "Visit MagicDNS URL in EWW" tailmacs--eww-browse-magic-dns-url)])

(transient-define-suffix tailmacs--browse-magic-dns-url (args)
  :transient t
  (interactive (list (transient-args 'tailmacs)))
  (let ((args (transient-args (oref transient-current-prefix command))))
    (browse-url (concat "https://" (transient-arg-value "machine=" (transient-args transient-current-command)) "." (tailmacs--magic-dns-name)))))

(transient-define-suffix tailmacs--eww-browse-magic-dns-url (args)
  :transient t
  (interactive (list (transient-args 'tailmacs)))
  (let ((args (transient-args (oref transient-current-prefix command))))
    (eww (concat "https://" (transient-arg-value "machine=" (transient-args transient-current-command)) "." (tailmacs--magic-dns-name)))))
              
;; == Utility ==

(defun tailmacs--device-names ()
  (mapcar (lambda (device)
            (let ((domain-name (alist-get 'name device)))
              (car (split-string domain-name "\\."))))
          (tailmacs--api-get-devices)))

(defun tailmacs--device-names-long ()
  (mapcar (lambda (device)
            (let ((domain-name (alist-get 'name device)))
              domain-name))
          (tailmacs--api-get-devices)))

(defun tailmacs--magic-dns-name ()
  (interactive)
  (string-join (cdr (split-string (car (tailmacs--device-names-long)) "\\.")) "."))

(defun tailmacs--shell-command-on-remote-machine (machine command)
  (let ((default-directory (expand-file-name (concat "/ssh:" "root@" machine ":~/"))))
    (with-connection-local-variables
     (message "%s" (shell-command-to-string command)))))

(defun tailmacs--run (tailscale-command filename transient-args)
  (let ((https-arg (transient-arg-value "--https=" (transient-args transient-current-command)))
	(http-arg (transient-arg-value "--http=" (transient-args transient-current-command)))
	(tcp-arg (transient-arg-value "--tcp=" (transient-args transient-current-command)))
	(tls-arg (transient-arg-value "--tls-terminated-tcp=" (transient-args transient-current-command))))
    (tailmacs--shell-command-on-remote-machine
     (transient-arg-value "machine=" (transient-args 'tailmacs))
     (concat tailscale-command " "
	     (if https-arg
		 (concat "--https " https-arg " ")
	       "")
	     (if http-arg
		 (concat "--http " http-arg " ")
	       "")
	     (if tcp-arg
		 (concat "--tcp " tcp-arg " ")
	       "")
	     (if tls-arg
		 (concat "--tls-terminated-tcp " tls-arg " ")
	       "")
	     (clean-file-path filename)))))

(defun clean-file-path (path)
 (replace-regexp-in-string (concat "/ssh:" "root@" (transient-arg-value "machine=" (transient-args 'tailmacs)) ":") "" path))

;; == Tramp ==

(transient-define-prefix tailmacs-tramp ()
  "Open transient menu for Tailmacs TRAMP."
  :value '("user=root" "dir=~/")
  ["Arguments"
   ("-u" "user" "user=")
   ("-d" "directory" "dir=")]
  
  ["Actions"
   ("t" "tramp" tailmacs--tramp-connect)])

(transient-define-suffix tailmacs--tramp-connect (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-tramp)))
  (let ((args (transient-args (oref transient-current-prefix command))))
    (dired (concat "/ssh:"
		   (or (transient-arg-value "user=" (transient-args transient-current-command)) "root")
		   "@" (transient-arg-value "machine=" (transient-args 'tailmacs)) ":"
		   (or (transient-arg-value "dir=" (transient-args transient-current-command)) "~/")))))

;; == Serve ==

;;;###autoload
(transient-define-prefix tailmacs-serve ()
  ["Serve command flags"
   ("-https" "Expose an HTTPS server at the specified port." "--https=")
   ("-HTTP" "Expose an HTTP server at the specified port." "--http=")
   ("-tcp" "Expose a TCP forwarder to forward TCP packets at the specified port." "--tcp=")
   ("-tls-terminated-tcp" "Expose a TCP forwarder to forward TLS-terminated TCP packets at the specified port." "--tls-terminated-tcp=")]

  ["Sub-commands" ("S" "status" tailmacs--serve-status) ("r" "reset" tailmacs--serve-reset)]

  ["Commands" ("p" "serve port" tailmacs--serve-port)]
  ["DWIM commands" ("s" "serve file at point" tailmacs--serve-file-at-point)])

;; (transient-define-suffix tailmacs--serve-file (args)
;;   :transient t
;;   (interactive (list (transient-args 'tailmacs-serve)))
;;   (let ((filename (read-file-name "Path: " (concat "/ssh:" "root@" (transient-arg-value "machine=" (transient-args 'tailmacs)) ":~/"))))
;;     (tailmacs--run "tailscale serve --bg" filename 'transient-args)))
  
(transient-define-suffix tailmacs--serve-file-at-point (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-serve)))
  (let ((filename (dired-get-filename)))
    (tailmacs--run "tailscale serve --bg" filename 'transient-args)))
         
(transient-define-suffix tailmacs--serve-port (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-serve)))
  (tailmacs--shell-command-on-remote-machine
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   (concat "tailscale serve --bg " (read-string "Port: "))))

(transient-define-suffix tailmacs--serve-reset (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-serve)))
  (tailmacs--shell-command-on-remote-machine
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   "tailscale serve reset")
  (message "Command sent"))

(transient-define-suffix tailmacs--serve-status (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-serve)))
  (tailmacs--shell-command-on-remote-machine
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   "tailscale serve status"))

;; == Funnel ==

;;;###autoload
(transient-define-prefix tailmacs-funnel ()
  ["Funnel command flags"
   ("-https" "Expose an HTTPS server at the specified port." "--https=")
   ("-HTTP" "Expose an HTTP server at the specified port." "--http=")
   ("-tcp" "Expose a TCP forwarder to forward TCP packets at the specified port." "--tcp=")
   ("-tls-terminated-tcp" "Expose a TCP forwarder to forward TLS-terminated TCP packets at the specified port." "--tls-terminated-tcp=")]

  ["Sub-commands" ("s" "status" tailmacs--funnel-status) ("r" "reset" tailmacs--funnel-reset)]

  ["Commands" ("p" "funnel port" tailmacs--funnel-port)]
  ["DWIM commands" ("f" "funnel file at point" tailmacs--funnel-file-at-point)])

;; (transient-define-suffix tailmacs--funnel-file (args)
;;   :transient t
;;   (interactive (list (transient-args 'tailmacs-serve)))
;;   (let ((filename (read-file-name "Path: " (concat "/ssh:" "root@" (transient-arg-value "machine=" (transient-args 'tailmacs)) ":~/"))))
;;     (tailmacs--run "tailscale funnel --bg" filename 'transient-args)))
  
(transient-define-suffix tailmacs--funnel-file-at-point (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-serve)))
  (let ((filename (dired-get-filename)))
    (tailmacs--run "tailscale funnel --bg" filename 'transient-args)))
         
(transient-define-suffix tailmacs--funnel-port (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-serve)))
  (tailmacs--shell-command-on-remote-machine
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   (concat "tailscale funnel --bg " (read-string "Port: "))))
  
(transient-define-suffix tailmacs--funnel-reset (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-funnel)))
  (tailmacs--run-command-as-root
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   "tailscale funnel reset")
  (message "Command sent"))

(transient-define-suffix tailmacs--funnel-status (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-funnel)))
  (tailmacs--run-command-as-root
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   "tailscale funnel status"))

;; == API ==

(defun tailmacs--api (endpoint)
  (concat "https://api.tailscale.com/api/v2/tailnet/" tailmacs-organization endpoint))

(defun tailmacs--api-get-devices ()
  (cdr (car (plz 'get (tailmacs--api "/devices")
    :headers (list (cons "Authorization" (format "Bearer %s" tailmacs-access-token)))
    :as #'json-read))))

;;; tailmacs.el ends here



