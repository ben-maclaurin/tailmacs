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

;; == Cache ==

(defvar tailmacs--devices-cache nil
  "Cached device list from Tailscale API.")

(defvar tailmacs--devices-cache-time nil
  "Time when the device cache was last updated.")

(defcustom tailmacs-cache-ttl 60
  "Time-to-live for device cache in seconds."
  :group 'tailmacs
  :type 'integer)

(defun tailmacs--cache-valid-p ()
  "Return non-nil if the device cache is still valid."
  (and tailmacs--devices-cache
       tailmacs--devices-cache-time
       (< (- (float-time) tailmacs--devices-cache-time) tailmacs-cache-ttl)))

(defun tailmacs-invalidate-cache ()
  "Invalidate all caches, forcing a refresh on next access."
  (interactive)
  (setq tailmacs--devices-cache nil
        tailmacs--devices-cache-time nil
        tailmacs--parsed-names-cache nil
        tailmacs--parsed-names-cache-time nil)
  (message "Tailmacs device cache invalidated"))

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
  :incompatible '(("machine=" "local"))
  ["Target" ("m" "Machine" "machine=" :choices tailmacs--device-names :always-read t) ("l" "Local" "local")]
  ["TRAMP" ("t" "Connect to machine via TRAMP" (lambda () (interactive) (transient-save) (tailmacs--tramp-dispatch)))]
  ["Commands" ("s" "Serve content and local servers on your tailnet" tailmacs-serve) ("f" "Serve content and local servers on the internet" tailmacs-funnel)])
  
(defun tailmacs--tramp-dispatch ()
  (if (transient-arg-value "local" (transient-args transient-current-command))
      (dired default-directory)
    (tailmacs-tramp)))

;; == Utility ==

(defvar tailmacs--parsed-names-cache nil
  "Cached parsed device names (short names and magic DNS domain).")

(defvar tailmacs--parsed-names-cache-time nil
  "Time when parsed names cache was last updated.")

(defun tailmacs--ensure-parsed-names ()
  "Parse device names from cache, computing short names and magic DNS domain once."
  (when (or (null tailmacs--parsed-names-cache)
            (null tailmacs--parsed-names-cache-time)
            (not (equal tailmacs--parsed-names-cache-time tailmacs--devices-cache-time)))
    (let* ((devices (tailmacs--api-get-devices))
           (first-domain (and devices (alist-get 'name (car devices))))
           (domain-parts (and first-domain (cdr (split-string first-domain "\\.")))))
      (setq tailmacs--parsed-names-cache
            (list :short-names (mapcar (lambda (device)
                                         (car (split-string (alist-get 'name device) "\\.")))
                                       devices)
                  :domain-names (mapcar (lambda (device) (alist-get 'name device)) devices)
                  :magic-dns (and domain-parts (string-join domain-parts ".")))
            tailmacs--parsed-names-cache-time tailmacs--devices-cache-time)))
  tailmacs--parsed-names-cache)

(defun tailmacs--device-names ()
  "Get list of short device names."
  (plist-get (tailmacs--ensure-parsed-names) :short-names))

(defun tailmacs--device-domain-names ()
  "Get list of full device domain names."
  (plist-get (tailmacs--ensure-parsed-names) :domain-names))

(defun tailmacs--magic-dns-domain-name ()
  "Get the magic DNS domain name for the tailnet."
  (interactive)
  (plist-get (tailmacs--ensure-parsed-names) :magic-dns))

(defun tailmacs--shell-command-on-remote-machine (remote-machine command)
  (let ((default-directory (expand-file-name (format "/ssh:root@%s:~/" remote-machine))))
    (with-connection-local-variables
     (message "%s" (shell-command-to-string command)))))

(defun tailmacs--format-args (tailscale-args transient-args)
  (let ((current-args (transient-args transient-current-command)))
    (delq nil
          (mapcar (lambda (item)
                    (let* ((key (concat item "="))
                           (value (transient-arg-value key current-args)))
                      (when value
                        (format "%s %s" item value))))
                  tailscale-args))))

(defconst tailscale-args (list "--https" "--http" "--tcp" "--tls-terminated-tcp"))

(defun tailmacs--run (tailscale-command filename transient-args)
  (let ((machine (transient-arg-value "machine=" (transient-args 'tailmacs))))
    (tailmacs--shell-command-on-remote-machine
     machine
     (format "%s %s%s"
             tailscale-command
             (mapconcat #'identity (tailmacs--format-args tailscale-args transient-args) " ")
             (tailmacs--clean-remote-file-path filename machine)))))

(defun tailmacs--clean-remote-file-path (path machine)
  (replace-regexp-in-string (format "/ssh:root@%s:" machine) "" path))

;; == Tramp ==

(transient-define-prefix tailmacs-tramp ()
  "Connect to a tailnet machine via TRAMP."
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
  "Share a local service securely within your tailnet."
  ["Flags"
   ("-https" "Expose an HTTPS server at the specified port." "--https=")
   ("-HTTP" "Expose an HTTP server at the specified port." "--http=")
   ("-tcp" "Expose a TCP forwarder to forward TCP packets at the specified port." "--tcp=")
   ("-tls-terminated-tcp" "Expose a TCP forwarder to forward TLS-terminated TCP packets at the specified port." "--tls-terminated-tcp=")]

  ["Sub-commands" ("S" "status" tailmacs--serve-status) ("r" "reset" tailmacs--serve-reset)]

  ["Commands" ("p" "serve port" tailmacs--serve-port)]
  ["DWIM commands" ("s" "serve file at point" tailmacs--serve-file-at-point :if-mode dired-mode)])

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
  "Share a local service over the internet."
  ["Flags"
   ("-https" "Expose an HTTPS server at the specified port." "--https=")
   ("-HTTP" "Expose an HTTP server at the specified port." "--http=")
   ("-tcp" "Expose a TCP forwarder to forward TCP packets at the specified port." "--tcp=")
   ("-tls-terminated-tcp" "Expose a TCP forwarder to forward TLS-terminated TCP packets at the specified port." "--tls-terminated-tcp=")]

  ["Sub-commands" ("s" "status" tailmacs--funnel-status) ("r" "reset" tailmacs--funnel-reset)]

  ["Commands" ("p" "funnel port" tailmacs--funnel-port)]
  ["DWIM commands" ("f" "funnel file at point" tailmacs--funnel-file-at-point :if-mode dired-mode)])

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
  (tailmacs--shell-command-on-remote-machine
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   "tailscale funnel reset")
  (message "Command sent"))

(transient-define-suffix tailmacs--funnel-status (args)
  :transient t
  (interactive (list (transient-args 'tailmacs-funnel)))
  (tailmacs--shell-command-on-remote-machine
   (transient-arg-value "machine=" (transient-args 'tailmacs))
   "tailscale funnel status"))

;; == API ==

(defun tailmacs--api (endpoint)
  (format "https://api.tailscale.com/api/v2/tailnet/%s%s" tailmacs-organization endpoint))

(defun tailmacs--api-get-devices ()
  "Fetch devices from Tailscale API with caching and error handling."
  (if (tailmacs--cache-valid-p)
      tailmacs--devices-cache
    (condition-case err
        (let ((devices (cdr (car (plz 'get (tailmacs--api "/devices")
                                      :headers `(("Authorization" . ,(format "Bearer %s" tailmacs-access-token)))
                                      :as #'json-read)))))
          (setq tailmacs--devices-cache devices
                tailmacs--devices-cache-time (float-time))
          devices)
      (error
       (message "Tailmacs: API request failed: %s" (error-message-string err))
       (or tailmacs--devices-cache '())))))

;;; tailmacs.el ends here
