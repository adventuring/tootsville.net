;;;; -*- lisp -*-
;;;
;;;; src/endpoints/slash-version.lisp is part of Tootsville
;;;
;;;; Copyright  ©   2008-2017  Bruce-Robert  Pocock;  ©   2018-2021  The
;;;; Corporation for Inter-World Tourism and Adventuring (ciwta.org).
;;;
;;;; This  program is  Free  Software: you  can  redistribute it  and/or
;;;; modify it under the terms of  the GNU Affero General Public License
;;;; as published by  the Free Software Foundation; either  version 3 of
;;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the  hope that it will be useful, but
;;; WITHOUT  ANY   WARRANTY;  without  even  the   implied  warranty  of
;;; MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE. See  the GNU
;;; Affero General Public License for more details.
;;;
;;; You should  have received a  copy of  the GNU Affero  General Public
;;; License    along     with    this     program.    If     not,    see
;;; <https://www.gnu.org/licenses/>.
;;;
;;; You can reach CIWTA at https://ciwta.org/, or write to us at:
;;;
;;; PO Box 23095
;;;; Oakland Park, FL 33307-3095
;;; USA

;;;; endpoints /version/*
(in-package :Tootsville)

(defendpoint (:get "/version/about" "application/json")
  "Returns all version information about this host."
  (list 200 nil (version-info-list)))

(defendpoint (:get "/version/about" "text/plain")
  "Returns all version information about this host."
  (list 200 nil (version-info-report-string '(:*))))

(defendpoint (:get "/version/about/detail/:param" "text/plain")
  "Returns (as plain text) the info specified by PARAM.

The values available can be seen by GET /version/about.txt, but
include the following. Values are case-insensitive.

@table @code

@item Product
The product running (eg, Tootsville)
@item Version
The current version of the application
@item Copyright
The copyright notice (one-line form) for the application
@item Environment/Configuration
The environment configuration being run within
@item Environment/Developmentp
True if this is a development server
@item Environment/Productionp
True if this is a production server
@item Machine/Version
The `MACHINE-VERSION' on which this is running.
@item Machine/Type
The `MACHINE-TYPE' on which this is running.
@item Machine/Instance
The `MACHINE-INSTANCE' on which this is running.
@item Site/Short-Name
The short name of the active site.
@item Site/Long-Name
The long name of the active site.
@item Lisp/Type
The `LISP-TYPE' of the compiler used.
@item Lisp/Version
The `LISP-VERSION' of the compiler used.
@item Software/Type
The `SOFTWARE-TYPE' of the operating system.
@item Software/Version
The `SOFTWARE-VERSION' of the operating system.
@item Copyright-Latest
The latest year in the copyright declaration.
@item Build-Date
The date on which the software was first compiled.
@item Compiled
The point in time at which the software was compiled.
@item Request/Name
The requestor name
@item Request/Port
The port on which the request was made
@item Request/Protocol
The protocol via which the request was made
@item Acceptor/Class
The class of the acceptor handling this request
@item Acceptor/Name
The name of the acceptor handling this request
@item Acceptor/Port
The port on which the acceptor is handling this request
@item Acceptor/Address
The address on which the acceptor is handling this request
@end table

"
  (if param
      (list 200 nil
            (version-info-report-string
             (uiop:split-string param :separator ".")))
      (list 400 nil
            "You forgot to ask anything.")))

(defendpoint (:get "/version/about/detail/:param" "application/json")
  "Returns (as a JSON object) the info specified by PARAM.

See the endpoint GET /version/about/detail/:param.txt  for a list  of possible
values of PARAM."
  (list 200 nil
        (list param
              (version-info-report-string
               (uiop:split-string param :separator ".")))))
