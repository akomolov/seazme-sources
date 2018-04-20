(ns seazme.sources.es
  (:require
   [clojure.core.match :refer [match]]
   [seazme.sources.twiki :as t]
   [seazme.sources.confluence :as c]
   [seazme.sources.mbox :as m]
   [seazme.sources.api :as a]
   [clojurewerkz.elastisch.rest :as esr]
   [clojurewerkz.elastisch.rest.index :as esi]
   [clojurewerkz.elastisch.rest.document :as esd])
  (:use seazme.sources.scheduler))

(def db-ini "initial-scan")
(def db-inc "incremental-scans")
(def db-inc-pro "processed")

(defn mk-es-connection[profile & {:keys [debug debug-body] :or {debug false debug-body false}}]
  (esr/connect (profile :host) {:basic-auth (profile :basic-auth) :debug debug :debug-body debug-body}))

(defn mk-conf-api[profile]
  (match [profile]
         [{:url u :api-cookie ac}] (a/mk-conf-api :get u ac 60000)
         [{:url u :basic-auth ba}] (a/mk-conf-api-auth :get u ba 60000)
         :else (throw (java.lang.Exception. (str "bad profile:" profile)))))

(def sna {:type "string" :index "not_analyzed"})
(defn reinit-index![conn [path indx ttype]]
  (let [mapping-types {ttype
                       {:properties
                        {:url sna
                         :instance-name sna
                         :type-name sna
                         :level0 sna
                         :level1 sna
                         :parent-id sna
                         :last-author sna
                         :last-ts sna
                         :text {:type "string" :analyzer "snowball"}
                         :text-size {:type "integer"}
                         }}}]
    [(esi/delete conn indx)
    (esi/create conn indx {:mappings mapping-types})]))

;;TODO can we create two same docs? test it
(defn put-doc! [conn indx ttype doc]
  (esd/put conn indx ttype (:id doc) doc)) ;;TODO can we create two same docs? test it


;;
;; Twiki
;;
(defn twiki-upload-index! [conn [path indx ttype]]
  (->>
   path
   t/find-topics
   (remove nil?)
   (map t/read-topic!)
   (map t/parse-topic)
   (map (partial put-doc! conn indx ttype))
   (map :created)
   frequencies))

;;
;; Email
;;
(defn index-mbox! [indx ttype path conn]
  (->>
   path
   m/process-mbox
   (map m/format-mbox)
   (map (partial put-doc! conn indx ttype))
   (map :created)
   frequencies))

;;
;; Confluence
;;

(defn confluence-upload-cache![api [path]]
  (->> api c/find-spaces (map (partial c/save-space-from-search api (str path "/" db-ini))) doall))

(defn confluence-update-cache![api [path]]
  (c/pull-confl-incr api (str path "/" db-inc)))

(defn confluence-upload-index![conn [path indx ttype base-url instance-name]]
  (->>
   [db-ini (str db-inc "/" db-inc-pro)] ;;order is essential for below sort to work
   (map (partial str path "/"))
   (map c/find-pages)
   (mapcat sort) ;;sort to make sure we update from oldest to newest (only for incremental scan) - never remove it
   (map (partial c/read-page! base-url instance-name))
   (map c/parse-page)
   (map (partial put-doc! conn indx ttype))
   (map :created)
   frequencies))

(defn confluence-update-index! [conn [path indx ttype base-url instance-name]]
  #_{:file "2017-04-10_15:50", :path+file "db/ppconf/incremental-scans/2017-04-10_15:50"}
  (letfn [(update-fn [args]
            (println "incremental indexing " (:file args))
            (->>
             args
             :path+file
             c/find-pages
             (map (partial c/read-page! base-url instance-name))
             (map c/parse-page)
             (map (partial put-doc! conn indx ttype))
             (map :created)
             frequencies))]
    (invoke-and-backup! (str path "/" db-inc) db-inc-pro update-fn)))

;;TODO
;; document junk-in garbage-out
;; run by cron every 15 minutes
;; do profiles vs individual settings
;; ERROR handling, confluence is down, session expires
;; make all pure functions, use ! if not possible