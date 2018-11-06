(ns seazme.sources.maintanance)

(comment
  "delete bad prefix"
  (def prefix "badprefix")
  (def to-del (->> (hb/scan* "datahub:snapshot-update-log" :starts-with prefix :lazy? true) (map (comp name first))))
  (->> to-del (map (partial delete (hb/get-conn) "datahub:snapshot-update-log")) dorun)
  )

(comment
  "delete bad indices"
  (->> (esi/get-settings e-p-s) keys (map name) (map (partial esi/delete e-p-s)) doall frequencies)
  )

(comment
  "find jira sessions"
  (def sessions (find-sessions nil))
  (def jira-sessions (->> sessions (filter (comp (partial = "jira") :kind :app second))))
  (defn xx[e] [(->> e :meta :key) #_(->> e :meta :id) (->> e :meta :created d) (->> e :range vals (map d)) (:count e)])
  (defn d[x] (str (java.util.Date. x)))
  (binding [clojure.pprint/*print-right-margin* 302] (->> jira-sessions (take-last 60) (map (comp xx :self second)) pprint))
  )

(comment
  "cancel sessions"
  (defn cancel[skey]
    (let [sself (hb/find-by* "datahub:intake-sessions" skey :self)]
      (hb/store* "datahub:intake-sessions" skey :self
                 (assoc-in (assoc-in sself
                                     [:meta :action] "cancel")
                           [:description] "the session was manually canceled after initial submit due to JIRA API issue"))))
  )

(comment
  "finding unique nil fields in JIRA"
  (require '[clj-time.format :as tf] '[clj-time.coerce :as tr])
  (use 'seazme.common.common 'seazme.common.datahub :reload)
  (def sessions (find-sessions nil))
  (def jira-sessions (->> sessions (map second) (filter (comp (partial = "jira") :kind :app))))
  (def jira-sessions-ids (->> jira-sessions (map (comp :id :meta :self))))
  (def recent-jira-sessions (->> jira-sessions (take-last 20) (map (comp :id :meta :self))))
  (def session-id (->> recent-jira-sessions last))

  (def jira-ts-formatter (tf/formatters :date-time))
  (def yymm-formatter (tf/formatter "YYMM"))
  (defn fmap [f m] (into (empty m) (for [[k v] m] [k (f v)])))
  (defn i4[x y] (assoc x (first y) ((partial merge-with +) (get x (first y) {}) (second y))))
  (defn fields4[ticket] (let [fields (->> ticket second :self :payload :fields)] [[(-> fields :project :key) (-> fields :issuetype :name) (->> fields :updated #_:created (tf/parse jira-ts-formatter) (tf/unparse yymm-formatter))] (fmap (comp {true 0 false 1} nil?) fields)]))
  (defn filter-zero-fileds[m] (->> m (remove #(->> % second zero?)) (into {})))
  (defn map-to-set[m] (->> m (map first) set))
  (defn nil-fields4a[session-id] (->> dist-bytes (map #(->> (get-data-entries-seq session-id %) (map fields4))) (apply concat) (reduce i4 {}) (fmap filter-zero-fileds)))
  (defn nil-fields4b[session-id] (->> dist-bytes (map #(->> (get-data-entries-seq session-id %) (map fields4) (fmap filter-zero-fileds))) (apply concat) (reduce i4 {})))
  (defn nil-tmp[session-id] (->> (get-data-entries-seq session-id "00") (map fields4) (reduce i4 {}) (fmap filter-zero-fileds)))

  (def t (nil-fields4 session-id))
  (def common-fields (apply clojure.set/intersection (->> t (fmap map-to-set) vals)))
  (def t2 (->> t (fmap #(apply dissoc % common-fields))))
  (def tt (->> recent-jira-sessions (take 3) (map nil-fields4) (appy merge))) ;;TODO add +
  (def common-fields (apply clojure.set/intersection (->> tt (fmap map-to-set) vals)))
  (def tt2 (->> tt (fmap #(apply dissoc % common-fields))))

  (->> tr (apply concat) (reduce i4 {}) (fmap filter-zero-fileds) (fmap #(apply dissoc % common-fields)) count)

  ;;test
  (def x [{["PRPOIM" "Task" "1809"] {:customfield_10778 0, :customfield_10779 1} ["PRPOIM" "Task" "1810"] {:customfield_10778 0, :customfield_10779 1}}   {["PRPOIM" "Task" "1809"] {:customfield_10778 0, :customfield_10779 1} ["PRPOIM" "Task" "1810"] {:customfield_10778 0, :customfield_10779 1}}])
  (reduce i4 {} (apply concat x))
  {["PRPOIM" "Task" "1809"] {:customfield_10778 0, :customfield_10779 2}, ["PRPOIM" "Task" "1810"] {:customfield_10778 0, :customfield_10779 2}}
  )

(comment
  "find tickets posted in many sessions"
  (defn fmap [f m] (into (empty m) (for [[k v] m] [k (f v)])))
  (def sessions (find-sessions nil))
  (def recent-sessions (->> sessions (map second) (filter (comp (partial = "jira") :kind :app)) (take-last 20) (map (comp :id :meta :self))))
  (defn keys-per-session[sid] (->> dist-bytes (pmap (partial get-data-entries-seq sid)) (mapcat identity) (map #(->> % second :self :payload :key))))

  ;;check :created for a break away timestamp
  (->> sessions (map second) (filter (comp (partial = "jira") :kind :app)) (take-last 20) (map :self) (map-indexed vector) pprint)

  (def x (map keys-per-session recent-sessions))
  (def y (map-indexed #(map (fn[k] [k %1]) %2) x))
  (def z (mapcat identity y))

  ;; pick sessions with index lower than break away
  (->> z (group-by first) (fmap #(map second %)) sort pprint)
  (pprint (map-indexed vector recent-sessions))
  )

(comment
  "verify JIRA API, there should never be new tickets when looking up for certain updated time frame in future"
  (defn fmap [f m] (into (empty m) (for [[k v] m] [k (f v)])))
  (def sessions (find-sessions nil))
  (def recent-sessions (->> sessions (map second) (filter (comp (partial = "jira") :kind :app)) (take-last 2)))
  (defn essential-per-session[session]
    (let[sid (-> session :self :meta :id)
         {:keys [from to]} (-> session :self :range)
         essef #(vector (->> % second :self :payload :key) (->> % second :self :payload :fields :updated))]
      (vector sid from to (->> dist-bytes (pmap (partial get-data-entries-seq sid)) (mapcat identity) (map essef)))))

  (def x (map essential-per-session recent-sessions))
  (def y (map-indexed #(map (fn[k] [k %1]) %2) x))
  (def z (mapcat identity y))
  )

(comment
  "verify JIRA API, there should never be new tickets when looking up for certain updated time frame in future"

  (def sessions (find-sessions nil))
  (def jira-sessions (->> sessions (filter (comp (partial = "jira") :kind :app second))))
  (def small-jira-sessions (->> jira-sessions (filter (comp (partial > 200) :count :self second))))
  (def recent-jira-sessions (->> jira-sessions (take-last 5)))
  (defn jiratuple[e] [(-> e :key) (-> e :fields :updated)])
  (defn jiras-per-sid[sid] (->> dist-bytes (pmap (partial get-data-entries-seq sid)) (mapcat identity) (map (comp jiratuple :payload :self second))))
  (defn jiras-per-sid-filtered[filt sid] (->> dist-bytes (pmap (partial get-data-entries-seq sid)) (mapcat identity) (map (comp :payload :self second)) (filter #(contains? filt (% :key))) set))
  (defn essential-per-session[session]
    (let[sid (-> session :self :meta :id)
         cnt (-> session :self :count)
         {:keys [from to]} (-> session :self :range)]
      (vector sid [from to] cnt (jiras-per-sid sid))))

  (defn test-pull[pja-search-api e]
    (let [[sid rnge cnt details] e
          in-datahub (set details)
          in-jira (set (jira/period-search pja-search-api rnge jiratuple))
          issub  (clojure.set/subset? in-jira in-datahub)]
      (prn "result" sid rnge issub (count in-jira) (count in-datahub) (clojure.set/difference in-jira in-datahub))
      [sid rnge in-jira in-datahub]
      ))

  (defn test-diff[e]
    (let [[sid rnge in-jira in-datahub] e]
      (prn "result" sid rnge (clojure.set/subset? in-jira in-datahub) (count in-jira) (count in-datahub) (clojure.set/difference in-jira in-datahub))))


  (def z1 (map (comp essential-per-session second) recent-jira-sessions))
  (def pja-search-api (jira-api/mk-pja-search-api (-> config/config :jira-pp-prod :url) (-> config/config :jira-pp-prod :credentials) false))
  (def z2 (->> z1 (map (partial test-pull pja-search-api)) doall))
  (->> z2 (map test-diff) dorun)
  )

(comment
  "misc"
  (->> sessions (map second) (map :self) (map #(quot (- (-> % :meta :created)  (Long/parseLong (-> % :meta :tsx) 16)) (:count %) )) pprint)
  )

(comment
  "reading cache files"

  (defn scan-file [cb file-path]
    (with-open [in (-> file-path clojure.java.io/input-stream java.util.zip.GZIPInputStream. clojure.java.io/reader java.io.PushbackReader.)]
      (let[edn-seq (repeatedly (partial clojure.edn/read {:eof nil} in))]
        (doall (map cb (take-while (partial not= nil) edn-seq))))))
  (def ese #(vector (-> % :key) (-> % :fields :updated) (-> % :fields :status :name)))

  ;; single
  (def files (->> "<your-dir>" clojure.java.io/file file-seq (filter #(.isFile %)) sort)) ;;TODO sort by file name
  (->> files first (scan-file ese) (sort-by second))

  ;;or diff
  (def f1 (->> "<dir1>" clojure.java.io/file file-seq (filter #(.isFile %)) sort))
  (def f2 (->> "<dir2>" clojure.java.io/file file-seq (filter #(.isFile %)) sort))
  (def r2 (->> f2 (mapcat #(scan-file ese %)) set))
  (def r1 (->> f1 (mapcat #(scan-file ese %)) set))
  (pprint (clojure.set/difference r2 r1))
  )
